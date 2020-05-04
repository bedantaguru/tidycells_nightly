
# phase 0
# - validation
# - pre processing
ai_main_part_phase_0_pre_process <- function(d){
  
  if (!is_cell_df(d)) {
    abort("A Cell DF expected")
  }
  
  if (!hasName(d, "type")) {
    abort(paste("The type column not found.",
                "(You may like to do 'Value Attribute Classification'.",
                "Check basic_classifier, sample_based_classifier, numeric_values_classifier for details.",
                sep = "\n"
    ))
  }
  
  val <- validate_cells(d)
  
  if (!val) {
    abort(attr(val, "msg") %>% paste0(collapse = "\n"))
  }
  
  #  remove empty cells
  d <- d %>% filter(type != "empty")
  
  # the term 'data' and 'value' are interchangeably used going forward
  data_cells <- d %>%
    filter(type == "value") %>%
    as_rc_df()
  
  attr_cells <- d %>%
    filter(type == "attribute") %>%
    as_rc_df()
  
  if (nrow(data_cells) == 0) {
    abort("No `value` cells found")
  }
  
  if (nrow(attr_cells) == 0) {
    abort("No `attribute` cells found")
  }
  
  # clean common knowledge
  common_knowledge(clean = T)
  
  d_dat <- get_group_id(data_cells, gid_tag = "d")
  d_att <- get_group_id(attr_cells, gid_tag = "a")
  
  list(d_dat = d_dat, d_att = d_att, d = d)
}

# phase 1
# - basic_map
# - relative_data_split_attr
# - dimention_analysis
ai_main_part_phase_1_admap <- function(d_dat, d_att) {

  d_dat_bd <- get_group_id_boundary(d_dat)
  #  start with simple attr data map
  admap0 <- ai_get_data_attr_map(
    dat_boundary = d_dat_bd,
    att_gid_map = d_att
  )


  # split attr gid relative to data_gid
  rel_chk <- ai_relative_data_split_attr(basic_map = admap0, d_att = d_att, d_dat_bd = d_dat_bd)
  if (rel_chk$done) {
    d_att <- rel_chk$d_att
    admap0 <- rel_chk$admap
    
    # following step is required for: 
    # diffrent split resulted in same attr_gid
    # these will be compacted to single group
    cmp <- compact_attr_gid_maps(d_att, admap0)
    
    if(cmp$done){
      d_att <- cmp$d_att
      admap0 <- cmp$admap
    }
    
  }

  # dimension analysis done here (major minor classification) 
  # phase 1
  admap1_major_minor <- admap0$all_map %>%
    filter(mapping_strength>0 | direction_group != "corner") %>%
    ai_get_dimention_analysis_details(d_dat, d_att)

  # as we started with all_map (yet near data to attr map is not done)
  # this is equivalent to admap0$map
  admap_out <- admap1_major_minor$map %>%
    filter(mapping_strength>0 | attr_group != "minor") %>%
    group_by(data_gid, direction_group) %>%
    filter(dist == min(dist)) %>%
    ungroup()

  
  # here the potential corner_WE and corner_NS needs to be changed back
  # before that 
  # @Dev figure out best way to transfer this info
  corner_micro <- admap_out %>% 
    filter(direction_group=="corner_WE" | direction_group=="corner_NS") %>% 
    get_data_attr_cell_wise_map_raw(d_dat, d_att)
  
  common_knowledge(corner_micro = corner_micro)
  
  admap_out <- admap_out %>% 
    mutate(
      direction_group =ifelse(
        stringr::str_detect(direction_group, "corner"), 
        "corner", 
        direction_group)
    )
  ##########################
  
  
  list(admap = admap_out, d_att = d_att)
}


# phase 2
# - possible data gid joins
# - possible attr gid joins
ai_main_part_phase_2_gid_joins <- function(d_dat, d_att, admap, d){
  
  # data_gid join (if possible)
  if (length(unique(d_dat$gid)) > 1) {
    
    d_dat_potential_joins <- ai_data_gid_join(
      d_dat, d_att, 
      data_attr_map = admap,
      full_data = d %>% as_tibble()
    )
    
    if (d_dat_potential_joins$done) {
      
      # data gids are joined
      d_dat <- d_dat_potential_joins$d_dat
      admap <- d_dat_potential_joins$admap
      
      # time to update d_att (join attributes induced by data_gid joins)
      # This will be taken care by next block
      
    }
  }
  
  # join attr based on block merges (potentially possible)
  rel_chk <- ai_relative_data_join_attr(admap_main = admap, d_att = d_att)
  if (rel_chk$done) {
    d_att <- rel_chk$d_att
    admap <- rel_chk$admap
  }
  
  cmp <- compact_attr_gid_maps(d_att, admap)
  
  if(cmp$done){
    d_att <- cmp$d_att
    admap <- cmp$admap
  }
  
  
  list(admap = admap, d_dat = d_dat, d_att = d_att)
  
}


# phase 3
# - information block
# - minor and rest attribute mapping
ai_main_part_phase_3_rest_map <- function(d_dat, d_att, admap){
  
  # time for corners (potential) / minor / less strong mappings
  # information blocks are introduced from here
  
  info_block <- ai_get_information_blocks(admap, d_dat, d_att)
  
  admap <- info_block$map
  d_inf <- info_block$d_inf
  
  
  
  unmapped_attr_gids <- d_att$gid %>% unique() %>% 
    setdiff(admap$attr_gid) %>% 
    # @Dev
    # following may not be perfect as it discards the <missed_block_connections> $ <attr_gid> without seeeing
    # <data_gid> <- this is stale now (for this we may opt for <row, col> sample in <missed_block_connections>)
    # for now solving purpose but may be dropped later
    # 
    setdiff(common_knowledge("missed_block_connections")$attr_gid)
  
  admap_fr1 <- NULL
  
  if(length(unmapped_attr_gids)>0){
    
    # attach unmapped_attr_gids to info_blocks
    
    # fr: for rest attr_ids (this actually mean <unmapped_attr_gids> and can include corners and non-corners also)
    admap_fr0 <- ai_get_data_attr_map(
      dat_boundary = get_group_id_boundary(d_inf),
      att_gid_map = d_att %>% filter(gid %in% unmapped_attr_gids),
      attr_to_near_data = TRUE,
      leave_inside = TRUE
    )
    
    admap_fr1 <- admap_fr0$map %>%
      ai_get_dimention_analysis_details(d_inf, d_att, major_direction_relax = FALSE)
    
    admap_fr1 <- admap_fr1$map %>% 
      # since data_gid is dummy name for info_gid 
      rename(info_gid = data_gid) %>% 
      # data_gid has to be added to make it comparable to admap
      inner_join(admap %>% distinct(data_gid, info_gid), by = "info_gid")
    
    #@Dev new addition
    admap_fr1 <- admap_for_rest_dir_fix(d_dat, d_att, admap_fr1)
    
    unmapped_attr_gids <-
      admap$attr_gid %>%
      c(admap_fr0$map$attr_gid) %>%
      setdiff(d_att$gid, .) %>%
      #@Dev
      # this is suffering from same problem mentioned earlier
      setdiff(common_knowledge("missed_block_connections")$attr_gid)
    
  }
  
  # merge two maps
  admap <- admap %>% bind_rows(admap_fr1)
  
  cmp <- compact_attr_gid_maps(d_att, admap)
  
  d_att <- cmp$d_att
  admap <- cmp$admap
  
  list(admap = admap, unmapped_attr_gids = unmapped_attr_gids)
  
}



# phase 4
# - get direction or header orientation (ho) tag (HOT)
# - sync name etc.
ai_main_part_phase_4_header_orientation <- function(d_dat, d_att, admap){
  # attach directions to it
  admap_with_dir <- get_data_attr_cell_wise_map_raw(admap, d_dat, d_att) %>% 
    ai_attach_header_orientation_tag()
  
  list(admap_cell_wise = admap_with_dir)
}


# phase 5
# - post-processing
ai_main_part_phase_5_post_process <- function(d_dat, d_att, admap, admap_cell_wise, d_orig){
  
  # @Dev
  # this may not be accurate 
  # one cell can be mapped in multiple manner
  # str-detection done
  this_cells <- get_cells_from_admap(admap, d_dat, d_att)
  
  # natural gid for easier user selection
  # non-seqential variant: 
  # gid_ngid <- d_dat %>%
  #   distinct(gid) %>%
  #   mutate(natural_gid = gid %>% as.factor() %>% as.numeric() %>% paste0("d",.)) 
  
  gid_ngid <- d_dat %>% 
    group_by(gid) %>% 
    summarise(row = round(mean(row)), col = round(mean(col))) %>% 
    arrange(col, row) %>% 
    mutate(natural_gid = paste0("d",format(seq_along(gid), justify = "left")) %>% 
             stringr::str_replace_all(" ","0")) %>% 
    select(gid, natural_gid)
  
  
  # attach natural gid
  this_cells <- this_cells %>%
    left_join(gid_ngid, by = "gid")
  
  # this need to be added after ai_attach_header_orientation_tag
  admap_cell_wise <- admap_cell_wise %>%
    left_join(gid_ngid, by = c("data_gid"="gid"))
  
  # @Dev
  #  issues to be covered differently or later
  # df_details <- get_definiteness_details(admap$raw_map,
  #   all_attr_gids = d_att$group_id_boundary$gid %>%
  #     setdiff(d_att$missed_blocks$gid)
  # )
  # definiteness_checks <- get_definiteness_checks(df_details, silent = silent)
  # @Dev
  # silent in main function is not used
  
  obj <- list(
    cells = this_cells,
    sections = get_group_id_boundary(d_dat) %>% left_join(gid_ngid, by = "gid"),
    details = list(
      attr_details = d_att,
      data_details = d_dat,
      data_attr_map_raw = admap_cell_wise,
      definiteness_checks = NULL
    ),
    cell_df = d_orig
  )
  
  # attach cell_df_analysis class
  class(obj) <- cell_df_analysis_class
  
  # clean common knowledge
  common_knowledge(clean = T)
  
  obj
}
