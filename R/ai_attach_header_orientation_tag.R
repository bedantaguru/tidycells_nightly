

ai_attach_header_orientation_tag <- function(admap_cellwise_raw) {
  
  # asp: attr split
  admap_cellwise_raw_asp <- ai_attr_gid_micro_splits(admap_cellwise_raw)
  
  do_hierarchical_reallocation <- !isTRUE(getOption("tidycells.analyze_cells_options")[["no_hierarchical_reallocation"]])
  
  if(do_hierarchical_reallocation){
    admap_cellwise_raw_asp <- ai_hierarchical_reallocation(admap_cellwise_raw_asp)
    admap_cellwise_raw_asp <- compact_attr_micro_gid_maps(admap_cellwise_raw_asp)
    admap_cellwise_raw_asp <- ai_attr_var_sync_name_fix(admap_cellwise_raw_asp)
  }
  
  admap_cellwise_raw_asp <- distinct(admap_cellwise_raw_asp)
  
  # remove n_rc later
  admap_cellwise_raw_asp <- admap_cellwise_raw_asp %>% 
    group_by(attr_micro_gid) %>% 
    mutate(n_rc = n_distinct(row_a, col_a)) %>% 
    ungroup()
  
  out1 <- admap_cellwise_raw_asp %>% 
    filter(n_rc == 1) %>% 
    mutate(header_orientation_tag = "direct")
  
  out2_part1 <- admap_cellwise_raw_asp %>% 
    filter(n_rc != 1) 
  
  out2_part2 <- ai_get_dimention_analysis_details(
    admap_cellwise_raw_asp = out2_part1, 
    raw_mode = T, 
    major_direction_relax = F)
  
  out2 <- get_header_orientation_tag(out2_part2)
  
  out <- out1 %>% bind_rows(out2)
  
  out <- out %>% 
    select(-n_rc) %>% 
    mutate(attr_micro_gid_is_full_dim = ifelse(
      is.na(attr_micro_gid_is_full_dim), FALSE, attr_micro_gid_is_full_dim))
  
  out
  
}

# helpers
ai_attr_gid_micro_splits <- function(admap_cellwise_raw){
  admap_cellwise_raw <- distinct(admap_cellwise_raw)
  # asp: attr split
  admap_cellwise_raw_asp <- admap_cellwise_raw %>%
    mutate(attr_gid_split = 
             ifelse(direction_group == "NS", paste0(row_a,":0"),
                    ifelse(direction_group == "WE", paste0("0:", col_a), 
                           ifelse(direction_group == "corner", paste0(row_a, ":", col_a), 0))
             )) %>% 
    mutate(attr_micro_gid = paste(attr_gid, direction, attr_gid_split, sep = "_"))
  
  # fix for potential corner micro cases
  # when both sided attr split happened earlier
  
  #######
  ####### attachment of corner mod start
  
  corner_micro <- tidycells_pkg_env$common_knowledge$corner_micro
  
  if(is.data.frame(corner_micro)){
    if(nrow(corner_micro)>0){
      admap_cellwise_raw_asp <- corner_micro %>% 
        distinct(row_d, col_d, row_a, col_a, corner_mod = direction_group) %>% 
        right_join(admap_cellwise_raw_asp, by = c("row_d", "col_d", "row_a", "col_a"))
      admap_cellwise_raw_asp <- admap_cellwise_raw_asp %>% 
        mutate(corner_mod = ifelse(is.na(corner_mod),"", corner_mod))
      admap_cellwise_raw_asp <- admap_cellwise_raw_asp %>% 
        group_by(attr_micro_gid) %>% 
        mutate(corner_mod = stat_mode(corner_mod)) %>% 
        ungroup()
    }
  }
  
  if(!hasName(admap_cellwise_raw_asp,"corner_mod")){
    admap_cellwise_raw_asp$corner_mod <- ""
  }
  
  admap_cellwise_raw_asp$corner_mod <- stringr::str_replace(admap_cellwise_raw_asp$corner_mod,"corner_","")
  
  ##### attachment of corner mod end
  #####
  
  ai_get_sync_names_for_attr_gid_splits(admap_cellwise_raw_asp)
  
}



#@Dev write doc
# this assigns a name suitable for staking attributes across data blocks in later stage
ai_get_sync_names_for_attr_gid_splits <- function(admap_cellwise_raw){
  # @Dev
  # this can be avoided in case of single data block
  
  dg <- admap_cellwise_raw %>% distinct(gid = data_gid, row = row_d, col = col_d)
  ag <- admap_cellwise_raw %>% distinct(attr_micro_gid, row = row_a, col = col_a, data_gid)
  
  dgb <- get_group_id_boundary(dg)
  ag <- ag %>% left_join(dgb, by = c("data_gid"="gid"))
  dag <- get_direction_df_nn(ag)
  
  ## this is potential fix for both sided header split case
  corner_micro <- admap_cellwise_raw %>% distinct(attr_micro_gid, data_gid, corner_mod)
  dag <- dag %>% inner_join(corner_micro, by = c("attr_micro_gid", "data_gid"))
  dag <- dag %>% 
    mutate(direction = ifelse(
      nchar(corner_mod)>0,
      paste0(direction, "_",corner_mod),
      direction
    ))
  
  dagmap <- dag %>% 
    group_by(attr_micro_gid, data_gid, direction) %>% 
    mutate(nc_dir = n()) %>% 
    group_by(attr_micro_gid, data_gid) %>% 
    filter(dist == min(dist)) %>% 
    filter(nc_dir == max(nc_dir)) %>% 
    summarise(dist = dist[1], direction = direction[1]) %>% 
    ungroup()
  
  sync_name_map <- dagmap %>% 
    group_by(direction, data_gid) %>% 
    mutate(dist_ord = dist %>% as.factor() %>% as.numeric(),
           attr_var_sync_name = paste0(direction, "_",dist_ord)) %>% 
    ungroup() %>% 
    distinct(attr_micro_gid, data_gid, dist_order = dist_ord, attr_var_sync_name)
  
  new_map <- admap_cellwise_raw %>% left_join(sync_name_map, by = c("data_gid", "attr_micro_gid"))
  
  ##############
  ### checks ###
  ##############
  
  new_map <- ai_attr_var_sync_name_fix(new_map)
  
  #######
  
  new_map
}


ai_attr_var_sync_name_fix <- function(admap_cellwise){
  # fix the names at global level
  # each attr_micro_gid should get a single attr_var_sync_name
  chk <- admap_cellwise %>% distinct(attr_micro_gid, attr_var_sync_name)
  issue_attr_micro_gid <- 
    chk %>% 
    group_by(attr_micro_gid) %>% 
    count() %>% 
    filter(n>1) %>% 
    pull(attr_micro_gid)
  
  if(length(issue_attr_micro_gid)>0){
    fix <- chk %>% 
      filter(attr_micro_gid %in% issue_attr_micro_gid) %>% 
      group_by(attr_micro_gid) %>% 
      # attached G_ to distinguise from other locally handled names
      summarise(new_attr_var_sync_name = paste0("G_",max(attr_var_sync_name)))
    
    admap_cellwise <- fix_attr_var_sync_name_admap(admap_cellwise, fix)
    
  }
  
  # within each data_gid each attr_var_sync_name should be assigned to only one attr_micro_gid
  # possibly this already gets ensured by previous processes
  chkd <- admap_cellwise %>% 
    distinct(data_gid, attr_micro_gid, attr_var_sync_name) %>% 
    group_by(data_gid, attr_var_sync_name) %>% 
    mutate(n_amg = n()) %>% 
    ungroup() %>% 
    filter(n_amg>1)
  
  if(nrow(chkd)>0){
    fix <- chkd %>% 
      group_by(data_gid, attr_var_sync_name) %>% 
      mutate(new_attr_var_sync_name = paste0(attr_var_sync_name,"_", seq_along(attr_micro_gid))) %>% 
      ungroup() %>% 
      distinct(attr_micro_gid, new_attr_var_sync_name)
    
    admap_cellwise <- fix_attr_var_sync_name_admap(admap_cellwise, fix)
    
  }
  admap_cellwise
}


fix_attr_var_sync_name_admap <- function(new_map, fix){
  new_map <- new_map %>% left_join(fix, by = "attr_micro_gid")
  new_map <- new_map %>% 
    mutate(attr_var_sync_name = 
             ifelse(is.na(new_attr_var_sync_name), 
                    attr_var_sync_name, new_attr_var_sync_name)) %>% 
    select(-new_attr_var_sync_name)
  new_map
}
