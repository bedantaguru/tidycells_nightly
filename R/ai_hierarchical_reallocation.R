
# @Dev
# need to check situation in both side layered headers
# also document

# full name should be 
# ai_attr_micro_gid_hierarchical_reallocation
ai_hierarchical_reallocation <- function(admap_cellwise_raw_asp){
  
  admap_cellwise_raw_asp <- compact_attr_micro_gid_maps(admap_cellwise_raw_asp)
  # take minor and corner micro gids
  minor_attr_micro_gid <- admap_cellwise_raw_asp %>% 
    filter(attr_group=="minor" | direction_group == "corner", dist_order>1) %>% 
    distinct(info_gid, data_gid, attr_micro_gid, dist_order, direction, direction_group)
  
  # leave attr_micro_gid within each info block which 
  #  has connectivity to all data_gid in that info block
  info_block_wise_data_block_count <- admap_cellwise_raw_asp %>% 
    group_by(info_gid) %>% 
    summarise(ndb = n_distinct(data_gid))
  
  minor_attr_micro_gid <- minor_attr_micro_gid %>% 
    left_join(info_block_wise_data_block_count, by = "info_gid")
  
  minor_attr_micro_gid <- minor_attr_micro_gid %>% 
    group_by(info_gid, attr_micro_gid) %>% 
    mutate(connected_to_db_frac = n_distinct(data_gid)/ndb[1]) %>% 
    ungroup()
  
  layered_attr_micro_gid <- minor_attr_micro_gid %>% 
    filter(connected_to_db_frac<1)
  
  if(nrow(layered_attr_micro_gid)==0){
    # nothing required to be done
    return(admap_cellwise_raw_asp)
  }
  
  dgids <- admap_cellwise_raw_asp %>% 
    distinct(info_gid, data_gid, row_d, col_d)
  
  prop_map <- adjustment_in_HR_for_prior_dual_attr_split(
    layered_attr_micro_gid, 
    admap_cellwise_raw_asp, 
    dgids
  )
  
  admap_cellwise_raw_asp_rest <- admap_cellwise_raw_asp %>% 
    anti_join(prop_map, by = c("data_gid", "info_gid", "attr_micro_gid")) %>% 
    anti_join(layered_attr_micro_gid, by = c("data_gid", "info_gid", "attr_micro_gid"))
  
  op2 <- admap_cellwise_raw_asp %>% 
    select(-data_gid, -row_d, -col_d) %>% 
    distinct() %>% 
    filter(attr_micro_gid %in% prop_map$attr_micro_gid) %>% 
    select(-attr_var_sync_name)
  
  op_sync_nm <- admap_cellwise_raw_asp %>% 
    distinct(info_gid, data_gid, attr_micro_gid, attr_var_sync_name) %>% 
    right_join(prop_map, by = c("info_gid", "data_gid", "attr_micro_gid"))
  op_sync_nm <- op_sync_nm %>% 
    mutate(l_ord = dist_order %>% as.factor() %>% as.numeric()) %>% 
    mutate(new_attr_var_sync_name = max(attr_var_sync_name, na.rm = T) %>% 
             paste0("L",l_ord,"_", .))
  op_sync_nm <- op_sync_nm %>% 
    distinct(info_gid, data_gid, 
             attr_micro_gid, attr_var_sync_name = new_attr_var_sync_name)
  
  op1 <- dgids %>% left_join(op_sync_nm, by = c("info_gid", "data_gid"))
  
  admap_cellwise_raw_asp_this <- op1 %>% 
    inner_join(op2, by = c("info_gid", "attr_micro_gid"))
  
  admap_cellwise_raw_asp_rest %>% 
    bind_rows(admap_cellwise_raw_asp_this)
  
}

# helpers

# HR : hierarchical_reallocation
# layered_attr_micro_gid (for a layer):  lamg
ai_attr_micro_gid_HR_for_a_layer <- function(lamg, dgids){
  lamg %>% 
    split(.$info_gid) %>% 
    map_df(~ai_attr_micro_gid_HR_for_a_layer_inside_a_info_block(.x, dgids))
}

ai_attr_micro_gid_HR_for_a_layer_inside_a_info_block <- function(lamg, dgids){
  dgids <- dgids %>% filter(info_gid == lamg$info_gid[1])
  prop1 <- unique(lamg$attr_micro_gid) %>% 
    map_df(~claim_region_for_attr_micro_gid_HR(.x, lamg, dgids))
  # check of dual map for a data_gid
  chk <- prop1 %>% 
    filter(is_expanded) %>% 
    group_by(data_gid) %>% 
    mutate(n_amg = n_distinct(attr_micro_gid)) %>% 
    ungroup() %>% 
    filter(n_amg>1)
  
  if(nrow(chk)>0){
    # nothing yet to be done here
    msg_once(
      "In hierarchical reallocation it is observed that both sided hierarchical header may be present"
    )
  }
  prop1
}

adjustment_in_HR_for_prior_dual_attr_split <- function(
  layered_attr_micro_gid, 
  admap_cellwise_raw_asp,
  dgids
){
  
  if(missing(dgids)){
    dgids <- admap_cellwise_raw_asp %>% 
      distinct(info_gid, data_gid, row_d, col_d)
  }
  
  admap_cellwise_raw_asp_this <- admap_cellwise_raw_asp %>% 
    inner_join(layered_attr_micro_gid %>% 
                 distinct(data_gid, info_gid, attr_micro_gid), 
               by = c("data_gid", "info_gid", "attr_micro_gid"))
  
  do_adj <- F
  # aopt : attr_ori_pre_tag
  aopt <- tidycells_pkg_env$common_knowledge$attr_ori_pre_tag
  if(!is.null(aopt)){
    if(is.data.frame(aopt)){
      if(nrow(aopt)>0){
        # check if any common attr_cell present
        chk <- admap_cellwise_raw_asp_this %>% inner_join(aopt, by = c("row_a", "col_a"))
        if(nrow(chk)>0){
          do_adj <- T
        }
      }
    }
  }
  
  if(do_adj){
    # perform adjustment for both sided splits
    aopt <- aopt %>% rename(initial_attr_gid = attr_gid)
    
    dgids_normal <- dgids %>% distinct(gid = data_gid, row = row_d, col = col_d)
    
    admap_cellwise_raw_asp_this <- 
      admap_cellwise_raw_asp_this %>% 
      left_join(aopt, by = c("row_a", "col_a"))
    
    admap_cellwise_raw_asp_this <- admap_cellwise_raw_asp_this %>% 
      mutate(ori_pre_tag = ifelse(is.na(ori_pre_tag), "Dummy", ori_pre_tag),
             initial_attr_gid = ifelse(is.na(initial_attr_gid), "DummyAttrGrp", initial_attr_gid))
    
    layered_attr_micro_gid_mod <- admap_cellwise_raw_asp_this %>%  
      distinct(info_gid, data_gid, attr_micro_gid, dist_order, ori_pre_tag, initial_attr_gid, direction, direction_group)
    
    layered_attr_micro_gid_mod_NS <- layered_attr_micro_gid_mod %>% 
      filter(ori_pre_tag=="NS")
    layered_attr_micro_gid_mod_WE <- layered_attr_micro_gid_mod %>% 
      filter(ori_pre_tag=="WE")
    layered_attr_micro_gid_mod_rest <- layered_attr_micro_gid_mod %>% 
      filter(!(ori_pre_tag %in% c("NS","WE")))
    prop_map_NS <- prop_map_WE <- prop_map_rest <- NULL
    
    if(nrow(layered_attr_micro_gid_mod_NS)>0){
      
      prop_map_NS <- attr_split_by_encl_and_map_back(
        layered_attr_micro_gid_mod_NS, enclosure_direction = "col", dgids_normal, dgids)
    }
    
    if(nrow(layered_attr_micro_gid_mod_WE)>0){
      
      prop_map_WE <- attr_split_by_encl_and_map_back(
        layered_attr_micro_gid_mod_WE, enclosure_direction = "row", dgids_normal, dgids)
    }
    
    if(nrow(layered_attr_micro_gid_mod_rest)>0){
      
      prop_map_rest <- layered_attr_micro_gid_mod_rest %>% 
        group_by(dist_order, initial_attr_gid) %>% 
        group_split() %>% 
        map_df(~ai_attr_micro_gid_HR_for_a_layer(.x, dgids))
    }
    
    # after adjustments
    prop_map <- prop_map_NS %>% 
      bind_rows(prop_map_WE) %>% 
      bind_rows(prop_map_rest) %>% 
      distinct()
    
  }else{
    prop_map <- layered_attr_micro_gid %>% 
      split(.$dist_order) %>% 
      map_df(~ai_attr_micro_gid_HR_for_a_layer(.x, dgids))
  }
  
  prop_map
}


attr_split_by_encl_and_map_back <- function(lamg, enclosure_direction, dgids_normal, dgids){
  
  dgids_encl <- get_group_id_enclosure(dgids_normal, 
                                       enclosure_direction = enclosure_direction, 
                                       details = T)
  dgencl <- dgids %>% 
    inner_join(dgids_encl$enclosure_gid_map, by = c("data_gid"="gid")) %>% 
    select(-data_gid) %>% 
    rename(data_gid = encl_gid)
  
  lamg <- lamg %>% 
    inner_join(dgids_encl$enclosure_gid_map, by = c("data_gid"="gid")) %>% 
    select(-data_gid) %>% 
    rename(data_gid = encl_gid) %>% 
    distinct()
  
  
  prop_map <- lamg %>% 
    group_by(dist_order, initial_attr_gid) %>% 
    group_split() %>% 
    map_df(~ai_attr_micro_gid_HR_for_a_layer(.x, dgencl))
  
  # translate back to normal data_gid
  
  prop_map <- prop_map %>% 
    inner_join(dgids_encl$enclosure_gid_map, by = c("data_gid"="encl_gid")) %>% 
    select(-data_gid) %>% 
    rename(data_gid = gid) %>% 
    distinct()
  
  prop_map
}


# rather claim_dgids
claim_region_for_attr_micro_gid_HR <- function(this_attr_micro_gid, lamg, dgids){
  
  this_lamg <- lamg %>% filter(attr_micro_gid == this_attr_micro_gid)
  rest_lamg <- lamg %>% filter(attr_micro_gid != this_attr_micro_gid)
  
  rest_dgids <- dgids %>% filter(data_gid %in% rest_lamg$data_gid)
  
  # split for each direction
  proposed_new_map <- this_lamg %>% 
    split(.$direction) %>% 
    map_df(~claim_region_for_attr_micro_gid_HR_for_a_dir(.x, this_lamg, rest_dgids, dgids))
  
  
  proposed_new_map
  
}

claim_region_for_attr_micro_gid_HR_for_a_dir <- function(.x, this_lamg, rest_dgids, dgids){
  
  is_expnd <- FALSE
  
  # W-E 
  if(stringr::str_detect(.x$direction[1],"W")){
    # all dgids that are east to this_attr_micro_gid - related dgid
    this_mc <- min(dgids$col_d[dgids$data_gid %in% .x$data_gid])
    
    claim_region_dgids <- dgids %>% filter(col_d >= this_mc)
    rest_mc <- rest_dgids %>% filter(col_d >= this_mc) %>% 
      pull(col_d) %>% c(Inf) %>% min()
    chk <- claim_region_dgids %>% filter(col_d < rest_mc)
    if(nrow(chk)>0){
      claim_region_dgids <- chk
      is_expnd <- TRUE
    }
  }else{
    if(stringr::str_detect(.x$direction[1],"E")){
      # all dgids that are west to this_attr_micro_gid - related dgid
      this_mc <- max(dgids$col_d[dgids$data_gid %in% .x$data_gid])
      
      claim_region_dgids <- dgids %>% filter(col_d <= this_mc)
      
      rest_Mc <- rest_dgids %>% filter(col_d <= this_mc) %>% 
        pull(col_d) %>% c(-Inf) %>% max()
      
      chk <- claim_region_dgids %>% filter(col_d > rest_Mc)
      if(nrow(chk)>0) {
        claim_region_dgids <- chk
        is_expnd <- TRUE
      }
    }
  }
  
  # N-S
  if(stringr::str_detect(.x$direction[1],"N")){
    # all dgids that are south to this_attr_micro_gid - related dgid
    this_mr <- min(dgids$row_d[dgids$data_gid %in% .x$data_gid])
    claim_region_dgids <- dgids %>% filter(row_d >= this_mr)
    
    rest_mr <- rest_dgids %>% filter(row_d >= this_mr) %>% 
      pull(row_d) %>% c(Inf) %>% min()
    
    chk <- claim_region_dgids %>% filter(row_d < rest_mr)
    if(nrow(chk)>0) {
      claim_region_dgids <- chk
      is_expnd <- TRUE
    }
    
  }else{
    if(stringr::str_detect(.x$direction[1],"S")){
      # all dgids that are north to this_attr_micro_gid - related dgid
      this_mr <- max(dgids$row_d[dgids$data_gid %in% .x$data_gid])
      claim_region_dgids <- dgids %>% filter(row_d <= this_mr)
      
      rest_Mr <- rest_dgids %>% filter(row_d <= this_mr) %>% 
        pull(row_d) %>% c(-Inf) %>% max()
      
      chk <- claim_region_dgids %>% filter(row_d > rest_Mr)
      if(nrow(chk)>0) {
        claim_region_dgids <- chk
        is_expnd <- TRUE
      }
      
    }
  }
  
  claim_region_dgids %>% 
    distinct(info_gid, data_gid) %>% 
    mutate(attr_micro_gid = .x$attr_micro_gid[1], 
           dist_order = .x$dist_order[1], 
           is_expanded = is_expnd)
}
