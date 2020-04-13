
# @Dev
# need to check situation in both side layered headers
# also document

# full name should be 
# ai_attr_micro_gid_hierarchical_reallocation
ai_hierarchical_reallocation <- function(admap_cellwise_raw_asp){
  
  # take minor and corner micro gids
  minor_attr_micro_gid <- admap_cellwise_raw_asp %>% 
    filter(attr_group=="minor" | direction_group == "corner", dist_order>1) %>% 
    distinct(info_gid, data_gid, attr_micro_gid, dist_order, direction_basic, direction_group)
  
  # leave attr_micro_gid within each info block which 
  #  has connectivity to all data_gid in that info block
  info_block_wise_data_block_count <- minor_attr_micro_gid %>% 
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
  
  prop_map <- layered_attr_micro_gid %>% 
    split(.$dist_order) %>% 
    map_df(~ai_attr_micro_gid_HR_for_a_layer(.x, dgids))
  
  admap_cellwise_raw_asp_rest <- admap_cellwise_raw_asp %>% 
    anti_join(prop_map, by = c("data_gid", "info_gid", "attr_micro_gid"))
  
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
  chk <- prop1 %>% group_by(data_gid) %>% 
    mutate(namg = n_distinct(attr_micro_gid)) %>% ungroup()
  if(nrow(chk %>% filter(namg>1))>0){
    # duplicate resolution required
    # @Dev
    browser()
  }
  prop1
}


# rather claim_dgids
claim_region_for_attr_micro_gid_HR <- function(this_attr_micro_gid, lamg, dgids){
  
  this_lamg <- lamg %>% filter(attr_micro_gid == this_attr_micro_gid)
  rest_lamg <- lamg %>% filter(attr_micro_gid != this_attr_micro_gid)
  
  rest_dgids <- dgids %>% filter(data_gid %in% rest_lamg$data_gid)
  
  # split for each direction
  proposed_new_map <- this_lamg %>% 
    split(.$direction_basic) %>% 
    map_df(~{
      
      # N - S 
      if(stringr::str_detect(.x$direction_basic[1],"W")){
        # all dgids that are east to this_attr_micro_gid - related dgid
        this_mc <- min(dgids$col_d[dgids$data_gid %in% .x$data_gid])
        
        claim_region_dgids <- dgids %>% filter(col_d >= this_mc)
        rest_mc <- rest_dgids %>% filter(col_d >= this_mc) %>% 
          pull(col_d) %>% c(Inf) %>% min()
        chk <- claim_region_dgids %>% filter(col_d < rest_mc)
        if(nrow(chk)>0) claim_region_dgids <- chk
      }else{
        if(stringr::str_detect(.x$direction_basic[1],"E")){
          # all dgids that are west to this_attr_micro_gid - related dgid
          this_mc <- max(dgids$col_d[dgids$data_gid %in% .x$data_gid])
          
          claim_region_dgids <- dgids %>% filter(col_d <= this_mc)
          
          rest_Mc <- rest_dgids %>% filter(col_d <= this_mc) %>% 
            pull(col_d) %>% c(-Inf) %>% max()
          
          chk <- claim_region_dgids %>% filter(col_d > rest_Mc)
          if(nrow(chk)>0) claim_region_dgids <- chk
        }
      }
      
      # W - E 
      if(stringr::str_detect(.x$direction_basic[1],"N")){
        # all dgids that are south to this_attr_micro_gid - related dgid
        this_mr <- min(dgids$row_d[dgids$data_gid %in% .x$data_gid])
        claim_region_dgids <- dgids %>% filter(row_d >= this_mr)
        
        rest_mr <- rest_dgids %>% filter(row_d >= this_mr) %>% 
          pull(row_d) %>% c(Inf) %>% min()
        
        chk <- claim_region_dgids %>% filter(row_d < rest_mr)
        if(nrow(chk)>0) claim_region_dgids <- chk
        
      }else{
        if(stringr::str_detect(.x$direction_basic[1],"S")){
          # all dgids that are north to this_attr_micro_gid - related dgid
          this_mr <- max(dgids$row_d[dgids$data_gid %in% .x$data_gid])
          claim_region_dgids <- dgids %>% filter(row_d <= this_mr)
          
          rest_Mr <- rest_dgids %>% filter(row_d <= this_mr) %>% 
            pull(row_d) %>% c(-Inf) %>% max()
          
          chk <- claim_region_dgids %>% filter(row_d > rest_Mr)
          if(nrow(chk)>0) claim_region_dgids <- chk
          
        }
      }
      
      claim_region_dgids %>% 
        distinct(info_gid, data_gid) %>% 
        mutate(attr_micro_gid = .x$attr_micro_gid[1], 
               dist_order = .x$dist_order)
    })
  
  
  proposed_new_map
  
}

