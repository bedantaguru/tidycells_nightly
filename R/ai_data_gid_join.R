ai_data_gid_join <- function(d_dat, d_att, data_attr_map, full_data) {
  
  done <- F
  
  # @Dev push this inside checks or handle differently
  # if any non_joinable information is present
  if(is_common_knowledge("non_joinable")){
    non_joinable_info <- common_knowledge("non_joinable") 
  }else{
    non_joinable_info <- tibble()
  }
  
  if(nrow(non_joinable_info)>0){
    non_joinable_info <- non_joinable_info %>% 
      inner_join(d_dat, by=c("data_gid"="gid")) %>% 
      group_by(id, data_gid) %>% 
      summarise(row = row[1], col = col[1]) %>% 
      ungroup()
  }
  
  repeat({
    
    if (length(unique(d_dat$gid)) < 2) break()
    
    data_gid_comb <- d_dat %>% approx_intra_block_dist()
    
    #@Dev
    # need to check performance 
    #  may introduce center dist
    # data_gid_comb <- d_dat$gid %>%
    #   unique() %>%
    #   utils::combn(2) %>%
    #   as.data.frame(stringsAsFactors = FALSE)
    
    
    
    if(is_common_knowledge("non_joinable")){
      if(nrow(non_joinable_info)>0){
        browser()
        # @Dev
        # remove it it not required
        # nj_chk <- data_gid_comb %>% 
        #   apply(MARGIN = 2, 
        #         function(.x){non_joinable_info %>% filter(data_gid %in% .x) %>% pull(id) %>% unique() %>% length()})
        # 
        #  this is also bad start with initial block list from outside this loop
        
        nj_chk <- data_gid_comb %>% 
          map_lgl(~{
            rc_gd1 <- d_dat %>% filter(gid == .x[1]) %>% inner_join(non_joinable_info, by = c("row","col"))
            rc_gd2 <- d_dat %>% filter(gid == .x[2]) %>% inner_join(non_joinable_info, by = c("row","col"))
            (nrow(rc_gd1)>0 & nrow(rc_gd2)>0)
          })
        
        data_gid_comb <- data_gid_comb[!nj_chk]
      }
    }
    
    #  @Dev need further tuning
    
    if(nrow(data_gid_comb)>0){
      
      if(nrow(data_gid_comb)>20){
        data_gid_comb <- data_gid_comb %>% filter(d <= quantile(d, 1/2))
      }
      
      data_gid_comb <- data_gid_comb %>%
        dplyr::rowwise() %>% 
        mutate(is_attachable_gids = is_attachable(
          gid1, gid2,
          d_dat, d_att, data_attr_map,
          whole_data = full_data
        )) %>% 
        ungroup()
      
      if (any(data_gid_comb$is_attachable_gids)) {
        data_gid_joins <- data_gid_comb %>% 
          filter(is_attachable_gids) %>% 
          select(gid= gid1, new_gid= gid2)
        
        data_gid_joins <-  gid_map_link_tune(data_gid_joins)
        
        d_dat <- get_group_id_join_gids(d_dat, gid_map = data_gid_joins, no_need_to_tune = T)
        data_attr_map <- ai_update_admap_after_data_gid_join(data_attr_map, link_tuned_gid_map = data_gid_joins)
        done <-  T
      } else {
        break()
      }
      
    }else{
      break()
    }
    
  })
  
  list(admap = data_attr_map, d_dat = d_dat, done = done)
}


###########
# helpers #
###########


ai_update_admap_after_data_gid_join <- function(admap, link_tuned_gid_map){
  
  admap %>%
    left_join(link_tuned_gid_map, by = c("data_gid" = "gid")) %>%
    mutate(new_gid = ifelse(is.na(new_gid), data_gid, new_gid)) %>%
    select(-data_gid) %>%
    rename(data_gid = new_gid) %>% 
    group_by(attr_gid, data_gid) %>% 
    dplyr::summarise_all(stat_mode) %>% 
    ungroup()
  
}




