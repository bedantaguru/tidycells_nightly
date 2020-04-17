ai_data_gid_join <- function(d_dat, d_att, data_attr_map, full_data) {
  
  
  refresh_stale_info_non_joinable_data_gid(d_dat)
  
  done <- F
  
  repeat({
    
    if (length(unique(d_dat$gid)) < 2) break()
    
    data_gid_comb <- d_dat %>% approx_intra_block_dist()
    
    
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




