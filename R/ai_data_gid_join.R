ai_data_gid_join <- function(d_dat, d_att, data_attr_map, full_data) {
  
  
  done <- F
  
  njdg <- common_knowledge("non_joinable_data_gid")
  
  repeat({
    
    if (length(unique(d_dat$gid)) < 2) break()
    
    
    # only dist based approach : 
    #data_gid_comb <- d_dat %>% approx_intra_block_dist()
    data_gid_comb <- d_dat %>% get_possible_data_gid_mergeable()
    
    data_gid_comb <- data_gid_comb %>% anti_join(njdg, by = c("gid1", "gid2"))
    
    #  @Dev need further tuning
    
    if(nrow(data_gid_comb)>0){
      
      if(nrow(data_gid_comb)>50){
        # @Dev
        # very far apart dist will not be tackled here
        # so above data_gid_comb <- d_dat %>% approx_intra_block_dist() is not good
        data_gid_comb <- data_gid_comb %>% arrange(d)
        data_gid_comb <- data_gid_comb[seq(20),]
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
        njdg <- update_non_joinable_data_gid(njdg, link_tuned_gid_map = data_gid_joins)
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


update_non_joinable_data_gid <- function(njdg, link_tuned_gid_map){
  if(any(link_tuned_gid_map$gid %in% c(njdg$gid1, njdg$gid2))){
    njdg %>% 
      left_join(link_tuned_gid_map, by = c("gid1"="gid")) %>% 
      mutate(gid1 = ifelse(is.na(new_gid), gid1, new_gid)) %>% 
      select(-new_gid) %>% 
      left_join(link_tuned_gid_map, by = c("gid2"="gid")) %>% 
      mutate(gid2 = ifelse(is.na(new_gid), gid2, new_gid)) %>% 
      select(-new_gid)
  }
  
  njdg
}


get_possible_data_gid_mergeable <- function(d_dat){
  
  dg_sides <- ai_get_data_attr_map(get_group_id_boundary(d_dat), d_dat)$map
  
  dg_sides <- dg_sides %>%  rename(g1 = data_gid, g2 =attr_gid) %>% filter(g1!=g2)
  
  dg_sides <- dg_sides %>% mutate(gid1 = pmin(g1, g2), gid2 = pmax(g1, g2))
  
  dg_sides %>% group_by(gid1, gid2) %>% summarise(d = mean(dist)) %>% ungroup()
}


