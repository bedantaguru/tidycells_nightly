ai_data_gid_join <- function(d_dat, d_att, data_attr_map, full_data) {
  
  done <- F
  
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
    
    # this_intra_data_block_dist <- d_dat %>% approx_intra_block_dist()
    
    #@Dev
    # need to check performance 
    #  may introduce center dist
    data_gid_comb <- d_dat$gid %>%
      unique() %>%
      utils::combn(2) %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    
    if(is_common_knowledge("non_joinable")){
      if(nrow(non_joinable_info)>0){
        
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
    
    if(ncol(data_gid_comb)>0){
      
      #@Dev
      browser()
      
      this_intra_data_block_dist <- this_intra_data_block_dist %>% filter(d <= quantile(d, 1/4))
      
      data_gid_comb <- this_intra_data_block_dist %>% select(-d) %>% t %>% as_tibble()
      
      data_gid_comb_chk <- data_gid_comb %>%
        map_lgl(~ is_attachable(
          gid1 = .x[1], gid2 = .x[2],
          d_dat, d_att, data_attr_map,
          whole_data = full_data
        ))
      
      if (any(data_gid_comb_chk)) {
        data_gid_joins <- data_gid_comb[data_gid_comb_chk]
        
        data_gid_join_map <- data_gid_joins %>% t() %>% as_tibble() %>% rename(gid= V1, new_gid= V2)
        
        d_dat <- get_group_id_join_gids(d_dat, gid_map = data_gid_join_map)
      } else {
        break()
      }
      
    }else{
      break()
    }
    
  })
  
  list(d_dat = d_dat, done = done)
}
