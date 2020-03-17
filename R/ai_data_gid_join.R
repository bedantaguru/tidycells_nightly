ai_data_gid_join <- function(d_dat, d_att, data_attr_map, full_data) {
  repeat({
    if (length(unique(d_dat$gid)) < 2) break()
    
    this_intra_data_block_dist <- d_dat %>% approx_intra_block_dist()
    
    #@Dev
    # need to check performance 
    # data_gid_comb <- d_dat$gid %>%
    #   unique() %>%
    #   utils::combn(2) %>%
    #   as.data.frame(stringsAsFactors = FALSE)
    
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
  })
  d_dat
}
