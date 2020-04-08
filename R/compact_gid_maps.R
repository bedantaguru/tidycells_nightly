
# diffrent split resulted in same attr_gid
# these will be compated to single group
compact_gid_maps <- function(d_att, admap) {
  
  foot_prints <- d_att %>%
    filter(gid %in% admap$attr_gid) %>% 
    distinct(row, col, gid) %>% 
    group_by(gid) %>% 
    arrange(row, col) %>%
    summarise(fp = paste0(row, ",", col, collapse = ";"))

  chk <- foot_prints %>% count(fp) %>% filter(n>1)
  
  if(nrow(chk)>0){
    
    # split overlap is present
    ngmap <- foot_prints %>%
      group_by(fp) %>%
      mutate(new_gid = min(gid)) %>%
      ungroup() %>%
      distinct(gid, new_gid)
    
    d_att <- d_att %>% left_join(ngmap, by = "gid")
    d_att <- d_att %>%
      mutate(new_gid = ifelse(is.na(new_gid), gid, new_gid)) %>%
      select(-gid) %>%
      rename(gid = new_gid) %>%
      distinct()
    
    admap_main_raw_map_new <- admap %>%
      left_join(ngmap %>% rename(attr_gid = gid, new_attr_gid = new_gid), by = "attr_gid")
    admap_main_raw_map_new <- admap_main_raw_map_new %>%
      mutate(new_attr_gid = ifelse(is.na(new_attr_gid), attr_gid, new_attr_gid)) %>%
      select(-attr_gid) %>%
      rename(attr_gid = new_attr_gid) %>%
      distinct()
    
    admap <- admap_main_raw_map_new
    
  }
  

  list(d_att = d_att, admap = admap)
}
