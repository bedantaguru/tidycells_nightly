
# diffrent split resulted in same attr_gid
# these will be compated to single group
compact_attr_gid_maps <- function(d_att, admap) {
  
  foot_prints <- d_att %>%
    filter(gid %in% admap$attr_gid) %>% 
    distinct(row, col, gid) %>% 
    group_by(gid) %>% 
    arrange(row, col) %>%
    summarise(fp = paste0(row, ",", col, collapse = ";"))

  chk <- foot_prints %>% count(fp) %>% filter(n>1)
  
  done <- F
  
  if(nrow(chk)>0){
    
    done <- T
    
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
  

  list(done = done, d_att = d_att, admap = admap)
}



compact_attr_micro_gid_maps <- function(admap_cellwise_raw_asp){
  
  foot_prints <- admap_cellwise_raw_asp %>%
    distinct(row = row_a, col = col_a, attr_micro_gid, attr_var_sync_name) %>% 
    group_by(attr_micro_gid, attr_var_sync_name) %>% 
    arrange(row, col) %>%
    summarise(fp = paste0(row, ",", col, collapse = ";")) %>% 
    ungroup()
  
  chk <- foot_prints %>% count(fp, attr_var_sync_name) %>% filter(n>1)
  
  done <- F
  
  if(nrow(chk)>0){
    
    done <- T
    
    # split overlap is present
    ngmap <- foot_prints %>%
      group_by(fp, attr_var_sync_name) %>%
      mutate(new_attr_micro_gid = min(attr_micro_gid)) %>%
      ungroup() %>%
      distinct(attr_micro_gid, attr_var_sync_name,  new_attr_micro_gid)
    
    admap_cellwise_raw_asp_new <- admap_cellwise_raw_asp %>%
      left_join(ngmap,
                by = c("attr_micro_gid","attr_var_sync_name"))
    
    admap_cellwise_raw_asp_new <- admap_cellwise_raw_asp_new %>%
      mutate(new_attr_micro_gid = ifelse(is.na(new_attr_micro_gid), attr_micro_gid, new_attr_micro_gid)) %>%
      select(-attr_micro_gid) %>%
      rename(attr_micro_gid = new_attr_micro_gid) %>%
      distinct()
    
    return(admap_cellwise_raw_asp_new)
    
  }
  
  admap_cellwise_raw_asp
  
}

