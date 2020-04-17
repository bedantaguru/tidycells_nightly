
#@Dev
# add doc

ai_get_information_blocks <- function(admap, d_dat, d_att){
  
  admap <- admap %>% mutate(info_gid = paste0("ib_",data_gid))
  
  
  repeat({
    
    admap_majors <- admap %>% filter(attr_group=="major")
    
    inf_gid_map <- admap_majors %>% 
      group_by(attr_gid) %>% 
      mutate(info_gid_new = min(info_gid)) %>% 
      ungroup() %>% 
      filter(info_gid!=info_gid_new) %>% 
      distinct(info_gid, info_gid_new)
    
    if(nrow(inf_gid_map)==0){
      break()
    }
    
    inf_gid_map <- inf_gid_map %>% 
      rename(gid = info_gid, new_gid = info_gid_new) %>% 
      gid_map_link_tune() %>% 
      rename(info_gid = gid, info_gid_new = new_gid)
    
    is_compressed <- length(unique(inf_gid_map$info_gid))>length(unique(inf_gid_map$info_gid_new))
    
    if(is_compressed){
      admap <- admap %>% 
        left_join(inf_gid_map, by = "info_gid") %>% 
        mutate(info_gid_new = ifelse(is.na(info_gid_new), info_gid, info_gid_new)) %>% 
        select(-info_gid) %>% 
        rename(info_gid = info_gid_new)
    }else{
      break()
    }
    
  })
  
  # info block row, col
  d_inf <- admap %>% distinct(attr_gid, data_gid, info_gid)
  
  d_inf <- d_inf %>% inner_join(d_dat, by = c("data_gid"="gid")) %>% 
    bind_rows(d_inf %>% inner_join(d_att, by = c("attr_gid"="gid"))) %>% 
    distinct(row, col, gid = info_gid)
  
  list(map = admap, d_inf = d_inf)
  
}
