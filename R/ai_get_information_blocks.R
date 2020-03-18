
#@Dev
# add doc

ai_get_information_blocks <- function(admap, d_dat, d_att){
  admap <- admap %>% mutate(info_gid = paste0("ib_",data_gid))
  
  admap_majors <- admap %>% filter(attr_group=="major")
  
  if(nrow(admap_majors)==0) return(admap)
  
  repeat({
    
    if (length(unique(admap$info_gid)) < 2) break()
    
    info_gid_comb <- admap$info_gid %>%
      unique() %>%
      utils::combn(2) %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    
    if(ncol(info_gid_comb)>0){
      
      info_gid_comb_chk <- info_gid_comb %>%
        map_lgl(~ is_attachable_as_info_block(
          gid1 = .x[1], gid2 = .x[2],
          admap_majors
        ))
      
      if (any(info_gid_comb_chk)) {
        info_gid_joins <- info_gid_comb[info_gid_comb_chk]
        
        info_gid_join_map <- info_gid_joins %>% t() %>% as_tibble() %>% rename(gid= V1, new_gid= V2)
        info_gid_join_map <- gid_map_link_tune(info_gid_join_map)
        
        admap <- admap %>% 
          left_join(info_gid_join_map, by = c("info_gid"="gid")) %>% 
          mutate(new_gid = ifelse(is.na(new_gid), info_gid, new_gid)) %>% 
          select(-info_gid) %>% 
          rename(info_gid = new_gid)
        
      } else {
        break()
      }
      
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

# helpers

is_attachable_as_info_block <- function(gid1, gid2, major_admap){
  
  ########## logic #########
  #### should have at least a common attr gid (in major - this is done by passing major map only)
  adm1 <- major_admap %>% filter(info_gid==gid1) %>% select(attr_gid, direction)
  adm2 <- major_admap %>% filter(info_gid==gid2) %>% select(attr_gid, direction)
  chk <- adm1 %>% inner_join(adm2, by = c("attr_gid", "direction"))
  if(nrow(chk)>0){
    return(TRUE)
  }
  
  return(FALSE)
}
