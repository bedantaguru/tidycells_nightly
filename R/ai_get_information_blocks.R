
#@Dev
# add doc
# any common attribute cell in major type 

ai_get_information_blocks <- function(admap, d_dat, d_att){
  
  admap <- admap %>% mutate(info_gid = paste0("ib_",data_gid))
  
  
  repeat({
    
    admap_majors_attr_cell_wise <- admap %>% 
      filter(attr_group=="major") %>% 
      inner_join(d_att %>% select(attr_gid = gid, row_a = row, col_a = col), by = "attr_gid")
    
    inf_gid_map <- admap_majors_attr_cell_wise %>% 
      group_by(row_a, col_a) %>% 
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


# @Dev may need file arrengments
# actually this is a help to ai_main_part_phase_3_rest_map
# ai_get_information_blocks is also a helper to ai_main_part_phase_3_rest_map


admap_for_rest_dir_fix <- function(d_dat, d_att, admap_fr){
  
  this_dgid <- d_dat %>% filter(gid %in% admap_fr$data_gid) 
  this_agid <- d_att %>% filter(gid %in% admap_fr$attr_gid) %>% rename(attr_gid = gid)
  this_dgid_bd <- get_group_id_boundary(this_dgid) %>% rename(data_gid = gid)
  
  this_rawmap <- admap_fr %>% distinct(attr_gid, data_gid) %>% 
    inner_join(this_agid, by = "attr_gid") %>% 
    inner_join(this_dgid_bd, by = "data_gid")
  
  this_rawmap <- get_direction_df_nn(this_rawmap)
  
  this_ref <- this_rawmap %>% 
    group_by(attr_gid, data_gid) %>% 
    filter(dist == min(dist)) %>% 
    summarise(dist = dist [1], direction =direction[1], direction_group = direction_group[1]) %>% 
    ungroup()
  
  admap_fr %>% 
    select(-dist, -direction, -direction_group) %>% 
    inner_join(this_ref, by = c("attr_gid", "data_gid"))
}

