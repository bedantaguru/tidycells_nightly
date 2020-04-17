
is_attachable <- function(gid1, gid2, d_dat, d_att, data_attr_map, whole_data) {
  
  
  if(check_non_joinable_data_gid(gid1, gid2, d_dat)){
    # in this case update_non_joinable_data_gid not required
    return(FALSE)
  }
  
  ########## logic #########
  #### should have similar major sides (of attributes)
  if (!identical(
    data_attr_map %>% filter(attr_group == "major", data_gid == gid1) %>% pull(direction) %>% unique() %>% sort(),
    data_attr_map %>% filter(attr_group == "major", data_gid == gid2) %>% pull(direction) %>% unique() %>% sort()
  )) {
    update_non_joinable_data_gid(gid1, gid2, d_dat)
    return(FALSE)
  }
  
  ########## logic #########
  #### if any intersecting cell present
  # this is ensured already skipping this test
  
  ########## logic #########
  #### should have no other entry within the enclosed combined boundary (direction-wise)
  # @Dev this log is changing to
  #### should have no other entry (non empty) within the enclosed combined boundary attaching major attributes (direction-wise)
  
  data_attr_map_this <- data_attr_map %>% filter(data_gid %in% c(gid1, gid2))
  
  chks <- c("N","E","W","S") %>% map_lgl(~{
    
    dm0 <- data_attr_map_this %>% filter(direction == .x)
    
    if(nrow(dm0)>0){
      this_group_info <- d_dat %>% filter(gid %in% c(gid1, gid2)) %>% 
        bind_rows(
          # attached attributes to these data_gids
          d_att %>% filter(gid %in% (dm0 %>% pull(attr_gid)))
        )
      this_group_info <- this_group_info %>% distinct(row, col) %>% mutate(gid = "dummy")
      combined_boundary <- get_group_id_boundary(this_group_info)
      this_region_data <- whole_data %>%
        filter(type!="empty") %>% 
        filter(
          row <= combined_boundary$r_max,
          row >= combined_boundary$r_min,
          col <= combined_boundary$c_max,
          col >= combined_boundary$c_min
        )
      this_region_data_rest <- this_region_data %>%
        anti_join(this_group_info, by = c("row", "col")) %>%
        filter(type %in% c("value", "attribute"))
      
      if (nrow(this_region_data_rest) > 0) {
        return(FALSE)
      }
      
    }
    
    return(TRUE)
  })
  
  if(any(!chks)){
    # if any direction is non-attachable
    update_non_joinable_data_gid(gid1, gid2, d_dat)
    return(FALSE)
  }
  
  
  return(TRUE)
}


update_non_joinable_data_gid <- function(gid1, gid2, d_dat){
  d_dat_smpl <- d_dat %>% 
    filter(gid == gid1 | gid == gid2) %>% 
    group_by(gid) %>% 
    summarise(row_d = row[1], col_d = col[1])
  
  common_knowledge(
    non_joinable_data_gid = d_dat_smpl %>% 
      mutate(id = paste0(sort(gid), collapse = "_")) %>% 
      select(-gid)
  )
}

check_non_joinable_data_gid <- function(gid1, gid2, d_dat){
  res <- F
  if(is_common_knowledge("non_joinable_data_gid")){
    njdg <- common_knowledge("non_joinable_data_gid")
    
    d_dat_smpl <- d_dat %>% 
      filter(gid == gid1 | gid == gid2) %>% 
      group_by(gid) %>% 
      summarise(row_d = row[1], col_d = col[1])
    
    njdg <- njdg %>% inner_join(d_dat_smpl, by = c("row_d", "col_d"))
    
    if(nrow(njdg)>0){
      g1 <- njdg %>% filter(gid == gid1) %>% pull(id)
      g2 <- njdg %>% filter(gid == gid2) %>% pull(id)
      res <- length(intersect(g1, g2))>0
    }
    
  }
  res
}