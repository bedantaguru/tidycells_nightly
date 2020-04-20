
is_attachable <- function(gid1, gid2, d_dat, d_att, data_attr_map, whole_data) {
  
  # logic is kept in increasing complexcity order
  # microbenchmark(f1(), f2(), f3())
  # Unit: milliseconds
  # expr       min        lq      mean    median        uq       max neval
  # f1()  2.414111  2.549463  2.777335  2.627937  2.722449  8.283669   100
  # f2()  2.679256  2.804345  3.250778  2.883889  3.003205 12.652160   100
  # f3() 13.044320 13.362708 15.535984 13.600698 14.118588 69.345895   100
 
  ########## logic #########
  #### should have similar major sides (of attributes)
  if (!identical(
    data_attr_map %>% filter(attr_group == "major", data_gid == gid1) %>% pull(direction) %>% unique() %>% sort(),
    data_attr_map %>% filter(attr_group == "major", data_gid == gid2) %>% pull(direction) %>% unique() %>% sort()
  )) {
    return(FALSE)
  }
  
  ########## logic #########
  #### if any intersecting cell present
  #### it will be duplication if logic fails
  d_com <- d_dat %>% filter(gid %in% c(gid1, gid2)) %>% mutate(gid = "com")
  gbd <- get_group_id_boundary(d_com)
  cells_in <- whole_data %>% filter(row<=gbd$r_max, row>=gbd$r_min,
                                    col<=gbd$c_max, col>=gbd$c_min)
  cells_in_no_dat <- cells_in %>% anti_join(d_com, by = c("row", "col"))
  if(nrow(cells_in_no_dat)>0){
    return(FALSE)
  }
  
  
  ########## logic #########
  #### should have no other entry within the enclosed combined boundary (direction-wise)
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
    return(FALSE)
  }
  
  
  return(TRUE)
}



