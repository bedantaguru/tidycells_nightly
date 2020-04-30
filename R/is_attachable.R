
is_attachable <- function(gid1, gid2, d_dat, d_att, data_attr_map, whole_data) {
  
  # logic is kept in increasing complexcity order
  # microbenchmark(f1(), f2(), f3())
  # Unit: milliseconds
  # expr       min        lq      mean    median        uq       max neval
  # f1()  2.414111  2.549463  2.777335  2.627937  2.722449  8.283669   100
  # f2()  2.679256  2.804345  3.250778  2.883889  3.003205 12.652160   100
  # f3() 13.044320 13.362708 15.535984 13.600698 14.118588 69.345895   100
  
  # is_attachable_prune_vectorized_logic_1 is taking care of this logic
  ########## logic #########
  #### should have similar major sides (of attributes)
  # if (!identical(
  #   data_attr_map %>% filter(attr_group == "major", data_gid == gid1) %>% pull(direction) %>% unique() %>% sort(),
  #   data_attr_map %>% filter(attr_group == "major", data_gid == gid2) %>% pull(direction) %>% unique() %>% sort()
  # )) {
  #   return(FALSE)
  # }
  
  
  ########## logic #########
  #### if any intersecting cell present
  #### it will be duplication if logic fails
  d_com <- d_dat[d_dat$gid %in% c(gid1, gid2),]
  chk <- is_attachable_logic_2_sub(d_com, whole_data)
  if(isFALSE(chk)){
    return(FALSE)
  }
  
  
  ########## logic #########
  #### should have no other attribute entry within the enclosed combined boundary (direction-wise)
  #### should have no other attribute entry (non empty) within the enclosed combined boundary attaching major attributes (direction-wise)
  
  data_attr_map_this <- data_attr_map %>% filter(data_gid %in% c(gid1, gid2))
  
  chks <- c("N","E","W","S") %>% map_lgl(~{
    
    dm0 <- data_attr_map_this %>% filter(direction == .x)
    
    if(nrow(dm0)>0){
      this_group_info <- d_dat %>% filter(gid %in% c(gid1, gid2)) %>% 
        bind_rows(
          # attached attributes to these data_gids
          d_att %>% filter(gid %in% dm0$attr_gid)
        )
      # @Dev
      # whole_data[whole_data$type == "attribute",] newly added check consistency
      chk <- is_attachable_logic_2_sub(this_group_info, whole_data[whole_data$type == "attribute",])
      if (isFALSE(chk)) {
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

is_attachable_logic_2_sub <- function(d_com, whole_data){
  gbd <- list(r_max = max(d_com$row), r_min = min(d_com$row),
              c_max = max(d_com$col), c_min = min(d_com$col))
  
  cells_in_rc <- whole_data$rc[whole_data$row<=gbd$r_max & 
      whole_data$row>=gbd$r_min &
      whole_data$col<=gbd$c_max & 
      whole_data$col>=gbd$c_min]
  
  cells_in_rc_rest <- setdiff( cells_in_rc, d_com$rc)
  
  if(length(cells_in_rc_rest)>0){
    return(FALSE)
  }
  return(TRUE)
}

is_attachable_prune_vectorized_logic_1 <- function(data_gid_comb, data_attr_map){
  
  data_gids_with_dir_fp <- data_attr_map %>% 
    filter(attr_group=="major") %>% 
    arrange(direction) %>% 
    group_by(data_gid) %>% 
    summarise(dir_fp = paste0(unique(direction), collapse = "_"))
  
  
  no_major_data_gids <- data_attr_map %>% 
    group_by(data_gid) %>% 
    mutate(any_major = any(attr_group=="major")) %>% 
    ungroup() %>% 
    filter(!any_major)
  
  if(nrow(no_major_data_gids)>0){
    no_major_data_gids <- no_major_data_gids %>% select(data_gid) %>% mutate(dir_fp = "dummy")
    data_gids_with_dir_fp <- data_gids_with_dir_fp %>% bind_rows(no_major_data_gids)
  }
  
  possible_merges <- data_gids_with_dir_fp %>% split(.$dir_fp) %>% 
    map_df(~expand.grid(gid1 = .x$data_gid, gid2 = .x$data_gid, stringsAsFactors = F))
  
  data_gid_comb %>% inner_join(possible_merges, by = c("gid1", "gid2"))
  
}


