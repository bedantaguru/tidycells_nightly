
# ... kept for optional: is_detailed_connection_build_up
ai_get_data_attr_map <- function(dat_boundary,
                                 att_gid_map,
                                 attr_to_near_data = FALSE, 
                                 leave_inside = FALSE, ...) {

  # check relative location of each attr_gid (gid) wrt each data_gid
  d_att_map <- get_raw_map_for_ai_get_data_attr_map(dat_boundary, att_gid_map, leave_inside)


  #  connect data and attr gids based on above raw map
  connect_data_and_attr_groups_from_raw_map(d_att_map, 
                                            attr_to_near_data = attr_to_near_data, 
                                            leave_inside = leave_inside, ...)
  
}

# helpers
# @Dev use this to reduce computation wheever raw map only is required
# this can be done in attr_split mostly.
get_raw_map_for_ai_get_data_attr_map  <- function(dat_boundary,
                                                  att_gid_map,
                                                  leave_inside = FALSE){
  d_att_map <- get_direction_df(dat_boundary, d_att = att_gid_map, allow_inside = leave_inside)
  d_att_map <- d_att_map %>%
    rename(attr_gid = gid) %>% 
    mutate(mapping_strength = 0)
  d_att_map
}


connect_data_and_attr_groups_from_raw_map <- function(raw_map,
                                                      attr_to_near_data = FALSE, 
                                                      leave_inside = FALSE, 
                                                      is_detailed_connection_build_up){
  
  # for each attr_gid (gid), data_gid, direction, direction_group :- get minimum distance
  # all possible maps
  d_gid_att_map <- raw_map %>%
    group_by(attr_gid, data_gid, direction, direction_group) %>%
    summarise(dist = min(dist), mapping_strength = max(mapping_strength)) %>%
    ungroup() 
  
  if(missing(is_detailed_connection_build_up)){
    is_detailed_connection_build_up <- isTRUE(getOption("tidycells.analyze_cells_options")[["detailed_connection_build_up"]])
  }
  
  
  # attach nearest attr_gid to each data gid
  # by default each data gid will choose nearest attr_gid (multiple allowed)
  if(is_detailed_connection_build_up){
    d_gid_att_map_min_d <- detailed_connection_build_up(d_gid_att_map)
  }else{
    d_gid_att_map_min_d <- simple_connection_build_up(d_gid_att_map)
  }
  
  
  if (attr_to_near_data) {
    # this does same for attr gid too
    # after each data gid choose nearest attr then each attr will choose nearest data gid
    
    d_gid_att_map_min_d <- d_gid_att_map_min_d %>%
      group_by(attr_gid) %>%
      filter(dist == min(dist)) %>%
      ungroup() 
  }
  
  
  lo <- list(map = d_gid_att_map_min_d, all_map = d_gid_att_map, raw = raw_map)
  if (!leave_inside) {
    lo
  } else {
    lo %>% map(~ .x %>% filter(direction != "INSIDE"))
  }
}



simple_connection_build_up <- function(d_gid_att_map){
  d_gid_att_map_min_d <- d_gid_att_map %>%
    group_by(data_gid, direction_group) %>%
    filter(dist == min(dist)) %>%
    ungroup() 
  
  d_gid_att_map_min_d
}



detailed_connection_build_up <- function(d_gid_att_map){
  
  strong_corners <- !isFALSE(getOption("tidycells.analyze_cells_options")[["detailed_connection_build_up_strong_method"]])
  
  d_gid_att_map_min_d_t2 <- d_gid_att_map %>%
    group_by(data_gid, direction, direction_group) %>%
    filter(dist == min(dist)) %>%
    ungroup() 
  
  # for the data_gid having two sides (like N and S) in non-corner case
  # if there is any other data_gid having less dist in same direction
  # the connection is deleted
  
  data_gid_duals <- d_gid_att_map_min_d_t2 %>% 
    filter(direction_group=="NS"|direction_group=="WE") %>% 
    group_by(data_gid, direction_group) %>% 
    mutate(ndir = n_distinct(direction)) %>% 
    ungroup() %>% 
    filter(ndir>1)
  
  attr_mdist_for_duals <- d_gid_att_map_min_d_t2 %>% 
    filter(attr_gid %in% data_gid_duals$attr_gid) %>% 
    group_by(attr_gid) %>% 
    summarise(a_m_dist = min(dist)) %>% 
    ungroup()
  
  
  data_gid_duals <- data_gid_duals %>% 
    inner_join(attr_mdist_for_duals,
               by = c("attr_gid"))
  
  # it will discard many
  data_gid_duals_invalids <- data_gid_duals %>% filter(dist>a_m_dist)
  
  # so here is adjustment
  # 1st those who are loosing all in each direction grp
  data_gid_duals_invalids_duals <- data_gid_duals_invalids %>% 
    group_by(data_gid, direction_group) %>% 
    mutate(ndir = n_distinct(direction)) %>% 
    ungroup() %>% 
    filter(ndir>1)
  
  # each data_gid would try to connect near attr for each dir grp
  data_gid_duals_valids <- data_gid_duals %>% 
    filter(data_gid %in% data_gid_duals_invalids_duals$data_gid) %>% 
    group_by(data_gid, direction_group) %>% 
    filter(dist == min(dist)) %>% 
    ungroup()
  
  data_gid_duals_invalids <- data_gid_duals_invalids %>% 
    anti_join(data_gid_duals_valids, by = c("attr_gid", "data_gid", "direction"))
  
  # delete these invalids
  
  d_gid_att_map_min_d_t2 <- d_gid_att_map_min_d_t2 %>% 
    anti_join(data_gid_duals_invalids, by = c("attr_gid", "data_gid", "direction"))
  
  
  # logic 
  # dir_var is variance in directino group hist for a attribute
  # most_dir_grp  is most frequent dir group
  # if a attr is ever NS or WE then that will never become corner
  # in another word corner can be those who have only become corner
  attr_cls <- d_gid_att_map_min_d_t2 %>% 
    group_by(attr_gid, data_gid, direction_group) %>% 
    summarise(md = min(dist)) %>% 
    ungroup()
  
  corner_split_case_dist <- function(x){
    sqrt(x^2+1.3)
  }
  
  attr_cls_dual_dir <- attr_cls %>% 
    group_by(data_gid, attr_gid) %>% 
    mutate(dual_dir = ("corner" %in% direction_group) & 
             (("NS" %in% direction_group) | ("WE" %in% direction_group))) %>% 
    ungroup() %>% 
    filter(dual_dir)
  
  if(strong_corners){
    attr_cls_dual_dir_corner <- attr_cls_dual_dir %>% 
      filter(direction_group=="corner")
    attr_cls_dual_dir_non_corner <- attr_cls_dual_dir %>% 
      filter(direction_group!="corner")
    attr_cls_dual_dir_non_corner <- attr_cls_dual_dir_non_corner %>% 
      group_by(attr_gid, data_gid) %>% 
      summarise(md_nc = max(md) %>% corner_split_case_dist)
    
    attr_cls_dual_dir_corner <- attr_cls_dual_dir_corner %>% 
      inner_join(attr_cls_dual_dir_non_corner,
                 by = c("attr_gid", "data_gid"))
    attr_cls_dual_dir_corner <- attr_cls_dual_dir_corner %>% 
      filter(md<md_nc)
    
    attr_cls_valid_corners1 <- attr_cls_dual_dir_corner %>% 
      distinct(attr_gid, data_gid, direction_group)
    
  }else{
    attr_cls_valid_corners1 <- attr_cls_dual_dir %>% 
      filter(direction_group == "corner") %>% 
      distinct(attr_gid, data_gid, direction_group)
  }
  
  attr_cls <- attr_cls %>% 
    group_by(attr_gid, direction_group) %>% 
    summarise(n = n(), md = min(md)) %>% 
    ungroup()
  
  attr_cls <- attr_cls %>% 
    group_by(attr_gid) %>% 
    summarise(
      dir_var = var(n), 
      most_dir_grp = direction_group[which.max(n)],
      most_near_dir_grp = direction_group[which.min(md)],
      md = min(md)
    ) %>% 
    inner_join(attr_cls %>% 
                 filter(direction_group == "corner") %>% 
                 select(attr_gid, cornerd=md), 
               by = "attr_gid") %>% 
    mutate(dir_var = ifelse(is.na(dir_var), 0, dir_var))
  
  attr_cls_valid_corners <- attr_cls %>% 
    filter(most_near_dir_grp=="corner", 
           most_dir_grp == "corner" ,
           dir_var == 0)
  
  if(strong_corners){
    # more regress method for corner
    # this may not be required
    data_gid_those_are_connected <- d_gid_att_map_min_d_t2 %>% 
      inner_join(attr_cls_valid_corners %>% select(attr_gid, direction_group = most_dir_grp),
                 by = c("attr_gid", "direction_group"))
    
    these_data_gids_avg_dist <- d_gid_att_map_min_d_t2 %>% 
      filter(data_gid %in% data_gid_those_are_connected$data_gid) %>% 
      filter(direction_group!="corner") %>% 
      group_by(data_gid) %>% 
      summarise(avg_d_dgid = mean(dist))
    
    data_gid_those_are_connected <- data_gid_those_are_connected %>% 
      inner_join(these_data_gids_avg_dist, by = "data_gid")
    
    attr_cls_valid_corners <- data_gid_those_are_connected %>% 
      group_by(attr_gid) %>% 
      summarise(avg_d = mean(avg_d_dgid)) %>% 
      inner_join(attr_cls_valid_corners, by = "attr_gid")
    
    attr_cls_valid_corners <- attr_cls_valid_corners %>% 
      filter(md < pmin(avg_d*5, 20))
  }
  
  attr_cls_valid_corners <- attr_cls_valid_corners %>% 
    select(attr_gid, direction_group = most_dir_grp)
  
  d_gid_att_map_min_d_t2_non_corner <- d_gid_att_map_min_d_t2 %>% 
    filter(direction_group!="corner")
  
  d_gid_att_map_min_d_t2_corner <- d_gid_att_map_min_d_t2 %>% 
    filter(direction_group=="corner")
  
  
  d_gid_att_map_min_d_t2_corner <- d_gid_att_map_min_d_t2_corner %>% 
    inner_join(attr_cls_valid_corners, by = c("attr_gid", "direction_group")) %>% 
    bind_rows(
      d_gid_att_map_min_d_t2_corner %>% 
        inner_join(attr_cls_valid_corners1, by = c("attr_gid","data_gid", "direction_group"))
    ) %>% 
    distinct()
  
  d_gid_att_map_min_d_t2 <- d_gid_att_map_min_d_t2_non_corner %>% 
    bind_rows(d_gid_att_map_min_d_t2_corner)
  
  d_gid_att_map_min_d_t2
  
}
