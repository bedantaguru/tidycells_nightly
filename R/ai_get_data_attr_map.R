
ai_get_data_attr_map <- function(dat_boundary,
                                 att_gid_map,
                                 attr_to_near_data = FALSE, leave_inside = FALSE) {

  # check relative location of each attr_gid (gid) wrt each data_gid
  d_att_map <- dat_boundary %>%
    split(.$gid) %>%
    map_df(~ get_direction_df(.x, datt = att_gid_map, allow_inside = leave_inside)) %>%
    rename(attr_gid = gid) %>% 
    mutate(mapping_strength = 0)


  #  connect data and attr gids based on above raw map
  connect_data_and_attr_groups_from_raw_map(d_att_map, attr_to_near_data = attr_to_near_data, leave_inside = leave_inside)
  
}

# helpers
connect_data_and_attr_groups_from_raw_map <- function(raw_map,
                                                      attr_to_near_data = FALSE, leave_inside = FALSE){
  
  # for each attr_gid (gid), data_gid, direction, direction_group :- get minimum distance
  # all possible maps
  d_gid_att_map <- raw_map %>%
    group_by(attr_gid, data_gid, direction, direction_group) %>%
    summarise(dist = min(dist), mapping_strength = max(mapping_strength)) %>%
    ungroup() 
  
  # attach nearest attr_gid to each data gid
  # by default each data gid will choose nearest attr_gid (multiple allowed)
  d_gid_att_map_min_d <- d_gid_att_map %>%
    group_by(data_gid, direction_group) %>%
    filter(dist == min(dist)) %>%
    ungroup() 
  
  
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
