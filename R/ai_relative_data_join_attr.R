

ai_relative_data_join_attr <- function(admap_main, d_att) {
  chk <- admap_main %>%
    group_by(data_gid, direction, attr_group) %>%
    mutate(n_att = n_distinct(attr_gid)) %>%
    ungroup() %>%
    filter(n_att > 1)

  done <- FALSE

  if (nrow(chk) > 0) {
    # relative join required
    done <- TRUE

    rel_gids <- chk %>%
      select(-n_att) 

    admap_main_rest <- admap_main %>%
      anti_join(chk, by = c("attr_gid", "data_gid", "direction", "attr_group"))

    rel_gids <- rel_gids %>%
      group_by(data_gid, direction, attr_group) %>%
      mutate(new_attr_gid = paste(min(attr_gid), data_gid, direction, sep = "_")) %>%
      ungroup()

    rel_gids <- rel_gids %>%
      group_by(new_attr_gid, data_gid) %>%
      mutate(
        # this is possibly not required anymore as attr_group was in grouping vars
        new_attr_group = ifelse(any(attr_group == "major"), "major", "minor"),
        new_dist = min(dist)
      ) %>%
      ungroup()

    admap_main <- rel_gids %>%
      select(-attr_group, -attr_gid, -dist) %>%
      rename(attr_gid = new_attr_gid, attr_group = new_attr_group, dist = new_dist) %>%
      distinct() %>% 
      bind_rows(admap_main_rest) %>% 
      distinct()

    
    new_part_of_d_att <- rel_gids %>% 
      distinct(gid = attr_gid, new_attr_gid) %>% 
      left_join(d_att, by = "gid") %>% 
      select(-gid) %>% 
      rename(gid = new_attr_gid)

    # update d_att 
    # (addition as potentilly these aids can get connected to others where duplicate has not arrived)
    
    d_att <- d_att %>%
      bind_rows(new_part_of_d_att)
    
    chk_this <- chk %>% distinct(attr_gid, data_gid)

    common_knowledge(missed_block_connections = chk_this)
    
  }

  list(done = done, d_att = d_att, admap = admap_main)
}
