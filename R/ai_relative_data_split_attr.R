
ai_relative_data_split_attr <- function(basic_map, d_att) {
  chk <- basic_map$map %>%
    group_by(data_gid, attr_gid) %>%
    summarise(n_dirs = n_distinct(direction)) %>%
    ungroup() %>%
    filter(n_dirs > 1)

  done <- FALSE

  if (nrow(chk) > 0) {
    # relative split required
    done <- TRUE

    rel_gids <- chk %>%
      select(-n_dirs) %>%
      inner_join(basic_map$raw, by = c("data_gid", "attr_gid"))
    rel_gids <- rel_gids %>%
      mutate(new_attr_gid = paste(attr_gid, data_gid, direction, sep = "_"))
    
    # major change here 
    # Tuning point 1
    rel_gids <- relative_gid_map_pattern_fix(rel_gids)

    d_att <- rel_gids %>%
      select(row, col, gid = new_attr_gid) %>%
      bind_rows(d_att) %>% 
      unique()
    

    # information kept for missing link detection
    # mbc : missed_block_connections
    mbc <- chk %>% distinct(attr_gid, data_gid)
    attr(d_att, "missed_block_connections") <- mbc

    admap_new <- basic_map
    
    rel_gids <- rel_gids %>% select(-attr_gid) %>% rename(attr_gid = new_attr_gid)
    
    admap_new$raw <- admap_new$raw %>%
      anti_join(mbc, by = c("attr_gid", "data_gid")) %>% 
      bind_rows(rel_gids)
    admap_new$all_map <- admap_new$raw %>%
      group_by(attr_gid, data_gid, direction, direction_group) %>%
      summarise(dist = min(dist)) %>%
      ungroup()
    admap_new$map <- admap_new$all_map %>% 
      group_by(data_gid, direction_group) %>%
      filter(dist == min(dist)) %>%
      ungroup() 
    
  } else {
    admap_new <- basic_map
  }

  list(done = done, d_att = d_att, admap = admap_new)
}


# helpers


relative_gid_map_pattern_fix <- function(rel_gids){
  rel_gids %>% split(.$attr_gid) %>% map_df(relative_gid_map_pattern_fix_for_a_attr_gid)
}

relative_gid_map_pattern_fix_for_a_attr_gid <- function(rel_gids){
  
  allocations <- list()
  
  for(this_dir in c("N","E","W","S")){
    if(any(stringr::str_detect(rel_gids$direction,this_dir))){
      allocations[[paste0("no_",this_dir)]] <- central_allocation_of_rel_gids(rel_gids %>% filter(!stringr::str_detect(direction,this_dir)))
    }
  }
  
  allocations$base0 <- central_allocation_of_rel_gids(rel_gids)
  
  allocations <- allocations %>% purrr::keep(~nrow(.x)==nrow(allocations$base0))
  
  if(length(allocations)==1){
    return(allocations[[1]])
  }
  
  alo_vars <- allocations %>% map_dbl(variation_in_allocations_of_rel_gids)
  
  allocations <- allocations[alo_vars==min(alo_vars)]
  
  return(allocations[[1]])
  
}

central_allocation_of_rel_gids <- function(rel_gids){
  rel_gids %>% group_by(row, col) %>% filter(dist == min(dist)) %>% ungroup()
}

variation_in_allocations_of_rel_gids <- function(alo){
  if(length(unique(alo$data_gid))<=1) return(0)
  
  dgid_summary <- alo %>% group_by(data_gid, direction) %>% summarise(adist = mean(dist)) %>% ungroup()
  
  ref_df <- expand.grid(data_gid = unique(dgid_summary$data_gid), direction = c("N","NW","W","SW","S","SE","E","NE"), stringsAsFactors = F)
  
  ref_calc <-  dgid_summary %>% full_join(ref_df, by = c("data_gid", "direction")) %>% mutate(adist = ifelse(is.na(adist), 0, adist))
  calc_mat <- ref_calc %>% tidyr::pivot_wider(id_cols = data_gid, names_from = direction, values_from = adist) %>% select(-data_gid) %>% as.matrix()
  d0 <- (calc_mat-matrix(rep(colMeans(calc_mat),2), nrow = nrow(calc_mat), byrow = T))^2 %>% sum()
  d0/ nrow(calc_mat)
}

