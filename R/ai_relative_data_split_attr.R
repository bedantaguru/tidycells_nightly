
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
    
    # mapping strength is higher
    rel_gids <- relative_gid_map_pattern_fix(rel_gids)

    d_att <- rel_gids %>%
      select(row, col, gid = new_attr_gid) %>%
      bind_rows(d_att) %>% 
      unique()
    

    # information kept for missing link detection
    # mbc : missed_block_connections
    mbc <- chk %>% distinct(attr_gid, data_gid)
    attr(d_att, "missed_block_connections") <- mbc
    
    common_knowledge(missed_block_connections = mbc)

    admap_new <- basic_map
    
    rel_gids <- rel_gids %>% select(-attr_gid) %>% rename(attr_gid = new_attr_gid)
    
    admap_new$raw <- admap_new$raw %>%
      anti_join(mbc, by = c("attr_gid", "data_gid")) %>% 
      bind_rows(rel_gids)
    
    admap_new <- connect_data_and_attr_groups_from_raw_map(admap_new$raw)
    
    
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
    
  }
  
  dirs <- c("N","E","W","S")
  
  allocations <- dirs %>% map(~{
    if(any(stringr::str_detect(rel_gids$direction,.x))){
      central_allocation_of_rel_gids(rel_gids %>% filter(!stringr::str_detect(direction,.x)))
    }else{
      tibble()
    }
  })
  
  names(allocations) <- paste0("no_", dirs)
  
  allocations$base0 <- central_allocation_of_rel_gids(rel_gids)
  
  # it has to be non-lossy
  # base0 will not be lossy
  allocations <- allocations %>% purrr::keep(~nrow(.x)==nrow(allocations$base0))
  
  if(length(allocations)!=1){
    
    alo_vars <- allocations %>% map_dbl(variation_in_allocations_of_rel_gids)
    
    allocations <- allocations[alo_vars==min(alo_vars)]
    
    allocations <- allocations %>% map(~.x %>% mutate(mapping_strength = 1-min(alo_vars)))
    
  }
  
  out <- allocations[[1]]
  
  # update in common knowledge about non-join possibilities of data-gids
  # if <corner> present in between in <row, col> or <col, row> seq then underlying block as non joinable
  seq1 <- out %>% arrange(row, col) %>% pull(direction_group)
  seq2 <- out %>% arrange(col, row) %>% pull(direction_group)
  seqt1 <- which(seq1=="corner")
  seqt2 <- which(seq2=="corner")
  chk <- (length(seqt1[seqt1>1 & seqt1 < length(seq1)]) > 0) | (length(seqt2[seqt2>1 & seqt2 < length(seq2)]) > 0)
  if(chk){
    common_knowledge(non_joinable = tibble(id = paste0("by_attr_id_", out$attr_gid[1]), data_gid = unique(out$data_gid)))
  }
  
  return(out)
  
}

central_allocation_of_rel_gids <- function(rel_gids){
  rel_gids %>% group_by(row, col) %>% filter(dist == min(dist)) %>% 
    ungroup() %>% 
    mutate(mapping_strength = 0.5)
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

