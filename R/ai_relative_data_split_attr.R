
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
      # in case there is any non_linked_block of d_att
      bind_rows(common_knowledge("non_linked_block_d_att")) %>% 
      bind_rows(d_att) %>% 
      unique()
    
    
    # information kept for missing link detection
    # mbc : missed_block_connections
    mbc <- chk %>% distinct(attr_gid, data_gid)
    
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
  
  # explained later why missed_block_of_d_att is required
  missed_block_of_d_att <- F
  
  allocations <- list()
  
  # rev is required for convention handling
  dirs <- c("N","E","W","S") %>% rev()
  
  allocations <- dirs %>% map(~{
    if(any(stringr::str_detect(rel_gids$direction,.x))){
      central_allocation_of_rel_gids(rel_gids %>% filter(!stringr::str_detect(direction,.x)))
    }else{
      tibble()
    }
  })
  
  names(allocations) <- paste0("no_", dirs)
  
  allocations$base0 <- central_allocation_of_rel_gids(rel_gids)
  
  # remove empty dfs
  allocations <- allocations %>% map_lgl(~nrow(.x)>0) %>% allocations[.]
  
  # it has to be non-lossy
  # base0 will not be lossy but duplicacy can be there
  # this duplicacy is not good also
  acells <- rel_gids %>% select(row, col) %>% unique()
  
  # strong check which is not required in this case : kept for refernece
  # al_checks1 <- allocations %>% 
  #   map_lgl(~nrow(
  #     distinct(.x, new_attr_gid, row, col) %>% 
  #       inner_join(acells, by = c("row","col")))==nrow(acells))
  
  
  # check for perfectly fit situations
  
  al_checks1 <- allocations %>% 
    map_lgl(~nrow(distinct(.x, new_attr_gid, row, col))==nrow(acells))
  
  if(any(al_checks1)){
    allocations <- allocations[al_checks1]
  }else{
    # full dimension did not capture so missing block need to be kept for further joins if possible
    missed_block_of_d_att <- T
    # duplicacy discouraged  and will be alloted only if no other options is present
    al_checks2 <- allocations %>% 
      map_lgl(~is_conforms_to_rcdf(.x))
    if(any(al_checks2)){
      allocations <- allocations[al_checks2]
    }
  }
  
  if(length(allocations)!=1){
    # this will be touched in rare conditions
    
    alo_vars <- allocations %>% map_dbl(variation_in_allocations_of_rel_gids)
    
    allocations <- allocations[alo_vars==min(alo_vars)]
    
    allocations <- allocations %>% map(~.x %>% mutate(mapping_strength = 1-min(alo_vars)))
    
  }
  
  
  is_there_possibly_a_bha <- !isTRUE(getOption("tidycells.analyze_cells_options")[["no_bidirectional_hierarchical_attributes"]])
  
  if(is_there_possibly_a_bha & missed_block_of_d_att & length(allocations)>1){
    # this will try to recover non_linked_block_d_att
    out <- allocations %>% 
      reduce(fj, 
             join_by = c("attr_gid",   "row",   "col" ), 
             ensure_unique = T)
  }else{
    out <- allocations[[1]]
  }
  
  
  
  if(missed_block_of_d_att){
    # update info so that d_att can be made informative and complete
    chk <- rel_gids %>% 
      anti_join(out, by = c("row", "col"))
    if(nrow(chk)>0){
      common_knowledge(non_linked_block_d_att = chk %>% 
                         group_by(row, col) %>% 
                         summarise(gid = paste0("NL_", min(new_attr_gid))) %>% ungroup())
    }
  }
  
  # update in common knowledge about non-join possibilities of data-gids
  # if <corner> present in between in <row, col> or <col, row> seq then underlying block as non joinable
  seq1 <- out %>% arrange(row, col) %>% pull(direction_group)
  seq2 <- out %>% arrange(col, row) %>% pull(direction_group)
  seqt1 <- which(seq1=="corner")
  seqt2 <- which(seq2=="corner")
  chk <- (length(seqt1[seqt1>1 & seqt1 < length(seq1)]) > 0) | (length(seqt2[seqt2>1 & seqt2 < length(seq2)]) > 0)
  if(chk){
    # it is by default additive (old informations are preserved)
    # it may be termed "stale" which is used in sense that if d_dat changes or data_gid changes
    # then this will be meaningless or has to be tackled in specific ways
    bds <- unique(out$data_gid)
    
    common_knowledge(
      non_joinable_data_gid = expand.grid(
        gid1 = bds, 
        gid2 = bds, stringsAsFactors = F
      ) %>% filter(gid1!=gid2)
    )
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
  
  ref_df <- expand.grid(data_gid = unique(dgid_summary$data_gid), 
                        direction = ordinary_compass_direction_names, 
                        stringsAsFactors = F)
  
  ref_calc <-  dgid_summary %>% full_join(ref_df, by = c("data_gid", "direction")) %>% mutate(adist = ifelse(is.na(adist), 0, adist))
  calc_mat <- ref_calc %>% tidyr::pivot_wider(id_cols = data_gid, names_from = direction, values_from = adist) %>% select(-data_gid) %>% as.matrix()
  d0 <- (calc_mat-matrix(rep(colMeans(calc_mat),nrow(calc_mat)), nrow = nrow(calc_mat), byrow = T))^2 %>% sum()
  d0/ nrow(calc_mat)
}

