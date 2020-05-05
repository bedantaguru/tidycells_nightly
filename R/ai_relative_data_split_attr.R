
ai_relative_data_split_attr <- function(basic_map, d_att, d_dat_bd) {
  chk <- basic_map$map %>%
    group_by(data_gid, attr_gid) %>%
    summarise(n_dirs = n_distinct(direction)) %>%
    ungroup() %>%
    filter(n_dirs > 1)
  
  done <- FALSE
  
  if (nrow(chk) > 0) {
    # relative split required
    done <- TRUE
    
    # in earlier stage : d_dat_bd <- get_group_id_boundary(d_dat)
    # this is kept for specific case (see example file 20490do001_2016.xls@Table_1.1 with NA filled)
    # the example (similar) included in master pattern
    
    data_gids_those_are_ever_connected <- 
      basic_map$map[basic_map$map$attr_gid %in% chk$attr_gid, c("attr_gid","data_gid")] %>% 
      distinct()
    
    rel_gids_revisited <- basic_map$raw %>% 
      inner_join(data_gids_those_are_ever_connected, by = c("attr_gid", "data_gid"))
    
    # information kept for missing link detection
    # mbc : missed_block_connections
    # used later
    mbc <- data_gids_those_are_ever_connected %>% distinct(attr_gid, data_gid)
    
    rel_gids <- rel_gids_revisited
    rel_gids <- rel_gids %>%
      mutate(new_attr_gid = paste(attr_gid, data_gid, direction, sep = "_"))
    
    # mapping strength is higher
    rel_gids <- relative_gid_map_pattern_fix(rel_gids, basic_map$map, d_dat_bd)
    
    d_att <- rel_gids %>%
      select(row, col, gid = new_attr_gid) %>%
      # in case there is any non_linked_block of d_att
      bind_rows(common_knowledge("non_linked_block_d_att")) %>% 
      bind_rows(d_att) %>% 
      unique()
    
    
    # information kept for missing link detection
    # mbc : missed_block_connections
    # defined earlier
    
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


relative_gid_map_pattern_fix <- function(rel_gids, admap, d_dat_bd){
  rel_gids %>% split(.$attr_gid) %>% map_df(relative_gid_map_pattern_fix_for_a_attr_gid, 
                                            admap = admap, d_dat_bd = d_dat_bd)
}

relative_gid_map_pattern_fix_for_a_attr_gid <- function(rel_gids, admap, d_dat_bd){
  # When a single attr_gid is in both major direction group (in NS and WE)
  # first split them into NS and WE type 
  # (both should include corners in same range as duplicate) 
  # *** (but this duplicate is not serving any good) Hence
  # duplicate part is absorbed into either direction (with rel_gids_WE)
  # if any_corner is left which is out of same range will be mapped as corner
  
  chk <- rel_gids$direction_group %>% unique()
  chk <- ("NS" %in% chk) & ("WE" %in% chk)
  out_done <- F
  if(chk){
    
    d_att_dgs <- rel_gids %>% 
      filter(direction_group!="corner") %>% 
      group_by(row, col , direction_group) %>% 
      mutate(nd = n_distinct(data_gid)) %>% group_by(row, col) %>% 
      filter(nd == max(nd)) %>% 
      ungroup() %>% 
      distinct(gid = attr_gid, row, col, direction_group)
    
    
    d_att_dgs_NS <- d_att_dgs %>% filter(direction_group=="NS")
    d_att_dgs_WE <- d_att_dgs %>% filter(direction_group=="WE")
    
    if(nrow(d_att_dgs_NS) >0  & nrow(d_att_dgs_WE)>0){
      d_att_dgs_NS_encl <- get_group_id_enclosure(
        d_att_dgs_NS, enclosure_direction = "row", details = T
      )$enclosure_induced_gid_boundary
      
      d_att_dgs_WE_encl <- get_group_id_enclosure(
        d_att_dgs_WE, enclosure_direction = "col", details = T
      )$enclosure_induced_gid_boundary
      
      
      rel_gids_WE <- rel_gids %>% 
        filter(col <= d_att_dgs_WE_encl$c_max, col >= d_att_dgs_WE_encl$c_min)
      
      rel_gids_NS <- rel_gids %>% 
        filter(row <= d_att_dgs_NS_encl$r_max, row >= d_att_dgs_NS_encl$r_min) %>% 
        anti_join(rel_gids_WE, by = c("row", "col"))
      
      common_knowledge(
        attr_ori_pre_tag = rel_gids_NS %>% 
          distinct(attr_gid, row_a = row, col_a = col, ori_pre_tag = "NS") %>% 
          bind_rows(
            rel_gids_WE %>% 
              distinct(attr_gid, row_a = row, col_a = col, ori_pre_tag = "WE")
          )
      )
      
      
      
      rel_gids_rest <- rel_gids %>% 
        anti_join(rel_gids_NS, by = c("row","col")) %>% 
        anti_join(rel_gids_WE, by = c("row","col"))
      
      if(nrow(rel_gids_rest)>0){
        common_knowledge(non_linked_block_d_att = rel_gids_rest %>% 
                           group_by(row, col) %>% 
                           summarise(gid = paste0("NL_", min(new_attr_gid))) %>% 
                           ungroup())
      }
      
      out_NS <- out_WE <- NULL
      if(nrow(rel_gids_NS)>0){
        out_NS <- relative_gid_map_pattern_fix_for_a_attr_gid_in_dirgrp(rel_gids_NS, admap, d_dat_bd)
      }
      
      if(nrow(rel_gids_WE)>0){
        out_WE <- relative_gid_map_pattern_fix_for_a_attr_gid_in_dirgrp(rel_gids_WE, admap, d_dat_bd)
      }
      out <- out_NS %>% bind_rows(out_WE) %>% distinct()
      out_done <- T
    }
    
    
    
  }
  
  if(!out_done){
    out<- relative_gid_map_pattern_fix_for_a_attr_gid_in_dirgrp(rel_gids, admap, d_dat_bd)
  }
  
  return(out)
  
}

relative_gid_map_pattern_fix_for_a_attr_gid_in_dirgrp <- function(rel_gids, admap, d_dat_bd){
  
  
  rel_gids_attr_major_dir_grp <- stat_mode(rel_gids$direction_group[rel_gids$direction_group!="corner"])
  
  do_unknown_missing_values_adjustments <- !isTRUE(getOption("tidycells.analyze_cells_options")[["no_unknown_missing_values_adjustments"]])
  
  if(do_unknown_missing_values_adjustments){
    opp_grp_rc_dir <- ifelse(rel_gids_attr_major_dir_grp=="WE", "row", "col")
    
    # group by opposite side attr_gid for segregated splits
    # in this case little tweak
    data_gid_opp_grp_l <- d_dat_bd %>% 
      filter(gid %in% rel_gids$data_gid) %>% 
      get_group_id_enclosure(drc_bd = ., 
                             enclosure_direction = opp_grp_rc_dir, details = T)
    
    encl_dgb <- data_gid_opp_grp_l$enclosure_induced_gid_boundary
    # extra precaution 
    # by doing this even if method = "fast" is used in
    # central_allocation_of_rel_gids(rel_gids, method = c("accurate", "fast"))
    # it will work as expected
    if(opp_grp_rc_dir == "row"){
      encl_dgb$c_max <- max(encl_dgb$c_max)
      encl_dgb$c_min <- min(encl_dgb$c_min)
    }else{
      encl_dgb$r_max <- max(encl_dgb$r_max)
      encl_dgb$r_min <- min(encl_dgb$r_min)
    }
    
    this_d_att <- rel_gids %>% distinct(gid = attr_gid, row, col)
    
    this_raw_map <- get_raw_map_for_ai_get_data_attr_map(
      dat_boundary = encl_dgb,
      att_gid_map = this_d_att
    ) 
    
    this_raw_map <-  this_raw_map%>% 
      mutate(
        new_attr_gid = paste0("u_",attr_gid, "_", data_gid, "_", direction)
      ) 
    
    # @Dev after introducing missing value make sure the cell df with missing type gets exempted from this
    # lot of adjustments to do
    
    tidycells_pkg_env$common_knowledge$pre_check_non_joinable_data_gid <- F
    
    out_pre <- relative_gid_map_pattern_fix_for_a_attr_gid_in_opp_grp(
      this_raw_map
    )
    
    tidycells_pkg_env$common_knowledge$pre_check_non_joinable_data_gid <- T
    
    # @Dev
    # the dist adjustment required
    out_pre <- out_pre %>% rename(encl_gid = data_gid) %>% 
      inner_join(
        data_gid_opp_grp_l$enclosure_gid_map %>% 
          rename(data_gid = gid),
        by = "encl_gid") %>% 
      select(-encl_gid)
    
    out_pre_adnn <- out_pre %>% distinct(new_attr_gid, data_gid, row, col) %>% 
      inner_join(d_dat_bd, by = c("data_gid"="gid"))
    out_pre_adnn <- get_direction_df_nn(out_pre_adnn)
    out_pre_adnn <- out_pre_adnn %>% select(new_attr_gid, data_gid, row, col, dist)
    
    out <- out_pre %>% select(-dist) %>% 
      inner_join(out_pre_adnn, by = c("row", "col", "new_attr_gid", "data_gid"))
    
    
  }else{
    
    # rather simple case
    # just dire grouping
    opp_grp_rc_dir <- ifelse(rel_gids_attr_major_dir_grp=="WE", "col", "row")
    
    # group by opposite side attr_gid for segregated splits
    data_gid_opp_grp <- d_dat_bd %>% 
      filter(gid %in% rel_gids$data_gid) %>% 
      get_group_id_enclosure(drc_bd = ., 
                             enclosure_direction = opp_grp_rc_dir) %>% 
      distinct(data_gid = gid, opp_grp = enclosure)
    
    rel_gids <- rel_gids %>% inner_join(data_gid_opp_grp, by = "data_gid")
    
    out <- rel_gids %>% split(.$opp_grp) %>% map_df(relative_gid_map_pattern_fix_for_a_attr_gid_in_opp_grp)
    
    out$opp_grp <- NULL
  }
  
  
  # corner separation to make sure the corner gets selected
  # this temp modification is reverted after the required action
  out <- out %>% 
    mutate(
      direction_group = ifelse(
        direction_group == "corner",
        paste0(direction_group, "_", rel_gids_attr_major_dir_grp), 
        direction_group)
    )
  
  return(out)
  
}


relative_gid_map_pattern_fix_for_a_attr_gid_in_opp_grp <- function(rel_gids){
  
  
  # explained later why missed_block_of_d_att is required
  missed_block_of_d_att <- F
  
  
  allocations <- list()
  
  # rev is required for convention handling
  # this order is absolutely required
  dirs <- c("N","W","E","S") %>% rev()
  
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
  
  # though ideally common_knowledge("pre_check_non_joinable_data_gid") can be used
  # but direct access is bit faster
  if(!isFALSE(tidycells_pkg_env$common_knowledge$pre_check_non_joinable_data_gid)){
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
      
      # note in case of dummy gid created by enclosures the non_joinable_data_gid 
      # will not have any impact on data_gid_join
      
      common_knowledge(
        non_joinable_data_gid = expand.grid(
          gid1 = bds, 
          gid2 = bds, stringsAsFactors = F
        ) %>% filter(gid1!=gid2)
      )
    }
  }
  
  return(out)
  
}

central_allocation_of_rel_gids <- function(rel_gids, method = c("accurate", "fast")){
  method <-  match.arg(method)
  
  if(method=="fast"){
    out <- rel_gids %>% 
      group_by(row, col) %>% 
      filter(dist == min(dist)) %>% 
      ungroup() %>% 
      mutate(mapping_strength = 0.5)
  }else{
    # choose attr_gid-part near to each data_gid and direction
    m1 <- rel_gids %>% 
      group_by(data_gid, direction) %>% 
      filter(dist == min(dist)) %>% 
      ungroup()
    
    # choose rest as near to any data_gid
    # this also not 100% accurate
    m2 <- rel_gids %>% 
      anti_join(m1 %>% select(row, col), by = c("row", "col")) %>% 
      group_by(row, col) %>% 
      filter(dist == min(dist)) %>% 
      ungroup()
    
    out <- bind_rows(m1, m2) %>% 
      mutate(mapping_strength = 0.5)
  }
  
  out
  
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

