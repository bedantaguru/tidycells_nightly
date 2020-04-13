

ai_attach_direction <- function(admap_cellwise_raw) {

  # asp: attr split
  admap_cellwise_raw_asp <- ai_attr_gid_micro_splits(admap_cellwise_raw)
  
  do_hierarchical_reallocation <- !isTRUE(getOption("tidycells.analyze_cells_options")[["no_hierarchical_reallocation"]])
  
  if(do_hierarchical_reallocation){
    admap_cellwise_raw_asp <- ai_hierarchical_reallocation(admap_cellwise_raw_asp)
  }
  
  admap_cellwise_raw_asp %>%
    group_by(data_gid, attr_micro_gid) %>%
    group_split() %>%
    map_df(~ .x %>% mutate(direction = get_direction(.x)))
}

# helpers
ai_attr_gid_micro_splits <- function(admap_cellwise_raw){
  # asp: attr split
  admap_cellwise_raw_asp <- admap_cellwise_raw %>%
    # kept for tracking
    mutate(direction_basic = direction) %>%
    mutate(attr_gid_split = ifelse(direction_group == "NS", paste0(row_a,":0"),
                                   ifelse(direction_group == "WE", paste0("0:", col_a), 
                                          ifelse(direction_group == "corner", paste0(row_a, ":", col_a), 0))
    ))
  
  ai_get_sync_names_for_attr_gid_splits(admap_cellwise_raw_asp)
  
}



#@Dev write doc
# this assigns a name suitable for staking attributes across data blocks in later stage
ai_get_sync_names_for_attr_gid_splits <- function(admap_cellwise_raw){
  # @Dev
  # this can be avoided in case of single data block
  
  
  admap_cellwise_raw <- admap_cellwise_raw %>% 
    mutate(attr_micro_gid = paste(attr_gid, direction, attr_gid_split, sep = "_"))
  
  admap_split <- admap_cellwise_raw %>% 
    group_by(data_gid) %>% 
    group_split() 
  
  sync_name_map <- admap_split %>% 
    map_df(~{
      dg <- .x %>% distinct(gid = data_gid, row = row_d, col = col_d)
      ag <- .x %>% distinct(gid = attr_micro_gid, row = row_a, col = col_a)
      dag <- get_direction_df(get_group_id_boundary(dg), ag)
      
      dagmap <- dag %>% 
        group_by(gid) %>% 
        summarise(dist = min(dist), direction = stat_mode(direction), data_gid = data_gid[1])
      
      # @Dev
      # check dist_order
      # it should not create problems later
      dagmap %>% 
        group_by(direction) %>% 
        mutate(dist_ord = dist %>% as.factor() %>% as.numeric(),
               attr_var_sync_name = paste0(direction, "_",dist_ord)) %>% 
        ungroup() %>% 
        distinct(attr_micro_gid = gid, data_gid, dist_order = dist_ord, attr_var_sync_name) 
        
    })
  
  new_map <- admap_cellwise_raw %>% left_join(sync_name_map, by = c("data_gid", "attr_micro_gid"))
  
  # fix the names at global level
  # each attr_micro_gid should get a single attr_var_sync_name
  chk <- new_map %>% distinct(attr_micro_gid, attr_var_sync_name)
  issue_attr_micro_gid <- 
    chk %>% 
    group_by(attr_micro_gid) %>% 
    count() %>% 
    filter(n>1) %>% 
    pull(attr_micro_gid)
  
  if(length(issue_attr_micro_gid)>0){
    fix <- chk %>% 
      filter(attr_micro_gid %in% issue_attr_micro_gid) %>% 
      group_by(attr_micro_gid) %>% 
      summarise(new_attr_var_sync_name = max(attr_var_sync_name))
    
    new_map <- new_map %>% left_join(fix, by = "attr_micro_gid")
    new_map <- new_map %>% 
      mutate(attr_var_sync_name = 
               ifelse(is.na(new_attr_var_sync_name), 
                      attr_var_sync_name, new_attr_var_sync_name)) %>% 
      select(-new_attr_var_sync_name)
    
  }
  
  new_map
}

