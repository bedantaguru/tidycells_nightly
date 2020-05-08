# note full_data has to be without empty types
ai_data_gid_join <- function(d_dat, d_att, data_attr_map, full_data) {
  
  
  do_data_gid_join <- !isTRUE(getOption("tidycells.analyze_cells_options")[["no_data_gid_join"]])
  
  # fd <- full_data %>% filter(type != "empty") <<-  this filter is not required as earlier ensured
  
  fd <- full_data %>% 
    select(row, col, type) %>% 
    # rc for avoiding anti_join
    mutate(rc = row+1i*col)
  
  # d_att is not giong out from here
  d_att <- d_att %>% 
    mutate(rc = row+1i*col)
  
  done <- F
  
  njdg <- common_knowledge("non_joinable_data_gid")
  if(!is.data.frame(njdg)){
    njdg <- tibble()
  }
  
  repeat({
    
    if (length(unique(d_dat$gid)) < 2 | !do_data_gid_join) break()
    
    # prune (discard) all possible data_gid joins
    
    # nearer data_gids already gets priority
    data_gid_comb <- d_dat %>% get_possible_data_gid_mergeable()
    
    if(nrow(njdg)>0){
      data_gid_comb <- data_gid_comb %>% anti_join(njdg, by = c("gid1", "gid2"))
    }
    
    data_gid_comb <- is_attachable_prune_vectorized_logic_1(data_gid_comb, data_attr_map)
    
    if(nrow(data_gid_comb)>0){
      
      d_dat0 <- d_dat %>% mutate(rc = row+1i*col)
      
      data_gid_comb <- data_gid_comb %>%
        dplyr::rowwise() %>% 
        mutate(is_attachable_gids = is_attachable(
          gid1, gid2,
          d_dat0, d_att, data_attr_map,
          whole_data = fd
        )) %>% 
        ungroup()
      
      
      if (any(data_gid_comb$is_attachable_gids)) {
        data_gid_joins <- data_gid_comb %>% 
          filter(is_attachable_gids) %>% 
          select(gid= gid1, new_gid= gid2)
        
        data_gid_joins <-  gid_map_link_tune(data_gid_joins)
        
        d_dat <- get_group_id_join_gids(d_dat, gid_map = data_gid_joins, no_need_to_tune = T)
        data_attr_map <- ai_update_admap_after_data_gid_join(data_attr_map, link_tuned_gid_map = data_gid_joins)
        if(nrow(njdg)>0){
          njdg <- update_non_joinable_data_gid(njdg, link_tuned_gid_map = data_gid_joins)
        }
        done <-  T
      } else {
        break()
      }
      
    }else{
      break()
    }
    
  })
  
  list(admap = data_attr_map, d_dat = d_dat, done = done)
}


###########
# helpers #
###########


ai_update_admap_after_data_gid_join <- function(admap, link_tuned_gid_map){
  
  admap %>%
    left_join(link_tuned_gid_map, by = c("data_gid" = "gid")) %>%
    mutate(new_gid = ifelse(is.na(new_gid), data_gid, new_gid)) %>%
    select(-data_gid) %>%
    rename(data_gid = new_gid) %>% 
    group_by(attr_gid, data_gid) %>% 
    summarise(dist = min(dist), 
              attr_group = attr_group[1], 
              mapping_strength = min(mapping_strength), 
              direction = direction[1], 
              direction_group = direction_group[1]) %>% 
    ungroup()
  
}


update_non_joinable_data_gid <- function(njdg, link_tuned_gid_map){
  if(any(link_tuned_gid_map$gid %in% c(njdg$gid1, njdg$gid2))){
    njdg %>% 
      left_join(link_tuned_gid_map, by = c("gid1"="gid")) %>% 
      mutate(gid1 = ifelse(is.na(new_gid), gid1, new_gid)) %>% 
      select(-new_gid) %>% 
      left_join(link_tuned_gid_map, by = c("gid2"="gid")) %>% 
      mutate(gid2 = ifelse(is.na(new_gid), gid2, new_gid)) %>% 
      select(-new_gid)
  }
  
  njdg
}


get_possible_data_gid_mergeable <- function(d_dat){
  
  dg_sides <- ai_get_data_attr_map(
    dat_boundary = get_group_id_boundary(d_dat), 
    # used for connection only
    att_gid_map = d_dat, 
    tune_for_table_gid_interaction = T, 
    tune_for_table_gid_interaction_mode = "data_mask")$map
  
  dg_sides <- dg_sides %>%  rename(g1 = data_gid, g2 =attr_gid) %>% filter(g1!=g2)
  
  dg_sides <- dg_sides %>% mutate(gid1 = pmin(g1, g2), gid2 = pmax(g1, g2))
  
  dg_sides %>% group_by(gid1, gid2) %>% 
    summarise(d = mean(dist), direction = direction[1]) %>% 
    ungroup()
}


