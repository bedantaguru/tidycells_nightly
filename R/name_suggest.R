


###########
# helpers #
###########

# info_gid plays a major role 
# need to consider correctly
name_suggest_part <- function(composed_col_name, ca, for_attr_micro_gid){
  
  dam <- ca$details$data_attr_map_raw
  
  if(missing(for_attr_micro_gid)){
    # fetch name
    cn_map <- dam %>% distinct(attr_var_sync_name, attr_micro_gid)
    for_attr_micro_gid <- cn_map$attr_micro_gid[cn_map$attr_var_sync_name %in% composed_col_name]
  }
  
  all_attrs <- dam %>% 
    select(info_gid, attr_gid, attr_micro_gid, row = row_a, col = col_a) %>% unique()
  all_attrs <- all_attrs %>% 
    left_join(ca$cell_df[c("row","col","value")], by = c("row","col"))
  
  this_attrs <- all_attrs %>% filter(attr_micro_gid %in% for_attr_micro_gid)
  # from same info_gid
  rest_attrs <- all_attrs %>% 
    filter(info_gid %in% this_attrs$info_gid) %>% 
    anti_join(this_attrs, by = c("row", "col"))
  t_to_r_map <- ai_get_data_attr_map(
    dat_boundary = this_attrs %>% 
      select(row, col, gid = info_gid) %>% 
      get_group_id_boundary(),
    att_gid_map = rest_attrs %>% select(row, col, gid = attr_micro_gid), 
    attr_to_near_data = T
  )
  
  connected_attrs <- rest_attrs %>% 
    inner_join(t_to_r_map$map %>% 
                 select(attr_micro_gid = attr_gid, info_gid = data_gid, direction, direction_group, dist),
               by = c("info_gid","attr_micro_gid")) %>% 
    inner_join(this_attrs %>% 
                 distinct(info_gid, attr_gid), 
               by = "info_gid", suffix = c("" ,"_this"))
  
  
  connected_attrs <- connected_attrs %>% 
    group_by(info_gid, attr_micro_gid) %>% 
    mutate(n_cells = n(), n_distinct_cells = n_distinct(value)) %>% 
    ungroup()
  
  dir_ranks <- dam %>% 
    filter(info_gid %in% this_attrs$info_gid) %>% 
    distinct(attr_micro_gid, direction = direction_basic, data_gid) %>% 
    group_by(direction) %>% 
    summarise(n_dir_in_dam = n()) %>% 
    mutate(n_dir_in_dam = n_dir_in_dam/max(n_dir_in_dam)*4)
  
  connected_attrs <- connected_attrs %>% 
    left_join(dir_ranks, by = "direction")
  
  connected_attrs <- connected_attrs %>% 
    mutate(n_dir_in_dam = ifelse(is.na(n_dir_in_dam), 0, n_dir_in_dam))
  
  connected_attrs <- connected_attrs %>% 
    mutate(
      # score for selection
      score = 
        # if they align to majority of overall directions
        n_dir_in_dam +
        # is they both belongs to same mother attr gets more priority
        ifelse(attr_gid==attr_gid_this, 2, 0) +
        # less dist is better
        1/(dist+1) + 
        # corner gets less value
        ifelse(direction=="corner",0,1) +
        # less n_cells gets more preference similarly less n_distinct_cells
        1/(n_cells+1)+1/(n_distinct_cells+1),
      
      # score for selection certainty (whether the name is inferable from data)
      certainty_score = 
        (n_dir_in_dam>0)+
        (attr_gid==attr_gid_this)+
        (n_cells == 1)
    )
  
  connected_attrs
  
}


