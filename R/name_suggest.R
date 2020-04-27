
name_suggest <- function(dat, ca){
  UseMethod("name_suggest")
}

### class specific rules ######
name_suggest.composed_df <- function(dat, ca){
  
  dat <- prune(dat)
  
  dam <- ca$details$data_attr_map_raw
  
  daname_map <- dam %>% 
    select(attr_micro_gid, name = attr_var_sync_name) %>% 
    unique()
  
  c_map_need <- daname_map %>% filter(name %in% colnames(dat))
  
  name_suggest_part_1(dat, c_map_need, ca)
}

name_suggest.column_collated_df <- function(dat, ca){
  
  dat <- prune(dat)
  
  cc_map <- attr(dat, "collate_columns_map")
  
  # 1 level of processing required here
  
  dam <- ca$details$data_attr_map_raw
  
  daname_map <- dam %>% 
    select(data_block = natural_gid, attr_micro_gid, old_name = attr_var_sync_name) %>% 
    unique()
  
  
  name_needs <- cc_map$new_name %>% intersect(colnames(dat))
  
  cc_map_need <- cc_map %>% filter(new_name %in% name_needs)
  
  cc_map_need <- cc_map_need %>% left_join(daname_map, by = c("old_name", "data_block"))
  
  block_attr_micro_gid <- cc_map_need$attr_micro_gid %>% unique()
  
  cc_map_need <- cc_map_need %>% select(name = new_name, attr_micro_gid) %>% unique()
  
  name_suggest_part_1(dat, cc_map_need, ca)
  
}

###########
# helpers #
###########

# info_gid plays a major role 
# need to consider correctly

name_suggest_part_1 <- function(dat, col_name_map, ca){
  
  # col_name_map should have name (should be colnames of dat) and
  # attr_micro_gid in wide mode
  
  nsp <- unique(col_name_map$name) %>% 
    map_df(~{
      
      name_suggest_part_2(
        for_attr_micro_gid = col_name_map %>% filter(name==.x) %>% pull(attr_micro_gid),
        ca = ca) %>% mutate(cname = .x)
    })
  
  nsp <- nsp %>% filter(certainty_score>0)
  
  nsp <- nsp %>% 
    group_by(str_sig) %>% 
    filter(score*certainty_score==max(score*certainty_score))
  
  nsp<- nsp %>% 
    group_by(cname) %>% 
    filter(score*certainty_score==max(score*certainty_score)) %>% 
    ungroup()
  
  # final map
  
  
  nsp_final <- nsp %>% 
    mutate(new_cname = values %>% map_chr(LCS_if)) %>% 
    distinct(cname , new_cname)
  
  rest_cols <- tibble(cname = setdiff(col_name_map$name, nsp_final$cname))
  
  if(nrow(rest_cols)>0){
    rest_cols <- rest_cols %>% 
      mutate(new_cname = cname %>% map_chr(~{LCS_if(dat[[.x]])}))
    rest_cols <- rest_cols %>% filter(nchar(new_cname)>0)
    nsp_final <- nsp_final %>% bind_rows(rest_cols)
  }
  
  dat <- rename_base(dat, old_names = nsp_final$cname, new_names = nsp_final$new_cname)
  
  dat
}

name_suggest_part_2 <- function(for_attr_micro_gid, 
                              omit_attr_micro_gid = NULL, 
                              ca, 
                              composed_col_name = NULL){
  
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
    anti_join(this_attrs, by = c("row", "col")) %>% 
    filter(!(attr_micro_gid %in% omit_attr_micro_gid))
  t_to_r_map <- ai_get_data_attr_map(
    dat_boundary = this_attrs %>% 
      select(row, col, gid = info_gid) %>% 
      get_group_id_boundary(),
    att_gid_map = rest_attrs %>% select(row, col, gid = attr_micro_gid)
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
    distinct(attr_micro_gid, direction, data_gid) %>% 
    group_by(direction) %>% 
    summarise(n_dir_in_dam = n()) %>% 
    mutate(n_dir_in_dam = n_dir_in_dam/(max(n_dir_in_dam)+0.00001)*4)
  
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
  
  connected_attrs <- connected_attrs %>% 
    mutate(str_sig = clean_string(value)) 
  
  connected_attrs <- connected_attrs %>% 
    group_by(attr_micro_gid) %>% 
    summarise(score = mean(score), 
              certainty_score = mean(certainty_score),
              values = value %>% 
                string_signature() %>% 
                pull(trim_name) %>% 
                list(),
              str_sig = str_sig %>% 
                string_signature() %>% 
                pull(name_signature) %>% 
                sort() %>% 
                paste0(collapse = "::"))
  
  connected_attrs %>% 
    group_by(str_sig) %>% 
    summarise(score = mean(score), 
              attr_micro_gids = attr_micro_gid %>% 
                unique() %>% 
                list(),
              certainty_score = mean(certainty_score),
              values = values %>% 
                unlist() %>% 
                string_signature() %>% 
                pull(trim_name) %>% 
                list())
  
}


LCS_if <- function(x){
  
  if(length(x)>1){
    lcs_x <- LCS(x) %>% stringr::str_trim()
    if(nchar(lcs_x)==0){
      return(paste0(unique(x), collapse = "_"))
    }else{
      return(lcs_x)
    }
  }
  
  return(x)
}


