
# dimention analysis and raw maps
ai_get_dimention_analysis_details <- function(basic_map, d_dat, d_att, major_direction_relax = TRUE) {
  dimension_analysis <- list()

  dimension_analysis$data_gid_dim <- d_dat %>%
    group_by(gid) %>%
    summarise(
      r_dim_data = n_distinct(row),
      c_dim_data = n_distinct(col)
    ) %>% 
    rename(data_gid = gid)


  d_att_dat_map <- basic_map

  d_att_dat_map_raw <- get_data_attr_cell_wise_map_raw(basic_map, d_dat, d_att)

  # attach dimension
  dimension_analysis$attr_data_dim <- d_att_dat_map_raw %>%
    group_by(attr_gid, data_gid) %>%
    summarise(
      r_dim = row_d %>% intersect(row_a) %>% length(),
      c_dim = col_d %>% intersect(col_a) %>% length(),
      direction_group = direction_group[1]
    ) %>%
    ungroup() %>%
    inner_join(dimension_analysis$data_gid_dim, by = c("data_gid")) %>%
    mutate(rel_dim = ifelse(direction_group == "NS", c_dim / c_dim_data, r_dim / r_dim_data)) %>%
    mutate(rel_dim = ifelse(direction_group == "corner", 0, rel_dim)) %>%
    mutate(full_dim = (rel_dim >= 1))

  # in case only non full dim major (NS or WE) attr present
  chk <- dimension_analysis$attr_data_dim %>% filter(direction_group %in% c("NS","WE")) %>% pull(full_dim)
  chk <- any(!chk)
  if (major_direction_relax & chk) {
    dimension_analysis$attr_data_dim <- dimension_analysis$attr_data_dim %>%
      group_by(data_gid, direction_group) %>%
      mutate(
        is_full_dim_present = any(full_dim),
        this_attr_max_rel = (rel_dim == max(rel_dim))
      ) %>%
      ungroup() %>%
      rename(full_dim_orig = full_dim) %>%
      mutate(full_dim = ifelse(direction_group == "corner",
        full_dim_orig,
        ifelse(is_full_dim_present,
          full_dim_orig,
          this_attr_max_rel
        )
      )) %>% 
      select(-is_full_dim_present, -this_attr_max_rel, -full_dim_orig)
  }

  # fix major minor

  d_att_dat_map <- dimension_analysis$attr_data_dim %>%
    distinct(attr_gid, data_gid, full_dim) %>%
    right_join(d_att_dat_map, by = c("attr_gid", "data_gid")) %>%
    mutate(attr_group = ifelse(full_dim, "major", "minor")) %>%
    select(-full_dim)


  list(map = d_att_dat_map, dimension_analysis = dimension_analysis)
}


# helpers
# @Dev this may be kept in different file

get_data_attr_cell_wise_map_raw <- function(map, d_dat, d_att){
  map %>%
    # join with data_gid to attach all data-cells
    inner_join(d_dat %>%
                 select(row_d = row, col_d = col, data_gid = gid),
               by = "data_gid"
    ) %>%
    # join with attr_gid to attach all attr-cells
    inner_join(d_att %>%
                 select(row_a = row, col_a = col, attr_gid = gid),
               by = "attr_gid"
    )
}

