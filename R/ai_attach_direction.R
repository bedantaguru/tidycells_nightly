

ai_attach_direction <- function(admap_cellwise_raw) {

  # asp: attr split
  admap_cellwise_raw_asp <- ai_attr_gid_micro_splits(admap_cellwise_raw)
  
  admap_cellwise_raw_asp %>%
    group_by(data_gid, attr_gid, direction, attr_gid_split) %>%
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
  
}