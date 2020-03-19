
#@Dev write doc
# this assigns a name suitable for staking in later stage
ai_get_sync_names_for_attr_gid_splits <- function(admap_cellwise_raw){
  
  admap_split <- admap_cellwise_raw %>% 
    group_by(data_gid, attr_gid, direction, attr_gid_split) %>%
    group_split() 
  
}