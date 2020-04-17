

# @Dev
# migrate to other file if required
# this is mostly required in analyze cells

refresh_stale_info_non_joinable_data_gid <- function(d_dat){
  
  # if any stale_non_joinable_data_gid information is present
  if(is_common_knowledge("stale_non_joinable_data_gid")){
    stale_non_joinable_data_gid_info <- common_knowledge("stale_non_joinable_data_gid") 
  }else{
    stale_non_joinable_data_gid_info <- tibble()
  }
  
  if(nrow(stale_non_joinable_data_gid_info)>0){
    d_dat_smpl <- d_dat %>% group_by(gid) %>% summarise(row_d = row[1], col_d = col[1])
    non_joinable_data_gid_info <- stale_non_joinable_data_gid_info %>% 
      inner_join(d_dat_smpl, by=c("data_gid"="gid")) %>% 
      select(-data_gid) %>% 
      distinct()
    if(nrow(non_joinable_data_gid_info)>0){
      common_knowledge(non_joinable_data_gid = non_joinable_data_gid_info)
    }
  }
  
  invisible(0)
}



