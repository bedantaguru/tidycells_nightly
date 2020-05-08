


possible_table_gid_interaction <- function(d_dat, d_att){
  do_this <- F
  this_d <- NULL
  # link gid and analysis connection
  if(!isTRUE(tidycells_pkg_env$common_knowledge$dummy_gid)){
    this_d <- tidycells_pkg_env$common_knowledge$this_cell_df
    if(is.data.frame(this_d)){
      if(hasName(this_d,"gid")){
        if(length(unique(this_d$gid))>1){
          do_this <- T
        }
      }
    }
  }
  
  if(do_this){
    # this_d should be defined in this case
    this_d <- this_d %>% as_tibble() %>%  distinct(table_gid = gid, row, col)
    common_knowledge(table_gid_impacts = T)
    dd <- d_dat %>% 
      left_join(this_d, by = c("row", "col"))
    da <- d_att %>% 
      left_join(this_d, by = c("row", "col"))
    
    common_knowledge(d_table_gid = this_d, add = F)
    common_knowledge(d_dat_table_gid_impact = dd, add = F)
    common_knowledge(d_att_table_gid_impact = da, add = F)
  }
  
  invisible(0)
}


tune_admap_for_possible_table_gid_interaction<- function(admap){
  # admap can be raw or partial or compact
  # @Dev check for other use cases
  if(isTRUE(tidycells_pkg_env$common_knowledge$table_gid_impacts)){
    
    d_dat_table_gid_impact <- tidycells_pkg_env$common_knowledge$d_dat_table_gid_impact
    d_att_table_gid_impact <- tidycells_pkg_env$common_knowledge$d_att_table_gid_impact
    
    admap_chk <- admap %>% 
      inner_join(d_dat_table_gid_impact %>% 
                   distinct(data_gid = gid, table_gid_data = table_gid), 
                 by = "data_gid") %>% 
      inner_join(d_att_table_gid_impact %>% 
                   distinct(attr_gid = gid, table_gid_attr = table_gid), 
                 by = "attr_gid")
    admap_chk <- admap_chk %>% filter(table_gid_data==table_gid_attr)
    admap <- admap_chk[colnames(admap)]
  }
  admap
}

