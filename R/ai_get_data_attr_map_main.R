
ai_get_data_attr_map_main <- function(d_dat, d_att) {

  #  start with simple attr data map
  admap0 <- ai_get_data_attr_map(
    dat_boundary = get_group_id_boundary(d_dat),
    att_gid_map = d_att
  )


  # split attr gid relative to data_gid
  rel_chk <- ai_relative_data_split_attr(basic_map = admap0, d_att = d_att)
  if (rel_chk$done) {
    d_att <- rel_chk$d_att
    admap0 <- rel_chk$admap
  }

  # dimension analysis done here (major minor classification) 
  # phase 1
  admap1_major_minor <- admap0$all_map %>%
    filter(mapping_strength>0 | direction_group != "corner") %>%
    ai_get_dimention_analysis_details(d_dat, d_att)

  # as we started with all_map (yet near data to attr map is not done)
  # this is equivalent to admap0$map
  admap1 <- admap1_major_minor$map %>%
    filter(mapping_strength>0 | attr_group != "minor") %>%
    group_by(data_gid, direction_group) %>%
    filter(dist == min(dist)) %>%
    ungroup()

  ##########################
  
  
  list(admap = admap1, d_att = d_att)
}
