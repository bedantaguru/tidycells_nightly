
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
    filter(direction_group != "corner") %>%
    ai_get_dimention_analysis_details(d_dat, d_att)

  # @Dev
  #  why comapting required?
  admap1_major_compact <- admap1_major_minor$map %>%
    filter(attr_group == "major") %>%
    group_by(data_gid, direction_group) %>%
    filter(dist == min(dist)) %>%
    ungroup()
    
  admap1_major_compact <- admap1_major_compact %>%
    # @Dev this filter seems redundant
    # filter(direction_group != "corner") %>%
    unique() %>%
    select(-attr_group)

  
  # dimension analysis done here (major minor classification)
  # phase 2 
  admap1 <- admap1_major_compact %>%
    ai_get_dimention_analysis_details(d_dat, d_att)

  ##########################
  # @Dev why do we need two phase??
  # really reaquired ? if so why two why not iterative ?? Check it..
  if(!identical(admap1_major_minor, admap1)){
    cat("you need to see it now!!\n")
    browser()
    # this is the situation
  }
  ##########################
  
  
  # @Dev
  # d_dat does not change 
  list(admap = admap1, d_dat = d_dat, d_att = d_att)
}
