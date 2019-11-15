


#' Get Group ID for joined cells
#'
#' @param dat the (data or attribute) cells (in at least row-col format)
#'
#' @details Used internally by get_data_block_information function
#' @keywords internal
#' @return Group ID attached information in a list
#'
get_group_id <- function(dat, no_group_boundary = FALSE, allow_corner = FALSE) {
  
  if(allow_corner){
    if(!hasName(dat, "gid")){
      abort("'gid' is required")
    }
  }
  
  if(allow_corner){
    dat_a <- as_tibble(dat) %>%
      select(row, col, gid)
    dat_a <- boundary_cells(dat_a)
  }else{
    dat_a <- as_tibble(dat) %>%
      select(row, col)
  }
  
  digi_sep <- dat_a %>%
    summarise(rm = max(row), cm = max(col)) %>%
    map(~ log(.x, base = 10) %>% ceiling()) %>%
    unlist() %>%
    max()
  digi_sep <- digi_sep + 1
  
  # attach r_id, c_id
  dat_a %>%
    arrange(row, col) %>%
    group_by(row) %>%
    mutate(cid = (c(1, diff(col)) != 1) %>% cumsum()) %>%
    ungroup() %>%
    mutate(cid = cid + row * 10^digi_sep) %>%
    arrange(col, row) %>%
    group_by(col) %>%
    mutate(rid = (c(1, diff(row)) != 1) %>% cumsum()) %>%
    ungroup() %>%
    mutate(rid = rid + col * 10^digi_sep) -> drc_id
  
  # attach g_id
  drc_id <- drc_id %>% mutate(gid = rid)
  
  n_gid <- drc_id %>%
    summarise(n_distinct(gid)) %>%
    pull(1)
  
  repeat ({
    drc_id %>%
      group_by(cid) %>%
      mutate(gid = min(gid)) %>%
      group_by(rid) %>%
      mutate(gid = min(gid)) %>%
      ungroup() -> drc_id
    
    if (n_gid > (drc_id %>% summarise(n_distinct(gid)) %>% pull(1))) {
      n_gid <- drc_id %>%
        summarise(n_distinct(gid)) %>%
        pull(1)
    } else {
      break()
    }
  })
  
  drc_id <- drc_id %>% mutate(gid = as.character(gid))
  
  if(allow_corner){
    drc_id <- drc_id %>% inner_join(dat %>% select(row, col), by = c("row", "col"))
  }
  
  if(no_group_boundary){
    return(list(group_id_map = drc_id))
  }
  # boundary
  drc_boundary <- get_group_id_boundary(drc_id)
  
  list(group_id_map = drc_id, group_id_boundary = drc_boundary)
}


get_group_id_boundary <- function(drc_id) {
  drc_id %>%
    group_by(gid) %>%
    summarise(r_min = min(row), c_min = min(col), r_max = max(row), c_max = max(col))
}


boundary_cells <- function(dat){
  bdr <- dat %>% split(.$gid) %>% map_df(boundary_cells_part)
  bdr <- bdr %>% filter(row>0, col>0, row <= max(dat$row), col <=max(dat$col)) %>% unique()
  bdr
}

boundary_cells_part <- function(dg){
  mr <- min(dg$row)
  Mr <- max(dg$row)
  mc <- min(dg$col)
  Mc <- max(dg$col)
  bind_rows(
    tibble(row = mr-1, col = (mc-1):(Mc+1)),
    tibble(row = Mr+1, col = (mc-1):(Mc+1)),
    tibble(row = mr:Mr, col = mc-1),
    tibble(row = mr:Mr, col = Mc+1)
  ) %>% bind_rows(dg) %>% select(row, col)
}


get_group_id_join_gids <- function(old_group_id_info, gid_map, no_group_boundary = FALSE) {
  old_group_id_info$group_id_map <- old_group_id_info$group_id_map %>%
    left_join(gid_map, by = "gid") %>%
    mutate(new_gid = ifelse(is.na(new_gid), gid, new_gid)) %>%
    select(-gid) %>%
    rename(gid = new_gid)
  
  if(no_group_boundary){
    return(old_group_id_info)
  }
  old_group_id_info$group_id_boundary <- get_group_id_boundary(old_group_id_info$group_id_map)
  old_group_id_info
}
