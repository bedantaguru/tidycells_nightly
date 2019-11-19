


#' Get Group ID for joined cells
#'
#' @param dat the (data or attribute) cells (in at least row-col format)
#'
#' @details Used internally by get_data_block_information function
#' @keywords internal
#' @return Group ID attached information in a list
#'
get_group_id <- function(dat, no_group_boundary = FALSE, allow_corner = FALSE) {
  
  dat_a <- as_tibble(dat) %>%
    select(row, col)
  
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
    repeat({
      drc_id_new <- drc_id %>% boundary_cells()
      l1 <- drc_id_new %>% pull(gid) %>% unique() %>% length()
      l2 <- drc_id %>% pull(gid) %>% unique() %>% length()
      if(l1==l2) break()
      drc_id <- drc_id_new
    })
    
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
  bdr <- dat %>% split(.$gid) %>% map_df(~{
    dpart <- .x %>% boundary_cells_part() %>% mutate(gid = .x$gid[1]) %>% 
      inner_join(dat, by = c("row", "col"), suffix = c("_1","_2"))
    dat %>% filter(gid %in% c(dpart$gid_1, dpart$gid_2)) %>% mutate(gid = min(gid))
  })
  bdr <- bdr %>% filter(row>0, col>0, row <= max(dat$row), col <=max(dat$col)) %>% unique() 
  bdr <- bdr %>% inner_join(dat %>% select(row, col), by = c("row", "col"))
  bdr %>% group_by(row, col) %>% summarise(gid = min(gid)) %>% ungroup
}

# corner cells
boundary_cells_part <- function(dg){
  d <- dg %>% distinct(row, col)
  list(
    d, 
    d %>% mutate(row=row-1, col = col-1),
    d %>% mutate(row=row-1, col = col+1),
    d %>% mutate(row=row+1, col = col-1),
    d %>% mutate(row=row+1, col = col+1)
  ) %>% 
    bind_rows() %>% 
    filter(row >0, col >0) %>% 
    distinct(row, col)
  
}


get_group_id_join_gids <- function(old_group_id_info, gid_map, no_group_boundary = FALSE) {
  old_group_id_info$group_id_map <- old_group_id_info$group_id_map %>%
    left_join(gid_map, by = "gid") %>%
    mutate(new_gid = ifelse(is.na(new_gid), gid, new_gid)) %>%
    select(-gid) %>%
    rename(gid = new_gid) %>% 
    group_by(row, col) %>% 
    dplyr::summarise_all(min) %>% 
    ungroup()
  
  if(no_group_boundary){
    return(old_group_id_info)
  }
  old_group_id_info$group_id_boundary <- get_group_id_boundary(old_group_id_info$group_id_map)
  old_group_id_info
}
