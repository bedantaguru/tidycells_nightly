


#' Get Group ID for joined cells
#'
#' @param dat the (data or attribute) cells (in at least row-col format)
#'
#' @details Used internally by get_data_block_information function
#' @keywords internal
#' @return Group ID attached information
#'
get_group_id <- function(dat, allow_corner = FALSE, gid_tag) {
  
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
    
    n_gid_now <- drc_id %>%
      summarise(n_distinct(gid)) %>%
      pull(1)
    
    if (n_gid > n_gid_now) {
      n_gid <- n_gid_now
    } else {
      break()
    }
  })
  

  drc_id <- drc_id %>% mutate(gid = as_character(gid))
  if(!missing(gid_tag)){
    drc_id <- drc_id %>% mutate(gid = paste0(gid_tag, gid))
  }
  
  drc_id <- drc_id %>% select(-rid, -cid)
  
  if(allow_corner){
    repeat({
      drc_id_new <- drc_id %>% boundary_cells()
      l1 <- drc_id_new %>% pull(gid) %>% unique() %>% length()
      l2 <- drc_id %>% pull(gid) %>% unique() %>% length()
      if(l1==l2) break()
      drc_id <- drc_id_new
    })
    
  }
  
  drc_id
}


# get boundary of group_id
# @Dev this possibly can be merged to intra block dist approx_intra_block_dist
get_group_id_boundary <- function(drc_id, flatten_it = F, need_both = F) {
  dout <- drc_id %>%
    group_by(gid) %>%
    summarise(r_min = min(row), c_min = min(col), r_max = max(row), c_max = max(col))
  if(flatten_it | need_both){
    xp1 <- dout[-1] %>% t() %>% as.data.frame() %>% map_df(~.x %>% matrix(nrow = 2, byrow = T) %>% as.data.frame() %>% expand.grid())
    xp2 <- dout$gid %>% rep(each =4)
    xp <- cbind(xp2, xp1)
    colnames(xp) <- c("gid","row","col")
    xp <- as_tibble(xp)
    if(need_both){
      return(list(normal = dout, flat = xp))
    }
    return(xp)
  }
  return(dout)
}

# for corner cells this algo may be tuned
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


# gid map need to tune for multiple maps
# thi enables it in a one go (gid merges)
gid_map_link_tune <- function(gid_map){
  grps <- list()
  
  if(nrow(gid_map) > 0){
    
    grps[[1]] <- c(gid_map$gid[1], gid_map$new_gid[1])
    
    if(nrow(gid_map) > 1){
      
      seq(2, nrow(gid_map)) %>% 
        purrr::walk(~{
          tl <- gid_map[.x, ] %>% as_character()
          mch <- grps %>% map_lgl(~(length(.x %>% intersect(tl)) >0))
          if(any(mch)){
            grps[[which(mch)[1]]] <<- c(grps[[which(mch)[1]]], tl)
          }else{
            grps[[length(grps)+1]] <<- c(tl)
          }
        })
    }
    
  }else{
    return(tibble(gid = character(0), new_gid = character(0)))
  }
  
  grps %>% map_df(~{
    tibble(gid = unique(.x), new_gid = min(.x))
  })
  
}


get_group_id_join_gids <- function(old_group_id_info, gid_map, no_need_to_tune = FALSE) {
  
  if(!no_need_to_tune){
    gid_map <- gid_map_link_tune(gid_map)
  }
  
  old_group_id_info <- old_group_id_info %>%
    left_join(gid_map, by = "gid") %>%
    mutate(new_gid = ifelse(is.na(new_gid), gid, new_gid)) %>%
    select(-gid) %>%
    rename(gid = new_gid) %>% 
    group_by(row, col) %>% 
    dplyr::summarise_all(min) %>% 
    ungroup()
  
  old_group_id_info
}
