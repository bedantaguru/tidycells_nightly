


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
gid_map_link_tune <- function(gid_map, algo = c("vect","exact")){
  algo <- match.arg(algo)
  
  if(algo=="exact"){
    
    repeat({
      
      gid_map_new<- gid_map_link_tune_single_iter(gid_map)
      if(identical(gid_map$new_gid, gid_map_new$new_gid)){
        break()
      }
      gid_map <- gid_map_new
    })
    
    return(gid_map)
    
  }else{
    
    ngm <- gid_map %>% 
      mutate(g = pmax(gid, new_gid), ng = pmin(gid, new_gid)) %>% 
      distinct(gid = g, new_gid = ng) %>% 
      filter(gid>new_gid) 
    
    ngm <- gid_map_link_tune_re_norm(ngm)
    
    repeat({
      ngml <- gid_map_link_tune_level_down(ngm)
      if(ngml$further){
        ngm <- ngml$ngm_now
      }else{
        break()
      }
    })
    
    return(ngm)
    
  }
  
}

gid_map_link_tune_re_norm <- function(ngm){
  ngm %>% 
    split(.$gid) %>% 
    map_df(~tibble(gid = c(.x$gid[1], .x$new_gid)) %>% 
             mutate(new_gid= min(gid))) %>% 
    distinct()
}

gid_map_link_tune_level_down <- function(ngm){
  
  ngm2 <- ngm %>% 
    left_join(ngm, by = c("new_gid"="gid"), suffix = c("", "2")) %>% 
    mutate(new_gid2 = ifelse(is.na(new_gid2), new_gid, new_gid2)) %>% 
    filter(new_gid2<=new_gid) 
  
  ngm2 <- ngm2 %>% distinct(gid, new_gid = new_gid2)
  
  ngm2 <- gid_map_link_tune_re_norm(ngm2)
  
  c1 <- ngm$new_gid %>% unique() %>% sort()
  c2 <- ngm2$new_gid %>% unique() %>% sort()
  
  if(identical(c1,c2)){
    outt <- F
  }else{
    outt <- T
  }
  
  list(further = outt, ngm_now = ngm2)
}


gid_map_link_tune_single_iter <- function(gid_map){
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



get_group_id_enclosure <- function(drc_id, drc_bd, enclosure_direction = c("row","col"), details = F){
  
  enclosure_direction <- match.arg(enclosure_direction)
  
  if(missing(drc_bd)){
    drc_bd<- get_group_id_boundary(drc_id)
  }
  
  # group id enclosure (row or col wise)
  
  if(enclosure_direction=="row"){
    drc_id_bd <- drc_bd %>% 
      # er:enclosed_range
      mutate(er_min = r_min, er_max = r_max) %>% 
      select(gid, er_min, er_max)
  }else{
    drc_id_bd <- drc_bd %>% 
      # er:enclosed_range
      mutate(er_min = c_min, er_max = c_max) %>% 
      select(gid, er_min, er_max)
  }
  
  
  drc_id_bd$is_changed <- F
  
  if(nrow(drc_id_bd)>1){
    
    repeat({
      
      drc_id_bd$er_len <- drc_id_bd$er_max- drc_id_bd$er_min+1
      drc_id_bd <- drc_id_bd[order(drc_id_bd$er_len, decreasing = T),]
      
      updated_row <- rep(F, nrow(drc_id_bd))
      
      for(i in 1:nrow(drc_id_bd)){
        if(!drc_id_bd$is_changed[i]){
          er_expnd <- gid_enclosure_expander(drc_id_bd$er_min, drc_id_bd$er_max, 
                                             drc_id_bd$er_min[i], drc_id_bd$er_max[i])
          updated_row[i] <- er_expnd$updated
          if(er_expnd$updated){
            drc_id_bd$er_min <- er_expnd$er_min
            drc_id_bd$er_max <- er_expnd$er_max
            drc_id_bd$is_changed <- drc_id_bd$is_changed | er_expnd$is_changed
          }
        }
      }
      
      if(!any(updated_row)) break()
      
    })
    
  }
  
  drc_id_bd <- drc_id_bd %>% 
    mutate(enclosure = paste0(enclosure_direction, "_", er_min,"_", er_max)) %>% 
    select(-is_changed)
  
  encl <- drc_id_bd %>% distinct(gid, enclosure)
  
  if(!details){
    out <- encl
  }else{
    drc_bd_with_encl <- drc_bd %>% inner_join(drc_id_bd, by = "gid")
    drc_bd_with_encl <- drc_bd_with_encl %>% 
      group_by(enclosure) %>% 
      mutate(new_gid = paste0("e", substr(enclosure_direction, 1, 1), min(gid))) %>% 
      ungroup()
    # though both case can be clubbed into one expression
    if(enclosure_direction == "row"){
      encl_induced_gid <- drc_bd_with_encl %>% 
        group_by(gid = new_gid) %>% 
        summarise(r_min=er_min[1], r_max = er_max[1], c_min = min(c_min), c_max = max(c_max))
    }else{
      encl_induced_gid <- drc_bd_with_encl %>% 
        group_by(gid = new_gid) %>% 
        summarise(r_min= min(r_min), r_max = max(r_max), c_min = er_min[1], c_max = er_max[1])
    }
    
    encl_gid_map <- drc_bd_with_encl %>% distinct(gid, encl_gid = new_gid)
    
    out <- list(enclosure = encl,
                enclosure_induced_gid_boundary = encl_induced_gid,
                enclosure_gid_map = encl_gid_map)
    
  }
  
  return(out)
  
}


gid_enclosure_expander <- function(er_min, er_max, aim_er_min, aim_er_max){
  er_min_out <- er_min
  er_max_out <- er_max
  updated <- F
  rel_loc <- rep(0, length(er_min_out))
  # 5 cases for range intersecions to be updated in rel_loc
  # **||  # *|*|  # |**|  # |*|*  # ||**
  tt <- rel_loc==0
  if(any(tt)){
    # **||
    rel_loc[tt] <- ifelse(er_max[tt]<aim_er_min, 
                          1, rel_loc[tt])
  }
  
  tt <- rel_loc==0
  if(any(tt)){
    # *|*| and *||*
    rel_loc[tt] <- ifelse(er_min[tt]<aim_er_min & er_max[tt]>=aim_er_min, 
                          2, rel_loc[tt])
  }
  
  tt <- rel_loc==0
  if(any(tt)){
    # |**|
    rel_loc[tt] <- ifelse(er_min[tt]>=aim_er_min & er_max[tt]<=aim_er_max, 
                          3, rel_loc[tt])
  }
  
  tt <- rel_loc==0
  if(any(tt)){
    # |*|* and *||* 
    # (*||* : this will be already be taken by 2)
    rel_loc[tt] <- ifelse(er_min[tt]<=aim_er_max & er_max[tt]>aim_er_max, 
                          4, rel_loc[tt])
  }
  
  tt <- rel_loc==0
  if(any(tt)){
    # ||**
    rel_loc[tt] <- ifelse(er_min[tt]>aim_er_max, 
                          5, rel_loc[tt])
  }
  
  # extension cases : 2 or 4
  chtt <- (rel_loc %in% c(2, 4))
  aim_er_max_final <- max(
    aim_er_max,
    er_max[chtt]
  )
  aim_er_min_final <- min(
    aim_er_min,
    er_min[chtt]
  )
  
  # change cases : 2, 3, 4
  chtt <- (rel_loc %in% c(2, 3, 4))
  er_max_out[chtt] <- aim_er_max_final
  er_min_out[chtt] <- aim_er_min_final
  
  is_changed_row <- chtt
  
  updated <- any(chtt)
  
  list(updated = updated, er_max = er_max_out, er_min = er_min_out, is_changed_row = is_changed_row)
}
