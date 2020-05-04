

intra_block_dist <- function(cd, method = c("hybrid", "real", "approx"), nearby_threshold = 5){
  method <- match.arg(method)
  
  if("with_table_blocks" %in% state(cd)){
    cd <- detect_table_block(cd)
  }
  
  if(method == "hybrid"){
    return(hybrid_intra_block_dist(cd, nearby_threshold = nearby_threshold))
  }
  
  if(method == "real"){
    return(real_intra_block_dist(cd))
  }
  
  return(approx_intra_block_dist(cd))
  
}


# helpers and raw functions

# helper function to approx_intra_block_dist_part1_get_upper_dist
# a generic concept similar to this
# https://stackoverflow.com/a/26178015/2694407
rect_dist <- function(r1m, c1m, r1M, c1M, r2m, c2m, r2M, c2M){
  
  left <- c2M < c1m
  right <- c1M < c2m   
  top <- r2M < r1m
  bottom <- r1M < r2m
  
  if(top & left){
    return(sqrt((r2M-r1m)^2+(c2M-c1m)^2))
  }
  
  if(top & right){
    return(sqrt((r2M-r1m)^2+(c1M-c2m)^2))
  }
  
  if(bottom & left){
    return(sqrt((r1M-r2m)^2+(c2M-c1m)^2))
  }
  
  if(bottom & right){
    return(sqrt((r1M-r2m)^2+(c1M-c2m)^2))
  }
  
  if(left){
    return(c1m-c2M)
  }
  
  if(right){
    return(c2m-c1M)
  }
  
  if(top){
    return(r1m-r2M)
  }
  
  if(bottom){
    return(r2m-r1M)
  }
  
  return(0)
  
}

# helper for approx_intra_block_dist
approx_intra_block_dist_part1_get_upper_dist <- function(gid_this, bdrs){
  guppers <- bdrs %>% filter(gid > gid_this)
  ## Note
  # this check is not required if following is ensured
  # gids <- sort(bdrs$gid)
  # gids[-length(gids)]
  ## -->if(nrow(guppers)==0) return(tibble())
  gthis <- bdrs %>% filter(gid == gid_this)
  guppers %>% dplyr::rowwise() %>% mutate(d = rect_dist(gthis$rm, gthis$cm, gthis$rM, gthis$cM, rm, cm, rM, cM)) %>% 
    ungroup() %>% mutate(gid1 = gid_this) %>% select(gid1, gid2 = gid, d)
}

### dist based on gid boundary
# fastest of three lacks accuracy
approx_intra_block_dist <- function(cd){
  bdrs <- cd %>% group_by(gid) %>% summarise(rm = min(row), cm = min(col), rM = max(row), cM =max(col))
  gids <- sort(bdrs$gid)
  gids[-length(gids)] %>% map_df(approx_intra_block_dist_part1_get_upper_dist, bdrs)
}


### this is a real dist as per def of it for any cell df with gid
# can be used in another module too
get_intra_block_dist <- function(gid1, gid2, cd){
  
  cd1 <- cd %>% filter(gid == gid1) %>% select(row, col) %>% as_tibble()
  cd2 <- cd %>% filter(gid == gid2) %>% select(row, col) %>% as_tibble()
  
  cd12 <- cd1 %>% mutate(dummy = 1) %>% full_join(cd2 %>% mutate(dummy = 1), by = "dummy")
  cd12 <- cd12 %>% mutate(dr = row.x - row.y, dc = col.x - col.y,
                          d = sqrt(dr^2+dc^2))
  min(cd12$d)
}

### this is based on actual dist
# extremely slow
real_intra_block_dist <- function(cd){
  gids <- cd$gid %>% unique()
  
  all_2_gids <- expand.grid(gid1 = gids, gid2 = gids, stringsAsFactors = FALSE)
  
  all_2_gids_upper <- all_2_gids %>% filter(gid1<gid2)
  
  if(nrow(all_2_gids_upper)==0){
    # case of single gid
    return(tibble())
  }
  
  all_2_gids_upper <- all_2_gids_upper %>% dplyr::rowwise() %>% mutate(d = get_intra_block_dist(gid1, gid2, cd)) %>% ungroup()
  
  return(all_2_gids_upper)
}

### this is optimal approach where best of speed and accuracy can be achieved
hybrid_intra_block_dist <- function(cd, nearby_threshold = 5){
  da <- approx_intra_block_dist(cd)
  dr <- da %>% filter(d<=nearby_threshold)
  if(nrow(dr)==0){
    # case of single gid
    return(da)
  }
  dr <- dr %>% dplyr::rowwise() %>% mutate(d = get_intra_block_dist(gid1, gid2, cd)) %>% ungroup()
  da_rest <- da %>% filter(d>nearby_threshold)
  bind_rows(da_rest,dr)
}
