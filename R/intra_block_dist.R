

intra_block_dist <- function(
  cd, 
  method = c("hybrid", "real", "approx"), 
  nearby_threshold = 1.9
){
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

### dist based on gid boundary
# fastest of three lacks accuracy
approx_intra_block_dist <- function(cd){
  cd <- cd %>% as_tibble()
  dr <- get_raw_map_for_ai_get_data_attr_map(
    get_group_id_boundary(cd),
    cd
  )
  drm <- dr %>% 
    group_by(g1 = attr_gid, g2 = data_gid) %>% 
    summarise(d = min(dist)) %>% 
    ungroup()
  
  drm <- drm %>% 
    mutate(gid1 = pmin(g1, g2), gid2 = pmax(g1, g2)) %>% 
    group_by(gid1, gid2) %>% 
    summarise(d = min(d)) %>% 
    ungroup()
  
  drm
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

