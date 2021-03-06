

# all are helpers for collate columns

norm_this <- function(x) {
  m <- min(x, na.rm = TRUE)
  M <- max(x, na.rm = TRUE)
  if (m == M) {
    if (m > 0.5) {
      x <- rep(1, length(x))
    } else {
      x <- rep(0, length(x))
    }
  } else {
    x <- (x - m) / (M - m)
  }
  x
}

get_col_representative <- function(x, cut_th = 500L, silent = TRUE) {
  dxt <- string_signature(x)
  x <- dxt$trim_name
  if (length(x) > cut_th) {
    if (!silent) {
      msg_once(paste0(
        "Representatives for column is selected based on a sample.",
        "\nYou may need to set.seed() to have a reproducible outcome"
      ))
    }
    dxt <- dxt %>% arrange(trim_name)
    fidx <- round(seq(from = 1.5, to = nrow(dxt) - 0.5, length.out = 10))
    dxtr <- dxt[-fidx, ]
    x1 <- sample(dxtr$trim_name, max(round(cut_th / 10), min(4, nrow(dxtr))))
    x2 <- dxt$trim_name[fidx]
    x <- unique(c(x1, x2))
  }
  sort(unique(x))
}

get_all_col_representative <- function(d, except_cols = NULL, ...) {
  d <- d[setdiff(colnames(d), except_cols)]
  crs <- d %>% map(get_col_representative, ...)
  crsd <- tibble(nn = names(crs), 
                 tinf = crs %>% 
                   map_chr(~ paste0(sort(.x), collapse = " :: ")))
  crsd <- crsd %>% 
    mutate(tndup = !duplicated(tinf))
  crs <- crs[crsd$nn[crsd$tndup]]
  
  crs
}



# cc : collate_columns
get_connected_cols_cc <- function(col_map_with_dist) {
  col_map_with_dist
  col_map <- tibble()
  repeat({
    col_map_this <- col_map_with_dist %>% filter(dist == min(dist))
    if (nrow(col_map_this) == 0 | nrow(col_map_with_dist) == 0) break()
    col_map <- col_map %>% bind_rows(col_map_this)
    col_map_with_dist <- col_map_with_dist %>% 
      filter(!(n1 %in% col_map$n1), !(n2 %in% col_map$n2))
  })
  col_map
}

# cc : collate_columns
reduce_2dfs_cc <- function(dc1, dc2, 
                           combine_th = 0.6,
                           common_names_rbind = FALSE,
                           fixed_columns = NULL,
                           retain_signature = TRUE,
                           rest_cols = Inf, 
                           retain_other_cols = FALSE,
                           signature_tuner_function = NULL) {
  
  
  if(common_names_rbind){
    common_names <- intersect(colnames(dc1), colnames(dc2))
    fixed_columns <- unique(c(fixed_columns, common_names))
  }
  # this renaming is required for recursive calling
  # maybe a better implementation be in place
  
  pre_exist_cname <- tibble(df = character(0), pre_exist_name = logical(0))
  if(stringr::str_detect(colnames(dc1), "collated_") %>% any()){
    pre_exist_cname <- pre_exist_cname %>% 
      bind_rows(tibble(df= "n1",pre_exist_name = T))
    colnames(dc1) <- stringr::str_replace_all(colnames(dc1), "^uncollated_", "d1_old_uc_")
    colnames(dc1) <- stringr::str_replace_all(colnames(dc1), "^collated_", "d1_old_c_")
  }
  
  if(stringr::str_detect(colnames(dc2), "collated_") %>% any()){
    pre_exist_cname <- pre_exist_cname %>% 
      bind_rows(tibble(df= "n1",pre_exist_name = T))
    colnames(dc2) <- stringr::str_replace_all(colnames(dc2), "^uncollated_", "d2_old_uc_")
    colnames(dc2) <- stringr::str_replace_all(colnames(dc2), "^collated_", "d2_old_c_")
  }
  
  
  cr1 <- get_all_col_representative(dc1, except_cols = fixed_columns)
  cr2 <- get_all_col_representative(dc2, except_cols = fixed_columns)
  
  # if either of cr1 or cr2 is empty simply rbind and return
  if (length(cr1) * length(cr2) == 0) {
    dcnew <- dc1 %>% bind_rows(dc2)
    return(dcnew)
  }
  
  all_maps <- expand.grid(n1 = names(cr1), n2 = names(cr2), stringsAsFactors = FALSE)
  
  m1 <- seq_len(nrow(all_maps)) %>%
    map(~ similarity_score(cr1[[all_maps$n1[.x]]], cr2[[all_maps$n2[.x]]]) %>% t()) %>%
    reduce(rbind)
  m2 <- m1 %>% apply(MARGIN = 2, norm_this)
  # KFL : kept for later
  # the wts can be changed
  # wts <- rep(1, ncol(m2))
  # m3 <- m2 %>% apply(MARGIN = 1, function(x) sum(x * wts) / sum(wts))
  # now simple mean only
  m3 <- m2 %>% apply(MARGIN = 1, mean)
  all_maps <- all_maps %>% mutate(dist = m3)
  
  all_maps_c <- get_connected_cols_cc(all_maps)
  
  amap_ok <- all_maps_c %>% filter(dist <= combine_th)
  amap_not_ok <- all_maps_c %>% filter(dist > combine_th)
  
  if (nrow(amap_not_ok) > 0) {
    repeat({
      induced_cols <- amap_not_ok %>%
        mutate(n1 = paste0("n1.", n1), n2 = paste0("n2.", n2)) %>%
        select(n1, n2) %>%
        unlist() %>%
        unique()
      
      if (length(induced_cols) > rest_cols) {
        amap_not_ok <- amap_not_ok %>% arrange(dist)
        if (rest_cols <= 0) {
          amap_not_ok <- amap_not_ok %>% filter(FALSE)
        } else {
          # pass a single row to amap_ok
          amap_ok <- amap_ok %>% bind_rows(amap_not_ok[1, ])
          amap_not_ok <- amap_not_ok[-1, ]
        }
      } else {
        break()
      }
    })
  }
  
  cmap <- tibble(new_name = NA, old_name = NA, df = NA) %>% filter(FALSE)
  
  if (nrow(amap_ok) > 0) {
    amap_ok <- amap_ok %>%
      arrange(dist) %>%
      mutate(new_name = paste0("collated_", seq_along(dist)))
    
    cmap_this <- amap_ok %>%
      select(-dist) %>%
      tidyr::gather(df, old_name, -new_name)
    
    cmap <- cmap %>% bind_rows(cmap_this)
  }
  
  if (nrow(amap_not_ok) > 0) {
    
    cmap_this <- amap_not_ok %>%
      arrange(dist) %>% 
      select(-dist) %>%
      tidyr::gather(df, old_name) %>%
      mutate(new_name = paste0("uncollated_", seq_along(old_name)))
    
    cmap <- cmap %>% bind_rows(cmap_this)
  }
  
  cmap1 <- cmap %>% filter(df == "n1")
  cmap2 <- cmap %>% filter(df == "n2")
  
  dc1 <- rename_base(dc1, old_names = cmap1$old_name, new_names = cmap1$new_name)
  
  dc2 <- rename_base(dc2, old_names = cmap2$old_name, new_names = cmap2$new_name)
  
  dcnew <- dc1 %>% bind_rows(dc2)
  
  if (!retain_other_cols) {
    nc_cols <- colnames(dcnew) %>%
      stringr::str_detect("collated") %>%
      colnames(dcnew)[.]
    dcnew <- dcnew[c(intersect(fixed_columns, colnames(dcnew)), nc_cols)]
  }
  
  if(retain_signature){
    # keep a trail of which columns merged into one
    old_sig <- attr(dc1, "collate_columns_map") %>% 
      bind_rows(attr(dc2, "collate_columns_map")) %>% 
      unique()
    if(is.function(signature_tuner_function)){
      cmap <- signature_tuner_function(cmap, dc1, dc2)
    }
    if(nrow(old_sig)>0){
      attr(dcnew, "collate_columns_map") <- cmap %>% 
        left_join(pre_exist_cname, by = "df") %>% 
        mutate(pre_exist_name = ifelse(is.na(pre_exist_name), FALSE, T),
               iter = max(old_sig$iter)+1) %>% 
        bind_rows(old_sig,.) %>% 
        unique()
    }else{
      attr(dcnew, "collate_columns_map") <- cmap %>% 
        left_join(pre_exist_cname, by = "df") %>% 
        mutate(pre_exist_name = ifelse(is.na(pre_exist_name), FALSE, T),
               iter = 1) 
    }
    
      
  }
  
  dcnew
}

