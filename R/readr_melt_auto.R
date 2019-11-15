

readr_melt_auto<- function(fn){
  
  melt_fns <- list(
    melt_csv = readr::melt_csv, 
    melt_csv2 = readr::melt_csv2, 
    melt_table2 = readr::melt_table2,
    melt_tsv = readr::melt_tsv
  )
  
  sel <- -1
  
  suppressWarnings(suppressMessages({
    try_outs <- melt_fns %>% map(~.x(fn, n_max = 100))
    
    try_out_score <- try_outs %>% map_df(~{
      lo <- list()
      
      lo$nr <- nrow(.x)
      lo$nc <- max(.x$col)
      lo$cnvar <- .x %>% group_by(row) %>% summarise(mc = max(col)) %>% pull(mc) %>% sd()
      lo$cvar <- .x %>% filter(row > min(row)) %>% group_by(col) %>% 
        summarise(cvar = value %>% stringr::str_split("") %>% unlist() %>% 
                    unique() %>% length()) %>% pull(cvar) %>% max()
      lo$mxnch <- .x %>% pull(value) %>% nchar() %>% max()
      
      as_tibble(lo)
      
    })
    
    try_out_score <- try_out_score %>% mutate(cnvar=-cnvar, cvar=-cvar, mxnch= -mxnch)
    
    try_out_score_norm <- try_out_score %>% map(norm_this) %>% as_tibble()
    score_final <- try_out_score_norm %>% apply(1, mean, na.rm = TRUE) %>% norm_this()
    
    
    sel <- which.max(score_final)
    
  }))
  
  if(sel %in% 1:length(melt_fns)){
    
    suppressWarnings(suppressMessages({
      dat <- melt_fns[[sel]](fn)
    }))  
    
    attr(dat, "used_function") <- names(melt_fns)[[sel]]
    
  }else{
    dat <- NULL
  }
  
  return(dat)
  
}

