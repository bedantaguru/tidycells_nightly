

# @Dev

attach_trace_info_external <- function(exl, dc){
  
  refresh <- FALSE
  
  if(missing(dc)){
    refresh <- TRUE
  }else{
    if(!any(stringr::str_detect(colnames(dc), "cellAddress_"))){
      refresh <- TRUE
    }
  }
  
  if(refresh){
    msg <- paste("This is highly experimental feature. Use at your own risk.",
                 "This is designed for traceback feature when external package/application",
                 "(anything apart from tidycells) is used for structural assignment / tidyfication.",
                 "The external arugement should be supplied with a list having two named nodes.",
                 "The node named <cells> should contain a <cell_df> which has been tidyfied.",
                 "Another node named <operation> should contain a two-argument-function.",
                 "This function should be responsible for tidyfication.", sep="\n")
    
    msg_once(msg)
    
    if(!hasName(exl,"cells") & !hasName(exl,"operation")){
      abort("Please check the guides. The external <list> does not have required nodes.")
    }
    
    if(!is.function(exl$operation)){
      abort("<operation> node of <external> is not a function")
    }
    
    d1 <- exl$operation(exl$cells)
    
    dcells <- exl$cells %>% mutate(value = paste0(row, "_", col)) 
    d2 <- exl$operation(dcells, exl$cells)
    
    d2 <- d2 %>% mutate(chk_col = paste0(row, "_", col))
    
    vcol <- d2 %>% select(-chk_col) %>% .[1:min(100, nrow(d2)),] %>% map_int(~.x %>% intersect(d2$chk_col) %>% length) %>% which.max() %>% names()
    
    d1.1 <- d1 
    colnames(d1.1)[which(colnames(d1.1)==vcol)] <- "value"
    
    d1.1.1 <- d1.1 %>% select(row, col, value)
    d1.1.2 <- d1.1 %>% select(-row, -col, -value)
    orig_colnames <- colnames(d1.1.2)
    
    cname_map <- tibble(orig = orig_colnames, cname = paste0("major_", seq_along(orig_colnames)), cname_l2 = paste0("major_col_top_", seq_along(orig_colnames), "_1"))
    
    colnames(d1.1.2) <- cname_map$cname
    
    d1.1.2.b <- d1.1.2
    colnames(d1.1.2.b) <- cname_map$cname_l2
    
    d1_final <- dplyr::bind_cols(d1.1.1, d1.1.2, d1.1.2.b)
    
    
    d2.1 <- colnames(d2) %>% setdiff(c(vcol, "chk_col")) %>% d2[.]
    
    d2.2  <- d2.1 %>% select(-row, -col) %>% map(~.x %>% stringr::str_replace_all("[^0-9_]+"," :: ")) %>% as_tibble()
    
    colnames(d2.2) <- paste0("cellAddress_",cname_map$cname_l2)
    
    d2_final <- dplyr::bind_cols(d2.1 %>% select(row, col), d2.2)
    
    
    dc_this <-  d1_final %>% inner_join(d2_final, by = c("row", "col"))
    
    dc <- dc_this %>% mutate(data_block = 1)
    
    cname_map <- cname_map %>% bind_rows(tibble(orig = vcol, cname = "value"))
    
    attr(dc, "tidycells.cname_map") <- cname_map
  }
  
  dc
}