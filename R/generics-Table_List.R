

print.Table_List <- function(x, ...){

  msg <- paste0(
    cli_bb("Table_List"), cli_b(" with :"),"\n",
    cli_bs(), cli_b("Number of "), cli_bb("Tables"), 
    cli_b(paste0(" (",attr(x, "mode"),")",": ")), length(x)
  )
  
  cat(msg)
}


# @Dev
calc_meta_for_tl <- function(x, fresh = FALSE){
  
  last_meta <- attr(x, "meta")
  
  refresh <- FALSE
  if(is.null(last_meta) | !is.data.frame(last_meta) | fresh | !hasName(last_meta, "name")){
    refresh <- TRUE
  }
  
  if(is.data.frame(last_meta)){
    if(hasName(last_meta, "name")){
      if(length(intersect(names(x), last_meta$name)) < length(x)){
        refresh <- TRUE
      }
    }
  }
  
  if(refresh){
    last_meta <- tibble(id = seq_along(x), name = names(x))
  }
  
  xt <- x[last_meta$name]
  
  if(!hasName(last_meta, "content") | fresh){
    last_meta <- last_meta %>% mutate(content = xt %>% map_chr(get_content))
  }
  
  if(!hasName(last_meta, "nrow") | fresh){
    last_meta <- last_meta %>% mutate(nrow = xt %>% map_dbl(~max(.x$row)-min(.x$row)+1))
  }
  
  if(!hasName(last_meta, "ncol") | fresh){
    last_meta <- last_meta %>% mutate(ncol = xt %>% map_dbl(~max(.x$col)-min(.x$col)+1))
  }
  
  if(!hasName(last_meta, "size") | fresh){
    last_meta <- last_meta %>% mutate(size = ncol*nrow)
  }
  
  last_meta
  
}

filter.Table_Field_Container <- function(.data, ..., refresh = FALSE){
  x <- .data
  
  this_meta <- calc_meta_for_tfc(x, fresh = refresh)
  this_meta_flt <- filter(this_meta, ...)
  x_flt <- x[this_meta_flt$name]
  attr(x_flt, "meta") <- this_meta_flt
  
  if(length(x)>0 & length(x_flt) == 0){
    dts <- rlang::enquos(...)
    dts <- dts %>% unlist() %>% as.character() %>% c(names(dts))
    dts <- dts %>% stringr::str_detect("content") %>% dts[.]
    dts <- dts %>% stringr::str_detect("[A-Z]") %>% dts[.]
    chk <- length(dts) > 0 
    if(chk){
      msg_once("Seems like you are filtering 'Table_Field_Container' based on content.",
               "\nKindly note that content is in lower case.",
               "\nThe string with which you are comparing should be also in lower case.",
               "\nCheck once whether that is the case.")
    }
  }
  
  formalize_tfc(x_flt)
}


as_tibble.Table_Field_Container <- function(x, ...){
  names(x) %>% map_df(~x[[.x]] %>% mutate(table_tag = .x) %>% as_tibble()) 
}

plot.Table_Field_Container <- function(x, ..., no_plot = FALSE){
  dummy_cdf <- as_tibble(x)
  bg <- plot.cell_df(dummy_cdf, ..., no_plot = TRUE, txt_alpha = 0.1)
  g <- bg+ggplot2::facet_wrap(~table_tag, scales = "free")
  
  if(!no_plot){
    graphics::plot(g)
  }
  
  return(invisible(g))
}


