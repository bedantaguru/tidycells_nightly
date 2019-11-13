
calc_content_variation_over_TFs <- function(x){
  lmt <- calc_meta_for_tfc(x)
  wds <- lmt$content %>% map(~stringr::str_split(.x, " ") %>% unlist)
  dr <- wds %>% unlist() %>% unique() %>% length()
  nr <- wds %>% reduce(intersect) %>% length()
  1-nr/dr
}

print.Table_Field_Container <- function(x, ...){
  if(is.null(attr(x, "content_variation_over_TFs"))){
    cv_msg <- cli_b("<content-information> is not generated. You may do the same by <read_it>.")
  }else{
    cv_this <- attr(x, "content_variation_over_TFs")
    cv_msg <- paste0(cli_b("Content Variation across TF(s): "), 
                     ifelse(cv_this>0.7, 
                            cli_r("High"), 
                            ifelse(cv_this<=0.3, 
                                   cli_g("Low"), 
                                   cli_b("Not Very Low"))), 
                     " (", round(cv_this, 2), ")")
  }
  
  msg <- paste0(
    cli_bb("Table_Field_Container"), cli_b(" with :"),"\n",
    cli_bs(), cli_b("Number of "), cli_bb("Table_Fields"), cli_b(": "), length(x),"\n",
    cli_bs(), cv_msg
  )
  
  cat(msg)
}


calc_meta_for_tfc <- function(x, fresh = FALSE){
  
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
    dts <- rlang::enquos(...) %>% unlist() %>% as.character()
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


