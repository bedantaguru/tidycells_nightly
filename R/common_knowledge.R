

# get and set common_knowledge
# this is to share information across multiple modules of analyze_cells
common_knowledge <- function(..., clean = FALSE, is = FALSE, simplify = TRUE){
  if(clean){
    tidycells_pkg_env$common_knowledge <- NULL
    return(invisible(0))
  }
  dots <- list(...)
  nds <- names(dots)
  
  if(is.null(nds)){
    dots_with_name <- list()
    dots_without_name <- dots
  }else{
    dots_with_name <- dots[nchar(nds)>0]
    dots_without_name <- dots[nchar(nds)==0]
  }
  
  dots_without_name_chrs <- dots_without_name %>% map_lgl(is.character) %>% dots_without_name[.] %>% unlist() %>% unique()
  
  ck_now <- tidycells_pkg_env$common_knowledge
  
  dots_for_get <- dots_without_name_chrs %>% intersect(names(ck_now))
  
  if(length(dots_with_name)>0){
    # set these args
    if(is.null(ck_now)){
      ck_put <- dots_with_name
    }else{
      ck_now_nomatch <- ck_now[setdiff(names(ck_now), names(dots_with_name))]
      ck_put <- c(ck_now_nomatch, dots_with_name)
    }
    tidycells_pkg_env$common_knowledge <- ck_put
  }
  
  if(length(dots_for_get)>0){
   # return get values
    if(is){ 
      return(TRUE)
    }
    xo <- tidycells_pkg_env$common_knowledge[dots_for_get]
    
    if(simplify){
      #  in case only one object needed return the same
      if(length(xo)==1){
        return(xo[[1]])
      }
    }
    
    return(xo)
    
  }
  return(invisible(0))
}

is_common_knowledge <- function(...){
  isTRUE(common_knowledge(..., is= TRUE))
}
