

# get and set common_knowledge
# this is to share information across multiple modules of analyze_cells
common_knowledge <- function(..., clean = FALSE, add = TRUE, is = FALSE, simplify = TRUE){
  if(clean){
    tidycells_pkg_env$common_knowledge <- NULL
    return(invisible(NULL))
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
      ck_now_match <- ck_now[intersect(names(ck_now), names(dots_with_name))]
      
      if(length(ck_now_match)>0 & add){
        dots_with_name_exists <- dots_with_name[intersect(names(ck_now), names(dots_with_name))]
        c_this <- ck_now_match %>% seq_along() %>% map(~{
          Append(ck_now_match[[.x]], dots_with_name_exists[[.x]])
        })
        names(c_this) <- names(ck_now_match)
        ck_put <- c(c_this, ck_now_nomatch)
      }else{
        ck_put <- c(dots_with_name, ck_now_nomatch)
      }
      
      
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
  return(invisible(NULL))
}

is_common_knowledge <- function(...){
  isTRUE(common_knowledge(..., is= TRUE))
}


Append <- function(x, y){
  UseMethod("Append")
}

Append.data.frame <- function(x, y){
  bind_rows(x, y) %>% unique()
}
Append.default <- function(x, y){
  unique(c(x, y))
}