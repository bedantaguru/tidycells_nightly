

# states

as_tfc <- function(x, ...){
  UseMethod("as_tfc")
}

validate_tfc <- function(x, strong = FALSE){
  if(strong){
    x %>% map_lgl(validate_cells) %>% all()
  }else{
    x %>% map_lgl(is_cell_df) %>% all()
  }
}

name_fix_for_tfc <- function(xl){
  
  if(is.null(names(xl))){
    names(xl) <- paste0("Table_Field_", seq_along(xl))
  }else{
    nms <- names(xl)
    nms <- nms[!is.na(nms)]
    nms <- nms[nchar(nms)>0]
    nms <- unique(nms)
    if(length(nms)!=length(xl)){
      nmap <- tibble(nms = names(xl), seq = seq_along(nms))
      nmap <- nmap %>% mutate(is_blank = nchar(nms)==0)
      nmap_blnk <- nmap %>% filter(is_blank)
      nmap <- nmap %>% mutate(nn = nms)
      if(nrow(nmap_blnk)>0){
        nmap_blnk <- nmap_blnk %>% mutate(nn = paste0("Table_Field_", seq_along(nms)))
        nmap <- nmap %>% filter(!is_blank) %>% bind_rows(nmap_blnk)
      }
      
      if(any(duplicated(nmap$nn))){
        nmapl <- nmap %>% group_by(nn) %>% group_split()
        nmap <- nmapl %>% map_df(~{
          if(nrow(.x)>1){
            .x <- .x %>% mutate(nn = paste0(nn, seq_along(nn)))
          }
          .x
        })
      }
      nmap <- nmap %>% arrange(seq)
      names(xl) <- nmap$nn
    }
  }
  
  xl
  
}

formalize_tfc <- function(x){
  if(!validate_tfc(x)){
    abort("Something went wrong...")
  }
  x <- name_fix_for_tfc(x)
  if(!is.null(attr(x, "meta"))){
    attr(x, "meta") <- attr(x, "meta") %>% mutate(name = names(x))
  }
  class(x) <- Table_Field_Container_class
  x
}

as_tfc.list <- function(x, ...){
  xtfc <- x %>% map(as_cell_df)
  
  formalize_tfc(xtfc)
}

as_tfc.cell_df <- function(x, ...){
  xtfc <- list(Table_Field_1 = x)
  formalize_tfc(xtfc)
}

as_tfc.tbl <- function(x, ...){
  xtfc <- list(Table_Field_1 = as_cell_df(x, take_col_names = TRUE))
  formalize_tfc(xtfc)
}

as_tfc.data.frame <- as_tfc.tbl

as_tfc.matrix <- as_tfc.tbl

as_tfc.exploration_findings <- function(x, ...){
  if(state(x)!="with_content"){
    abort(paste0("Does not have <content>. Have you missed <read_it>?"))
  }
  
  tfco <- x$tfc %>% 
    seq_along() %>% 
    map(~{
      nn <- x$tfc[[.x]]
      if(!is.null(nn)){
        names(nn) <- file.path(x$well_name[.x], names(nn))
      }
      nn
    }) %>% 
    reduce(c)
  
  formalize_tfc(tfco)
}

as_tfc.Table_Field_Container <- function(x, ...){
  formalize_tfc(x)
}
