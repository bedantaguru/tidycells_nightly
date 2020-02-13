

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



formalize_tfc <- function(x){
  if(!validate_tfc(x)){
    abort("Something went wrong...")
  }
  x <- name_fix_for_list(x, name_tag = "Table_Field")
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
  if(!("with_content" %in% state(x))){
    abort(paste0("Does not have <content>. Have you missed <read_it>?"))
  }
  
  tfco <- x$tfc %>% 
    seq_along %>% 
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
