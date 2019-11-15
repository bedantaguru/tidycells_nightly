

# @Dev
# a list of either matrix or data.frame


#' `Table_List` class
#'
#' @description
#' The `Table_Field_Container` class is based on data.frame,
#' created in order to store analysis of cell level information ([`cell_df`][cell_df-class]).
#'
#' @section Properties of `Table_Field_Container`:
#'
#' Objects of class `Table_Field_Container` have following named nodes:
#' * `cells` : Contains information about `cell_group_type` in terms of (data, minor and major attributes).(a tibble)
#' * `sections` : Contains boundaries of each data block. (a tibble)
#' * `details` : a list containing further information
#' * `cell_df` : The original [`cell_df`][cell_df-class] which is passed for processing
#'
#' @section Applicable methods on `Table_Field_Container`:
#' * `print`: Prints identifier of the class and the number of blocks (and potential issues if any).
#' * Apart from these all methods available to data.frame or tibble. 
#'
#' @name Table_Field_Container-class
#' @keywords internal
NULL


Table_List_class <- c("Table_List", "list")
setOldClass(Table_List_class)


as_table_list <- function(x){
  if(!is.list(x)){
    abort("Not a list")
  }
  xo <- x
  if(length(xo)>0){
    all_same <- FALSE
    mode <- ""
    if(is.matrix(xo[[1]])){
      if(xo %>% map_lgl(is.matrix) %>% all) mode <- "matrix"
    }
    if(is.data.frame(xo[[1]])){
      if(xo %>% map_lgl(is.data.frame) %>% all) mode <- "data.frame"
    }
    if(!(mode %in% c("matrix","data.frame"))){
      abort("all nodes must be either matrix or data.frame")
    }
    attr(xo, "mode") <- mode
  }
  
  xo <- name_fix_for_list(xo, name_tag = "Table")
  if(!is.null(attr(xo, "meta"))){
    attr(xo, "meta") <- attr(xo, "meta") %>% mutate(name = names(xo))
  }
  class(xo) <- Table_List_class
  xo
  
}
