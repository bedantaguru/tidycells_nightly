
#' Transform data into Cell-DF Structure
#'
#' @description Transform an R object (mostly matrix or data.frame) into a [`cell_df`][cell_df-class]
#' for further processing in other `tidycells` functions.
#'
#' @param d the data (either a matrix with column name or a data.frame)
#' @param take_col_names consider column names as separate cells
#' (applicable only for data with no (row, col) information). Default is \code{TRUE}.
#' @param take_row_names consider row names as separate cells
#' (applicable only for data with no (row, col) information). Default is \code{FALSE}.
#'
#' @return An object of class [`cell_df`][cell_df-class].
#'
#' **Note**: After this, you may like to do [`Value Attribute Classification`][value_attribute_classify()].
#'
#' @export
#' @rdname as_cell_df
#' @seealso
#' * [`validate_cells`][validate_cells()] which is used to validate `cell_df`.
#' * [`as_cells`][unpivotr::as_cells()] from `unpivotr` package.
#'
#' @examples
#'
#' as_cell_df(iris)
#'
#' # consider column name as cell
#' as_cell_df(iris, take_col_names = TRUE)
#'
#' # if the data is already in a similar format it will not further transform
as_cell_df <- function(d, take_col_names = TRUE, take_row_names = FALSE, ...) {
  UseMethod("as_cell_df")
}

#' @export
as_cell_df.data.frame <- function(d,  take_col_names = TRUE, take_row_names = FALSE, ...) {
  di <- d %>%
    attach_intermediate_class() 
  
  if(inherits(di, "unknown")){
    di %>%
      as_cell_df_internal(take_col_names = take_col_names, take_row_names = take_row_names, ...)
  }else{
    di %>%
      as_cell_df_internal(...)
  }
  
}

#' @export
as_cell_df.matrix <- function(d, take_col_names = TRUE, take_row_names = FALSE, ...) {
  di <- d %>%
    as_tibble() %>%
    attach_intermediate_class() 
  
  if(inherits(di, "unknown")){
    di %>%
      as_cell_df_internal(take_col_names = take_col_names, take_row_names = take_row_names, ...)
  }else{
    di %>%
      as_cell_df_internal(...)
  }
  
}
