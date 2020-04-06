

# @Dev
# document clear


# this hosts all intermediate classes which are exposed to user functions directly or indirectly 



#' minor internal classes
#'
#' @description
#' The `df` class is based on data.frame,
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
#' @name df-class
#' @keywords internal
NULL


df_class <- c("df", c("tbl_df", "tbl", "data.frame"))
setOldClass(df_class)



composed_df_class <- c("composed_df", df_class)
setOldClass(composed_df_class)

composed_list_class <- c("composed_list", "list")
setOldClass(composed_list_class)


column_collated_df_class <- c("column_collated_df",df_class)
setOldClass(column_collated_df_class)