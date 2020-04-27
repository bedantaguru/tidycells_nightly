
# @Dev

#' `exploration_findings` class
#'
#' @description
#' The `exploration_findings` class is based on data.frame,
#' created in order to store analysis of cell level information ([`cell_df`][cell_df-class]).
#'
#' @section Properties of `exploration_findings`:
#'
#' Objects of class `exploration_findings` have following named nodes:
#' * `cells` : Contains information about `cell_group_type` in terms of (data, minor and major attributes).(a tibble)
#' * `sections` : Contains boundaries of each data block. (a tibble)
#' * `details` : a list containing further information
#' * `cell_df` : The original [`cell_df`][cell_df-class] which is passed for processing
#'
#' @section Applicable methods on `exploration_findings`:
#' * `print`: Prints identifier of the class and the number of blocks (and potential issues if any).
#' * Apart from these all methods available to data.frame or tibble. 
#'
#' @name exploration_findings-class
#' @keywords internal
NULL

exploration_findings_class <- c("exploration_findings", "tbl_df", "tbl", "data.frame")
setOldClass(exploration_findings_class)