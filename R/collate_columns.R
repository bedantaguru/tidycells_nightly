
# Dev
# clean documentation

#' Collate Columns Based on Content
#'
#' @description After [`compose_cells`][compose_cells()], this function rearranges and rename attribute-columns in order to
#' make columns properly aligned, based on the content of the columns.
#'
#' @param composed_data output of [`compose_cells`][compose_cells()] (preferably not processed)
#' @param combine_threshold a numerical threshold (between 0-1) for content-based collation of columns. (Default 1)
#' (High value indicate high chance of collation.)
#' @param rest_cols number of rest columns (beyond `combine_threshold` joins these many numbers of columns to keep)
#' @param retain_other_cols whether to keep other intermediate (and possibly not so important) columns. (Default `FALSE`)
#' @param retain_cell_address whether to keep columns like (`row`, `col`, `data_block`).
#' This may be required for [`traceback`][cell_composition_traceback()] (Default `FALSE`)
#'
#' @return A column collated data.frame
#'
#' @details
#' * **Dependency on _stringdist_**: If you have \code{\link[stringdist:stringdist-package]{stringdist}} installed,
#' the approximate string matching will be enhanced. There may be variations in outcome if you have `stringdist`
#' vs if you don't have it.
#' * **Possibility of randomness**: If the attribute column is containing many distinct values, then a column representative sample will be drawn.
#' Hence it is always recommended to [`set.seed`][base::set.seed()] if reproducibility is a matter of concern.
#'
#' @export
#'
#' @examples
#'
#' d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>%
#'   readRDS()
#' d <- numeric_values_classifier(d)
#' da <- analyze_cells(d)
#'
#' dc <- compose_cells(da, print_attribute_overview = TRUE)
#'
#' collate_columns(dc)
collate_columns <- function(x, ...,
                            common_names_rbind = FALSE,
                            fixed_columns = NULL,
                            combine_threshold = 1,
                            retain_signature = TRUE,
                            rest_cols = Inf,
                            retain_other_cols = FALSE){
  fixed_columns <- nse_to_se_colname_picker(substitute(fixed_columns))
  # @DFOut always
  UseMethod("collate_columns")
}

collate_columns.composed_list <- function(x,  ...,
                                          common_names_rbind = FALSE,
                                          fixed_columns = NULL,
                                          combine_threshold = 1,
                                          retain_signature = TRUE,
                                          rest_cols = Inf,
                                          retain_other_cols = FALSE,
                                          retain_cell_address = FALSE) {
  
  # @Dev
  # table tag for tcf may be considered
  this_fixed_cols <- c("row", "col", "data_block", "value")
  fixed_columns <- c(fixed_columns, this_fixed_cols) %>% unique()
  
  out_d <-  reduce(x,
                   reduce_2dfs_cc,
                   combine_th = combine_threshold, 
                   common_names_rbind = common_names_rbind,
                   fixed_columns = fixed_columns, 
                   retain_signature = retain_signature, 
                   rest_cols = rest_cols, 
                   retain_other_cols = retain_other_cols, 
                   signature_tuner_function = function(cmap, d1, d2){
                     dmap <- tibble(
                       df = c("n1","n2"),
                       data_block = c(d1$data_block[1], d2$data_block[1]))
                     cmap %>% left_join(dmap, by = "df")
                   })
  
  if (!retain_cell_address) {
    out_d <- out_d[setdiff(colnames(out_d), c("row", "col", "data_block"))]
  }
  
  class(out_d) <- column_collated_df_class
  out_d[sort(colnames(out_d))]
}


collate_columns.composed_df <- function(x,  ...,
                                        common_names_rbind = FALSE,
                                        fixed_columns = NULL,
                                        combine_threshold = 1,
                                        retain_signature = TRUE,
                                        rest_cols = Inf,
                                        retain_other_cols = FALSE,
                                        retain_cell_address = FALSE) {
  
  
  
  
  xl <- split(x, x$data_block)
  collate_columns.composed_list(xl, ...,
                                common_names_rbind = common_names_rbind,
                                fixed_columns = fixed_columns,
                                combine_threshold = combine_threshold,
                                retain_signature = retain_signature,
                                rest_cols = rest_cols,
                                retain_other_cols = retain_other_cols,
                                retain_cell_address = retain_cell_address)
}


collate_columns.list <- function(x,  ...,
                                 common_names_rbind = FALSE,
                                 fixed_columns = NULL,
                                 combine_threshold = 1,
                                 retain_signature = TRUE,
                                 rest_cols = Inf,
                                 retain_other_cols = FALSE){
  
  out_d <- reduce(x,
                  reduce_2dfs_cc,
                  combine_th = combine_threshold, 
                  common_names_rbind = common_names_rbind,
                  fixed_columns = fixed_columns, 
                  retain_signature = retain_signature, 
                  rest_cols = rest_cols, 
                  retain_other_cols = retain_other_cols)
  
  class(out_d) <- column_collated_df_class
  out_d
}

collate_columns.data.frame <- function(x, y, ...,
                                       common_names_rbind = FALSE,
                                       fixed_columns = NULL,
                                       combine_threshold = 1,
                                       retain_signature = TRUE,
                                       rest_cols = Inf,
                                       retain_other_cols = FALSE){
  out_d <- reduce_2dfs_cc(x, y, 
                          combine_th = combine_threshold, 
                          common_names_rbind = common_names_rbind,
                          fixed_columns = fixed_columns, 
                          retain_signature = retain_signature, 
                          rest_cols = rest_cols, 
                          retain_other_cols = retain_other_cols)
  class(out_d) <- column_collated_df_class
  out_d
}

collate_columns.matrix <- function(x, y, ...,
                                   common_names_rbind = FALSE,
                                   fixed_columns = NULL,
                                   combine_threshold = 1,
                                   retain_signature = TRUE,
                                   rest_cols = Inf,
                                   retain_other_cols = FALSE){
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  out_d <- reduce_2dfs_cc(x, y, 
                          combine_th = combine_threshold, 
                          common_names_rbind = common_names_rbind,
                          fixed_columns = fixed_columns, 
                          retain_signature = retain_signature, 
                          rest_cols = rest_cols, 
                          retain_other_cols = retain_other_cols)
  class(out_d) <- column_collated_df_class
  out_d
}
