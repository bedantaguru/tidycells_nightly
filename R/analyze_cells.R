


#' Analyze Cells
#'
#' @description After [`Value Attribute Classification`][value_attribute_classify()] done on a [`cell_df`][cell_df-class] next task to do is
#' analyze it's contents for data block detection, attribute orientation identification etc. The function `analyze_cells` (and also `analyse_cells`)
#' does the same for you.
#'
#' **Note**:
#' \if{html}{If you are not sure about what package functions actually do or how they work together,
#' please start with \href{../doc/tidycells-intro.html}{\code{vignette("tidycells-intro")}}.}
#' \if{latex}{If you are not sure about what package functions actually do or how they work together,
#' please start with \code{vignette("tidycells-intro")}.}
#'
#' @param d A [`cell_df`][cell_df-class] after [`Value Attribute Classification`][value_attribute_classify()] done
#' @param silent logical scalar indicating whether to raise a warning if heuristic detection fails. (Default TRUE).
#'
#' @details it returns detailed analysis of the data structure including data block detection, attribute orientation detection etc.
#' The argument `silent` is set to `TRUE` by default, as the warning will be given whenever the [`cell_analysis`][cell_analysis-class] is printed.
#'
#' After this step one may like to do :
#' * [`compose_cells`][compose_cells()]
#'
#' If in an interactive session, following additional functions can be helpful for interactive visualizations:
#' * [`visual_data_block_inspection`][visual_data_block_inspection()]
#' * [`visual_orientation_modification`][visual_orientation_modification()]
#' * [`visual_traceback`][visual_traceback()]
#'
#' @return Detailed analysis of the cell data structure.
#' Which will be a [`cell_analysis`][cell_analysis-class] class object.
#'
#' @seealso [`compose_cells`][compose_cells()], [`collate_columns`][collate_columns()]
#' @export
#' @examples
#' d <- structure(c(
#'   "block 1", "", "C", "D", "", "block 2", "", "C",
#'   "D", "", "A", "1", "2", "", "", "A", "10", "20", "", "B", "3",
#'   "4", "", "", "B", "30", "40"
#' ), .Dim = c(9L, 3L))
#' d <- as.data.frame(d)
#' cd <- as_cell_df(d) %>% numeric_values_classifier()
#'
#' # see it
#' cd %>% plot(adaptive_txt_size = FALSE)
#' ca <- analyze_cells(cd)
#'
#' # look at the plot for detected directions
#' plot(ca)
analyze_cells <- function(d, silent = TRUE) {
  analyze_cells_raw(d = d, silent = silent)
}


analyze_cells_raw <- function(d, silent = TRUE) {
  
  #ref: ai_main_part are main sections of analyze cells
  
  step0 <- ai_main_part_phase_0_pre_process(
    d
  )
  
  ##################
  # analysis start #
  ##################
  
  # phase-1
  step1 <- ai_main_part_phase_1_admap(
    d_dat = step0$d_dat, 
    d_att = step0$d_att
  )
  
  # phase-2
  step2 <- ai_main_part_phase_2_gid_joins(
    d_dat = step0$d_dat, 
    d_att = step1$d_att, 
    admap = step1$admap, 
    d = step0$d
  )
  
  step3 <- ai_main_part_phase_3_rest_map(
    d_dat = step2$d_dat, 
    d_att = step2$d_att, 
    admap = step2$admap
  )
  
  
  step4 <- ai_main_part_phase_4_header_orientation(
    d_dat = step2$d_dat, 
    d_att = step2$d_att, 
    admap = step3$admap
  )
  
  
  ################
  # analysis end #
  ################
  
  ai_main_part_phase_5_post_process(
    d_dat = step2$d_dat,
    d_att = step2$d_att,
    admap = step3$admap,
    admap_cell_wise = step4$admap_cell_wise,
    d_orig = d
  )
  
}
