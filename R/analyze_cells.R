


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
  if (!is_cell_df(d)) {
    abort("A Cell DF expected")
  }

  if (!hasName(d, "type")) {
    abort(paste("The type column not found.",
      "(You may like to do 'Value Attribute Classification'.",
      "Check basic_classifier, sample_based_classifier, numeric_values_classifier for details.",
      sep = "\n"
    ))
  }

  val <- validate_cells(d)
  if (!val) {
    abort(attr(val, "msg") %>% paste0(collapse = "\n"))
  }

  #  remove empty cells
  d_orig <- d
  d <- d %>% filter(type != "empty")

  # the term 'data' and 'value' are interchangeably used going forward
  data_cells <- d %>%
    filter(type == "value") %>%
    as_rc_df()

  attr_cells <- d %>%
    filter(type == "attribute") %>%
    as_rc_df()

  if (nrow(data_cells) == 0) {
    abort("No `value` cells found")
  }

  if (nrow(attr_cells) == 0) {
    abort("No `attribute` cells found")
  }

  d_dat <- get_group_id(data_cells, gid_tag = "d")
  d_att <- get_group_id(attr_cells, gid_tag = "a")

  #@Dev
  #  this function name may be changed
  step1 <- ai_get_data_attr_map_main(d_dat, d_att)

  d_att <- step1$d_att
  admap1 <- step1$admap

  # data_gid join (if possible)
  if (length(unique(d_dat$gid)) > 1) {
    d_dat_potential_joins <- ai_data_gid_join(
      d_dat, d_att, 
      data_attr_map = admap1,
      full_data = d %>% as_tibble()
    )
    
    if (d_dat_potential_joins$done) {
      # @Dev  need to fix
      # this means results has been invalidated
      browser()
      d_dat <- d_dat0

      # revert back to original form
      d_att <- d_att_orig

      step2 <- ai_get_data_attr_map_main(d_dat, d_att, crude_join = FALSE)

      d_dat <- step2$d_dat
      d_att <- step2$d_att
      admap1 <- step2$admap
    }
  }

  # join attr based on block merges (potentially possible)
  rel_chk <- ai_relative_data_join_attr(admap_main = admap1, d_att = d_att)
  if (rel_chk$done) {
    # this also need to fix
    browser()
    d_att <- rel_chk$d_att %>% map(unique)
    admap1 <- rel_chk$admap
  }

  # now time for corners (potential) / minor / less strong mappings
  # information blocks are introduced from here
  
  # @Dev check and delink functions
  # d_dat$group_id_extended_boundary <- extend_data_block(d_dat$group_id_boundary, admap1$map, d_att)

  info_block <- ai_get_information_blocks(admap1, d_dat, d_att)
  
  admap1 <- info_block$map
  
  d_inf <- info_block$d_inf
  
  
  
  unmapped_attr_gids <- d_att$gid %>% unique() %>% 
    setdiff(admap1$attr_gid) %>% 
    # @Dev
    # following may not be perfect as it discards the <missed_block_connections> $ <attr_gid> without seeeing
    # <data_gid> <- this is stale now (for this we may opt for <row, col> sample in <missed_block_connections>)
    # for now solving purpose but may be dropped later
    # 
    setdiff(common_knowledge("missed_block_connections")$attr_gid)

  if(length(unmapped_attr_gids)>0){
    
    # attach unmapped_attr_gids to info_blocks
    
    # fr: for rest attr_ids (this actually mean <unmapped_attr_gids> and can include corners and non-corners also)
    admap_fr0 <- ai_get_data_attr_map(
      dat_boundary = get_group_id_boundary(d_inf),
      att_gid_map = d_att %>% filter(gid %in% unmapped_attr_gids),
      attr_to_near_data = TRUE,
      leave_inside = TRUE
    )
    
    admap_fr1 <- admap_fr0$map %>%
      ai_get_dimention_analysis_details(d_inf, d_att, major_direction_relax = FALSE)
    
    admap_fr1 <- admap_fr1$map %>% 
      # since data_gid is dummy name for info_gid 
      rename(info_gid = data_gid) %>% 
      # data_gid has to be added to make it comparable to admap1
      inner_join(admap1 %>% distinct(data_gid, info_gid), by = "info_gid")
    
    
    unmapped_attr_gids <-
      admap1$attr_gid %>%
      c(admap_fr0$map$attr_gid) %>%
      setdiff(d_att$gid, .) %>%
      #@Dev
      # this is suffering from same problem mentioned earlier
      setdiff(common_knowledge("missed_block_connections")$attr_gid)
    
  }

  # merge two maps
  admap2 <- admap1 %>% bind_rows(admap_fr1)

  # @Dev
  # I think this should be moved to split ai case
  # following setp is required for: 
  # diffrent split resulted in same attr_gid
  # these will be compated to single group
  cmp <- compact_gid_maps(d_att, admap2)
  d_att <- cmp$d_att
  admap2 <- cmp$admap

  # @Dev
  # del it later
  # admap3 <- admap2 %>%
  #   select(-attr_group) %>%
  #   ai_get_dimention_analysis_details(d_dat, d_att)
  # 
  # if (!identical(admap3$map, admap2$map)) {
  #   # I think this can be iterated
  #   # KFL
  #   admap3_pass <- admap3$map %>%
  #     rename(md = dist) %>%
  #     group_by(data_gid, direction_group, attr_group) %>%
  #     mutate(m_dist = min(md)) %>%
  #     ungroup() %>%
  #     filter(md == m_dist) %>%
  #     select(-md) %>%
  #     rename(dist = m_dist)
  # 
  #   admap <- admap3_pass %>%
  #     select(-attr_group) %>%
  #     ai_get_dimention_analysis_details(d_dat, d_att)
  # } else {
  #   admap <- admap3
  # }

  
  admap <- admap2
  
  # @Dev
  # this may not be accurate 
  # one cell can be mapped in multiple manner
  # str-detection done
  this_cells <- get_cells_from_admap(admap, d_dat, d_att)

  # natural gid for easier user selection
  gid_ngid <- d_dat %>%
    distinct(gid) %>%
    mutate(natural_gid = gid %>% as.factor() %>% as.numeric() %>% paste0("d",.))

  # attach natural gid
  this_cells <- this_cells %>%
    left_join(gid_ngid, by = "gid")
  
  # attach directions to it
  admap_with_dir <- get_data_attr_cell_wise_map_raw(admap,d_dat, d_att) %>% 
    ai_attach_direction()
  
  # this need to be added after ai_attach_direction
  admap_with_dir <- admap_with_dir %>%
    left_join(gid_ngid, by = c("data_gid"="gid"))

  # @Dev
  #  issues to be covered differently or later
  # df_details <- get_definiteness_details(admap$raw_map,
  #   all_attr_gids = d_att$group_id_boundary$gid %>%
  #     setdiff(d_att$missed_blocks$gid)
  # )
  # definiteness_checks <- get_definiteness_checks(df_details, silent = silent)

  obj <- list(
    cells = this_cells,
    sections = get_group_id_boundary(d_dat) %>% left_join(gid_ngid, by = "gid"),
    details = list(
      attr_details = d_att,
      data_details = d_dat,
      data_attr_map_raw = admap_with_dir,
      definiteness_checks = NULL
    ),
    cell_df = d_orig
  )

  # attach cell_df_analysis class
  class(obj) <- cell_df_analysis_class

  obj
}
