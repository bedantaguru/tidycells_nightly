

#' Compose a Cell Analysis to a tidy form
#'
#' @description After [`analyze_cells`][analyze_cells()] carried out, you may like to use this function in order to
#' stitch the cells together as per the analyzed results, to form a meaningful structural representation (like tidy format).
#'
#' @param ca a cell_analysis to process
#' @param post_process logical scalar. If disabled a list will be returned without performing post-processing. (Default `TRUE`)
#' @param attr_sep a character string to separate the attributes. (Default is `<space>::<space>`)
#' @param discard_raw_cols logical scalar. If enabled only main processed columns will be returned. (Default `FALSE`)
#' @param print_attribute_overview print the overview of the attributes (4 distinct values from each attribute of each block)
#' @param silent whether to suppress warning message on compose failure (Default `FALSE`)
#'
#' @return a data.frame (as tibble) in tidy form.
#' @export
#'
#' @examples
#' cd <- 1:(9) %>%
#'   matrix(nrow = 3) %>%
#'   as_cell_df()
#' cd <- sample_based_classifier(cd, attribute_sample = "1")
#' cd <- cd %>% dplyr::filter(value != "1")
#' ca <- analyze_cells(cd)
#'
#' compose_cells(ca)
compose_cells <- function(ca, post_process = TRUE,
                          attr_sep = " :: ",
                          discard_raw_cols = FALSE,
                          print_attribute_overview = FALSE,
                          silent = FALSE) {
  compose_cells_raw(
    ca = ca,
    post_process = post_process,
    attr_sep = attr_sep,
    discard_raw_cols = discard_raw_cols,
    print_col_info = print_attribute_overview,
    silent = silent
  )
}

compose_cells_raw <- function(ca, post_process = TRUE, attr_sep = " :: ",
                              discard_raw_cols = FALSE,
                              trace_it_back = FALSE,
                              details = FALSE,
                              print_col_info = FALSE,
                              silent = FALSE,
                              ask_user = TRUE) {
  if (!inherits(ca, "cell_analysis")) {
    abort("A 'Cell Analysis' expected.")
  }

  # dam : Data Attr Map
  dam <- ca$details$data_attr_map_raw

  dcomp00 <- dam %>%
    group_by(data_gid, attr_micro_gid) %>%
    group_split()
  
  cdf <- ca$cell_df %>% filter(type!="empty")

  dcomp0 <- dcomp00 %>%
    map(~ {
      # this try should be removed if unpivotr::enhead is internalized
      # or similar behaving fucntions is developed.
      e <- try(stitch_direction(.x, cdf, trace_it = trace_it_back), silent = TRUE)
      .ok <- !inherits(e, "try-error")
      .d <- NULL
      if (!.ok) .d <- .x
      list(ok = .ok, out = e, dat = .d)
    })

  chk0 <- dcomp0 %>%
    map_lgl(~ !.x$ok) %>%
    any()

  if (chk0) {
    if (!silent) {
      # Need to show user what has been missed
      warn(paste0(
        "Some attributes (possibly minor only) failed to compose.",
        "\nCheck whether output is as expected.",
        "\nYou can disable this by setting silent = TRUE."
      ))
      
    }
  }

  dcomp0 <- dcomp0 %>% map_lgl(~ .x$ok) %>%
    dcomp0[.] %>%
    map(~ .x$out)

  chk1 <- length(dcomp0)

  if (chk1 > 0) {
    dblks <- dcomp0 %>% map_chr(~.x$data_block[1])
    
    dcomp <- unique(dblks) %>% map(~{
      dblk <- .x
      dcomp0[dblks==dblk] %>% reduce(full_join, by = c("row", "col", "value", "data_block"))
    })
  } else {
    abort("Failed to compose")
  }


  if (print_col_info) {
    dlinf <- dcomp %>% map(get_all_col_representative, cut_th = 4, lower_it = FALSE)

    dlinfc <- dlinf %>% map(~ .x %>% purrr::imap_chr(~ paste0("  ", cli_bb(.y), "\n     ", paste0(cli_g(.x), collapse = ", "))))
    names(dlinfc) <- paste0("data_block = ", seq_along(dlinfc))

    xmsg <- dlinfc %>%
      purrr::imap_chr(~ paste0(cli_br(.y), "\n", paste0(.x, collapse = "\n"))) %>%
      paste0(collapse = "\n")

    cat(paste0(xmsg, "\n\n"))
  }

  if (!post_process) {
    class(dcomp) <- composed_list_class
    return(invisible(dcomp))
  }

  # @Dev
  #compose_cells_raw_post_process(dcomp, details = details, discard_raw_cols = discard_raw_cols, attr_sep = attr_sep)
  
  dout <- bind_rows(dcomp)
  # @DFOut
  class(dout) <- composed_df_class
  dout
}

compose_cells_raw_post_process <- function(dcomp, details = FALSE, discard_raw_cols = FALSE, attr_sep = " :: ") {
  cns <- dcomp %>%
    map(colnames) %>%
    unlist() %>%
    unique()
  cns_trace <- cns[stringr::str_detect(cns, "cellAddress_")]
  cns <- cns %>% setdiff(cns_trace)
  cns_base <- c("row", "col", "data_block", "value")
  cns <- cns %>% setdiff(cns_base)

  cns_d <- tibble(cname = cns, cn = cns) %>%
    tidyr::separate(cn, into = c("ag", "rc", "dir", "ad", "d"))


  cns_d <- cns_d %>%
    # anticlockwise
    mutate(dir_n = recode(dir,
      top = 1,
      topLeft = 2,
      left = 3,
      bottomLeft = 4,
      bottom = 5,
      bottomRight = 6,
      right = 7,
      topRight = 8
    )) %>%
    mutate(rc_n = recode(rc,
      row = 1,
      col = 2,
      corner = 3
    )) %>%
    mutate(cname_ord = paste(rc_n, dir_n, ad, d, sep = "_"))



  dcomp_r <- dcomp %>%
    map(~ refine_cols(.x, cn_df = cns_d, sep = attr_sep)) %>%
    bind_rows()

  #  add rc_df class
  class(dcomp_r) <- c(class(dcomp_r), "rc_df") %>% unique()

  this_cols <- colnames(dcomp_r)
  f_cols <- c("row", "col", "data_block", "value")
  this_cols <- this_cols %>% setdiff(f_cols)
  nm_cols <- this_cols[stringr::str_detect(this_cols, "row|col|corner")]
  m_cols <- this_cols %>% setdiff(nm_cols)

  if (details) {
    lo <- list(raw_data = dcomp_r, must_cols = f_cols, major_col = m_cols, minor_col = nm_cols)
    return(lo)
  }

  if (discard_raw_cols) {
    dcomp_r[c(f_cols, m_cols)]
  } else {
    dcomp_r[c(f_cols, m_cols, nm_cols)]
  }
}


refine_cols <- function(dcomp_part, cn_df, sep = " :: ") {
  cn_df <- cn_df %>% filter(cname %in% colnames(dcomp_part))
  out <- dcomp_part
  if (nrow(cn_df) > 0) {
    cn_df <- cn_df %>%
      group_by(ag) %>%
      mutate(cname_new = rc_n %>% as.factor() %>% as.integer() %>% paste0(ag, "_", .)) %>%
      ungroup() %>%
      arrange(cname_new, cname_ord)
    
    cn_l <- cn_df %>%
      select(cname, cname_new) %>%
      split(.$cname_new)
    
    out_p <- cn_l %>%
      map(~ {
        .d0 <- dcomp_part[.x$cname]
        .d0 <- .d0 %>% mutate_all(stringr::str_trim)
        dcomp_part[.x$cname_new[1]] <- .d0 %>% apply(1, paste, collapse = sep)
        dcomp_part[c("row", "col", .x$cname_new[1])]
      }) %>%
      reduce(fj, join_by = c("row", "col"))
    
    out <- out_p %>% fj(dcomp_part, join_by = c("row", "col"))
  }
  
  out
}
