
#' @importFrom utils hasName


detect_cell_df_pattern_generic <- function(dat){
  ans <- F
  chk1 <- hasName(dat, "row") & hasName(dat, "col")
  
  if(chk1 & ncol(dat)>= 4 & nrow(dat)>0 & is_conforms_to_rcdf(dat)){
    # has at least two other columns
    small_d <- dat[1:min(nrow(dat), 50),] %>% select(-row, -col) 
    chk1.1 <- small_d %>% map_lgl(is.character) %>% any()
    dchk_arr1.2 <- small_d %>% map_int(~setdiff(.x, colnames(small_d)) %>% length)
    chk1.2 <- any(dchk_arr1.2==0)
    if(chk1.1 & chk1.2){
      ans <- T
      attr(ans, "dtype") <- names(which(dchk_arr1.2==0))
    }
  }
  ans
}

detect_cell_df_pattern <- function(dat) {

  # this is built as per the description of the return values from supported packages
  # supported packages / functions :
  # 1) tidyxl::xlsx_cells
  # 2) unpivotr::as_cells
  # 3) readr::melt_csv (and family)
  # (following is not detected here)
  # 4) generic_type (row , col, <some_name>, <some_column>)

  chk <- tibble(
    type = c(
      "tidyxl",
      "unpivotr",
      "readr",
      "cell_df"
    ),

    col_names = list(
      c("sheet", "address", "row", "col", "is_blank", "data_type", "error", "logical", "numeric", "date", "character"),
      c("row", "col", "data_type"),
      c("row", "col", "data_type", "value"),
      c("row", "col", "data_type", "value")
    ),

    data_types = list(
      c("error", "logical", "numeric", "date", "character", "blank"),
      c("chr", "cplx", "cplx", "dbl", "fct", "int", "lgl", "list", "ord"),
      c("integer","double", "character", "date"),
      c("numeric", "character")
    ),

    optional_cols = list(
      c("sheet", "error", "logical", "numeric", "date", "character", "blank"),
      c("chr", "cplx", "cplx", "dbl", "fct", "int", "lgl", "list", "ord"),
      c(),
      c()
    )
  )

  if (!hasName(dat, "data_type")) {
    # all of them has data_type
    return("unknown")
  }

  d_type <- chk %>%
    mutate(
      ccn = col_names %>% map_lgl(~ hasName(dat, .x) %>% all()),
      cdt = data_types %>% map_int(~ (.x %in% dat$data_type) %>% sum()),
      coc = optional_cols %>% map_int(~ hasName(dat, .x) %>% sum())
    ) %>%
    filter(ccn, cdt>0) %>%
    filter(cdt == max(cdt), coc == max(coc)) %>%
    pull(type)

  if (length(d_type) == 0) {
    return("unknown")
  }

  if (length(d_type) > 1) {
    d_type <- d_type[1]
  }

  d_type
}


attach_intermediate_class <- function(dat) {
  p1 <- detect_cell_df_pattern(dat)
  if(p1=="unknown"){
    # see if generic type pattern is present
    chk2 <- detect_cell_df_pattern_generic(dat)
    if(chk2){
      p1 <- "generic_type"
      attr(dat,"tidycells.generic_type_dtype") <-attr(chk2, "dtype") 
    }
  }
  class(dat) <- c(class(dat), p1) %>% unique()

  dat
}
