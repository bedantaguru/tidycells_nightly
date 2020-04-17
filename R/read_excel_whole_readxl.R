# readxl
# possible_date_range is somehow inspired by LibreOffice
read_excel_whole_part_readxl <- function(fn, sheet = 1L, possible_date_range = c(as.Date("1930-01-01"), Sys.Date() + 3800)) {
  if (!is_available("readxl")) {
    abort("'readxl' package is required")
  }

  suppressWarnings({
    d0 <- readxl::read_excel(fn,
      col_names = FALSE, col_types = "text", sheet = sheet, .name_repair = "minimal"
    )
    d1 <- readxl::read_excel(fn,
      col_names = FALSE, col_types = "date", sheet = sheet, .name_repair = "minimal"
    )
  })

  possible_date_range <- as_character(possible_date_range)

  d0f <- as.matrix(d0) %>% as_character()
  d1f <- as.matrix(d1) %>% as_character()
  d1f <- ifelse(d1f <= max(possible_date_range) & d1f >= min(possible_date_range), d1f, NA_character_)

  df <- ifelse(is.na(d1f), d0f, d1f)

  d <- matrix(df, nrow = nrow(d0))

  as.data.frame(d, stringsAsFactors = FALSE) %>%
    tibble::as_tibble()
}

# @Dev limiter needed
read_excel_whole_readxl <- function(fn) {
  if (!is_available("readxl")) {
    abort("'readxl' package is required")
  }
  sheets <- readxl::excel_sheets(fn)
  lout <- sheets %>% map(~ read_excel_whole_part_readxl(fn, sheet = .x)) %>% 
    # @Dev this is to match new behaviour
    # make sure all philosophy align properly.
    map(~as_cell_df(.x,take_col_names = F))
  names(lout) <- sheets
  lout
}
