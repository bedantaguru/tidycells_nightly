

stitch_direction <- function(d_part, dat, attr_name, trace_it = FALSE) {
  d0 <- d_part %>%
    distinct(data_gid, row = row_d, col = col_d)

  a0 <- d_part %>%
    distinct(attr_gid, row = row_a, col = col_a)

  chkd <- d0$data_gid %>%
    unique() %>%
    length()
  
  chka <- a0$attr_gid %>%
    unique() %>%
    length()
  
  chkatsp <- d_part$attr_gid_split %>%
    unique() %>%
    length()

  chkdir <- d_part$header_orientation_tag %>%
    unique() %>%
    length()

  chkaty <- d_part$attr_group %>%
    unique() %>%
    length()

  if (chkd != 1 | chka != 1 | chkdir != 1 | chkatsp != 1 | chkaty != 1) {
    abort(paste("failed to stitch.",
      "(have you tampered a cell-analysis?)",
      "(make sure the cell-analysis is free from error or potential issues)",
      "If still the error persists, please contact the developer.",
      sep = "\n"
    ))
  }


  if (missing(attr_name)) {
    
    attr_name <- d_part$attr_var_sync_name[1]
  }



  direction <- d_part$header_orientation_tag[1]

  d0 <- d0 %>%
    select(-data_gid) %>%
    left_join(dat %>% select(row, col, value), by = c("row", "col"))
  a0 <- a0 %>%
    select(-attr_gid) %>%
    left_join(dat %>% select(row, col, attr = value), by = c("row", "col"))

  if (trace_it) {
    a0 <- a0 %>% mutate(cadd = paste(row, col, sep = "_"))
  }

  # suppressWarnings should be removed once unpivotr::enhead chages
  # this is happening as "All elements of `...` must be named." warning in tidyr
  # ref: https://github.com/tidyverse/tidyr/issues/714
  # ref: https://github.com/nacnudus/unpivotr/issues/26
  # directions NNW (and similar) is strict; it does not allow multi block
  # possibly need separate enhead like function
  suppressWarnings({
    d1 <- d0 %>%
      bind_header(a0, direction)
  })


  colnames(d1)[which(colnames(d1) == "attr")] <- attr_name

  if (trace_it) {
    colnames(d1)[which(colnames(d1) == "cadd")] <- paste0("cellAddress_", attr_name)
  }

  d1 <- d1 %>% mutate(data_block = d_part$natural_gid[1])

  d1
}
