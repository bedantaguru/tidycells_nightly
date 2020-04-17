# @Dev

detect_table_block <- function(dat, allow_corner = FALSE, ...){
  UseMethod("detect_table_block")
}

detect_table_block.cell_df <- function(dat, allow_corner = FALSE, ...){
  dat_gids <- get_group_id(dat, allow_corner = allow_corner, gid_tag = "tb_")
  dat_with_gid <- dat
  dat_with_gid$gid <- NULL
  dat_gids_map <- dat_gids %>% mutate(gid = gid %>% as.factor() %>% as.integer() %>% as_character())
  dat_with_gid <- dat_with_gid %>% left_join(dat_gids_map %>% select(row, col, gid), by = c("row", "col"))
  dat_with_gid
}

detect_table_block.Table_Field_Container <- function(dat, allow_corner = FALSE, ...){
  dat %>% map(~detect_table_block(.x, allow_corner = allow_corner)) %>% formalize_tfc()
}