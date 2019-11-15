# @Dev

detect_table_block <- function(dat, allow_corner = FALSE, ...){
  UseMethod("detect_table_block")
}

detect_table_block.cell_df <- function(dat, allow_corner = FALSE, ...){
  dat_gids <- get_group_id(dat, no_group_boundary = TRUE)
  if(allow_corner){
    dat_gids <- get_group_id(dat_gids$group_id_map, no_group_boundary = TRUE, allow_corner = TRUE)
  }
  dat_with_gid <- dat
  dat_with_gid$gid <- NULL
  dat_gids_map <- dat_gids$group_id_map %>% mutate(gid = gid %>% as.factor() %>% as.integer() %>% as.character())
  dat_with_gid <- dat_with_gid %>% left_join(dat_gids_map %>% select(row, col, gid), by = c("row", "col"))
  dat_with_gid
}

detect_table_block.Table_Field_Container <- function(dat, allow_corner = FALSE, ...){
  dat %>% map(~detect_table_block(.x, allow_corner = allow_corner)) %>% formalize_tfc()
}