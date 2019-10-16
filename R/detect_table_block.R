# @Dev

detect_table_block <- function(dat){
  UseMethod("detect_table_block")
}

detect_table_block.cell_df <- function(dat){
  dat_gids <- get_group_id(dat)
  dat_with_gid <- dat
  dat_with_gid$gid <- NULL
  dat_gids_map <- dat_gids$group_id_map %>% mutate(gid = gid %>% as.factor() %>% as.integer())
  dat_with_gid <- dat_with_gid %>% left_join(dat_gids_map %>% select(row, col, gid), by = c("row", "col"))
  dat_with_gid
}
