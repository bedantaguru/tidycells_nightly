

decompress_file <- function(src_fkind){
  fn <- src_fkind$name
  class(fn) <- c(paste0("file_", src_fkind$file_type),class(fn))
  tdir <- this_temp_file(pattern = "decompress")
  unlink(tdir, recursive = TRUE, force = TRUE)
  dir.create(tdir, showWarnings = FALSE)
  try(decompress(fn, tdir), silent = TRUE)
  fkout <- file_kind_identifier(tdir, temporary_file = TRUE)
  fkout
}
