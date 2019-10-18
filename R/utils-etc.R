
this_temp_file <- function(pattern = "tc_file", tmpdir = tempdir(check = TRUE), fileext = ""){
  tf <-  tempfile(pattern = pattern, tmpdir = tmpdir, fileext = fileext)
  tidycells_pkg_env$temp_files <- c(tidycells_pkg_env$temp_files, tf) %>% unique()
  tf
}

rem_temp_file <- function(tf){
  tidycells_pkg_env$temp_files <- setdiff(tidycells_pkg_env$temp_files, tf) %>% unique()
  unlink(tf, recursive = TRUE, force = TRUE)
}
