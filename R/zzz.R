
# for cleanup of temp files / folders in any
.onUnload <- function(libpath){
  if(!is.null(tidycells_pkg_env$temp_files)){
    tidycells_pkg_env$temp_files %>% map(~unlink(.x, recursive = TRUE, force = TRUE))
  }
  invisible()
} 
