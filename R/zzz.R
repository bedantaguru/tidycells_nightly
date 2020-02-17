
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.tidycells <- list(
    # this seems can be usable by other packages too
    AutoUnloadShiny = TRUE,
    # single setting for disabling all enhanced features
    tidycells.safemode = FALSE,
    # tidycells.plot_mode can be either of (auto, ggplot2, DT)
    tidycells.plot_mode = "auto"
  )
  toset <- !(names(op.tidycells) %in% names(op))
  if (any(toset)) options(op.tidycells[toset])
  
  invisible()
}

# for cleanup of temp files / folders in any
.onDetach <- function(libpath){
  if(!is.null(tidycells_pkg_env$temp_files)){
    tidycells_pkg_env$temp_files %>% map(~unlink(.x, recursive = TRUE, force = TRUE))
  }
  invisible()
} 
