
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.tidycells <- list(
    # this seems can be usable by other packages too
    AutoUnloadShiny = TRUE,
    # single setting for disabling all enhanced features
    tidycells.safemode = FALSE,
    # tidycells.plot_mode can be either of (auto, ggplot2, DT)
    tidycells.plot_mode = "auto",
    # tidycells.header_binder can be either of (tidycells, unpivotr)
    tidycells.header_binder = "tidycells"
  )
  toset <- !(names(op.tidycells) %in% names(op))
  if (any(toset)) options(op.tidycells[toset])
  
  tidycells_pkg_env$options_to_clean <- setdiff(names(op.tidycells), c("AutoUnloadShiny"))
  
  invisible()
}

# for cleanup of temp files / folders in any
.onDetach <- function(libpath){
  if(!is.null(tidycells_pkg_env$temp_files)){
    tidycells_pkg_env$temp_files %>% map(~unlink(.x, recursive = TRUE, force = TRUE))
  }
  
  if(!is.null(tidycells_pkg_env$options_to_clean)){
    bl <- rep(list(NULL), length(tidycells_pkg_env$options_to_clean))
    names(bl) <- tidycells_pkg_env$options_to_clean
    options(bl)
  }
  
  invisible()
} 
