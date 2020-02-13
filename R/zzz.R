
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.tidycells <- list(
    # this seems can be usable by other packages too
    AutoUnloadShiny = TRUE,
    # single setting for disabling all enhanced features
    tidycells.safemode = FALSE
  )
  toset <- !(names(op.tidycells) %in% names(op))
  if (any(toset)) options(op.tidycells[toset])
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  # experimental assitant framework # start
  #@Dev
  
  ok <- FALSE
  if (is_available("rstudioapi")) {
    if (rstudioapi::hasFun("viewer")) {
      if (rstudioapi::isAvailable()){
        ok <- TRUE
      }
    }
  }
  
  if(ok){
    #@Dev
    dir <- tempfile()
    dir.create(dir)
    htmlFile <- file.path(dir, "index.html")
    writeLines(
      '<h1 style="text-align: center;">Thank you for using <span style="color: #ff0000;">Tidycells</span></h1>
<p style="text-align: center;">It is an <span style="color: #ff6600;"><strong>assistant</strong> </span>for you</p>
<p style="text-align: center;">It is <span style="text-decoration: underline;">yet to <span style="color: #0000ff; text-decoration: underline;">evolve</span></span></p>',
      htmlFile
    )
    suppressWarnings(rstudioapi::viewer(htmlFile))
    # deletion not working >>> unlink(dir, recursive = TRUE)
    # it will be cleaned only when mother R process is restarted / stopped
  }
  
  # experimental assitant framework # end
}

# for cleanup of temp files / folders in any
.onDetach <- function(libpath){
  if(!is.null(tidycells_pkg_env$temp_files)){
    tidycells_pkg_env$temp_files %>% map(~unlink(.x, recursive = TRUE, force = TRUE))
  }
  invisible()
} 
