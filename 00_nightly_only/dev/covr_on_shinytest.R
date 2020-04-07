

# shinytest:::sd_startShiny
# 
# shinytest::ShinyDriver 
# 

require(covr)
require(shinytest)

# testApp("00_nightly_only/shiny_test/exapp/", testnames = "crop")

shiny_coverage <- function(code) {
  requireNamespace("shiny", quietly = TRUE)
  
  registerShinyDebugHook <<- function(params) {
    replace(replacement(params$name, env = params$where))
  }
  
  message("Use Ctrl-C to interrupt the process when finished and then run `covr::shiny_results()` to retrieve the results")
  force(code)
}

shiny_results <- function() {
  on.exit(clear_counters())
  
  structure(as.list(.counters), class = "coverage")
}


reg.finalizer(asNamespace('tidycells'), function(...) { covr:::save_trace('C:/Users/NilSarmi/AppData/Local/Temp/RtmpcLCB95/shiny_test_app_dir2080741c30ba') })
reg.finalizer(asNamespace('shiny'), function(...) { covr:::save_trace('C:/Users/NilSarmi/AppData/Local/Temp/RtmpcLCB95/shiny_test_app_dir2080741c30ba') })

registerShinyDebugHook <<- function(params) {
  replace(replacement(params$name, env = params$where))
}


environment(shiny_coverage) <- environment(covr::code_coverage)
environment(shiny_results) <- environment(covr::code_coverage)

shinytest::testApp

Sys.setenv(NOT_CRAN = "true")
debug(covr::package_coverage)
covr::package_coverage()



