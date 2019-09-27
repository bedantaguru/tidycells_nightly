

current_state_of_pkgs <- function() {
  x <- utils::sessionInfo()
  lo <- list()
  lo$base <- sort(x$basePkgs)
  lo$ns_attached <- sort(names(x$otherPkgs))
  lo$ns_loaded <- sort(names(x$loadedOnly))
  lo
}
