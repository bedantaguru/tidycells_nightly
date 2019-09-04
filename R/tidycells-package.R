
#' @section Get Started: 
#' To get started check out \href{../doc/tidycells-intro.html}{\code{vignette("tidycells-intro")}}.
#'
#' @name tidycells-package
#' @keywords internal
"_PACKAGE"


# pkg share global vars
tidycells_pkg_env <- new.env()
# used in --> is_available.R
assign("na_pkgs", NULL, envir = tidycells_pkg_env)
