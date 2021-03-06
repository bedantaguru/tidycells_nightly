
#' @section Get Started:
#'
#' To get started check out \href{../doc/tidycells-intro.html}{\code{vignette("tidycells-intro")}}.
#'
#' Alternatively, You can check out \href{A-Brief-Introduction.html}{A Brief Introduction} for
#' quick start (and useful information about how to utilize help for `tidycells`).
#'
#' @section Main Functions:
#'
#' Here is a glimpse of main functions from this package. (_There are other functionality available
#' which are briefed in \href{A-Brief-Introduction.html}{A Brief Introduction}._)
#' * [`as_cell_df`][as_cell_df()]
#' * [`value_attribute_classify`][value_attribute_classify()]
#' * [`analyze_cells`][analyze_cells()]
#' * [`compose_cells`][compose_cells()]
#' * [`collate_columns`][collate_columns()]
#'
#' And a function to combine all these (above listed) functionality:
#' * [`read_cells`][read_cells()]
#'
#' @name tidycells-package
#' @keywords internal
"_PACKAGE"


# pkg share global vars
# is tidycells_pkg_env which is in 000.R

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib tidycells, .registration = TRUE
## usethis namespace: end
NULL
