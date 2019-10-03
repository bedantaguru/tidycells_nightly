
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
tidycells_pkg_env <- new.env()

# used in --> is_available.R
assign("na_pkgs", NULL, envir = tidycells_pkg_env)

# used in --> file_kind_identifier.R
assign("file_kinds", NULL, envir = tidycells_pkg_env)

# used in --> file_type_from_magic_numbers.R
assign("magic_numbers", NULL, envir = tidycells_pkg_env)

# used in --> zzz.R  +  utils-etc.R
assign("temp_files", NULL, envir = tidycells_pkg_env)
