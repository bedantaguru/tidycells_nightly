
finalize_lo <- function(lo) {
  if (length(lo$type) == 0) {
    lo$type <- "unknown"
  }
  lo
}

make_shaft <- function(lo, lo_done = FALSE) {
  shaft_this <- list(
    shaft_done = lo_done,
    shaft_info = lo
  )
  shaft_this
}

shaft_html <- function(prior_shaft, lo, ignore_fty = FALSE) {
  lo_done <- FALSE

  if (!missing(prior_shaft)) {
    if (prior_shaft$shaft_done) {
      # do nothing if already done
      # return early
      return(prior_shaft)
    }

    # read info from prior_shaft
    lo <- prior_shaft$shaft_info
  }

  fn <- lo$file_name
  omit <- lo$omit

  if (!("html" %in% omit)) {
    if (("html" %in% lo$type) | ignore_fty) {
      if (is_available("XML")) {
        read_try <- try(XML::readHTMLTable(fn, header = FALSE), silent = TRUE)
        if (inherits(read_try, "try-error")) read_try <- NULL
        if (length(read_try) != 0) {
          lo$type <- "html"
          lo$used_function <- "XML::readHTMLTable"
          lo$content <- read_try
          # decision done
          lo <- finalize_lo(lo)
          lo_done <- TRUE
        } else {
          # not html
          lo$type <- setdiff(lo$type, "html")
        }
      }
    }
  }

  make_shaft(lo, lo_done)
}

shaft_csv <- function(prior_shaft, lo, ignore_fty = FALSE) {
  lo_done <- FALSE

  if (!missing(prior_shaft)) {
    if (prior_shaft$shaft_done) {
      # do nothing if already done
      # return early
      return(prior_shaft)
    }

    # read info from prior_shaft
    lo <- prior_shaft$shaft_info
  }

  fn <- lo$file_name
  omit <- lo$omit

  if (!("csv" %in% omit)) {
    if (("csv" %in% lo$type) | ignore_fty) {
      if (is_available("readr")) {
        # @Dev need to create auto melt type detect here
        # also possible need to rename the file
        # e.g. consider the case when file is like abc.gz but actually a csv
        # tested and working for gz
        read_try <- try(readr::melt_csv(fn), silent = TRUE)
        if (inherits(read_try, "try-error")) read_try <- NULL
        if (is.data.frame(read_try)) {
          lo$type <- "csv"
          lo$used_function <- "readr::melt_csv"
          lo$content <- read_try
          # decision done
          lo <- finalize_lo(lo)
          lo_done <- TRUE
        } else {
          # not csv type
          lo$type <- setdiff(lo$type, "csv")
        }
      }
    }
  }

  make_shaft(lo, lo_done)
}

shaft_csv_utils <- function(prior_shaft, lo, ignore_fty = FALSE) {
  lo_done <- FALSE

  if (!missing(prior_shaft)) {
    if (prior_shaft$shaft_done) {
      # do nothing if already done
      # return early
      return(prior_shaft)
    }

    # read info from prior_shaft
    lo <- prior_shaft$shaft_info
  }

  fn <- lo$file_name
  omit <- lo$omit



  if (!("csv{utils}" %in% omit)) {
    if (("csv" %in% lo$type) | ignore_fty) {
      read_try <- try(utils::read.csv(fn, header = FALSE, nrows = 1), silent = TRUE)
      if (inherits(read_try, "try-error")) read_try <- NULL

      if (is.data.frame(read_try)) {
        lo$type <- "csv"
        lo$used_function <- "utils::read.csv"

        # read full
        lo$content <- utils::read.csv(fn, header = FALSE)
        # decision done
        lo <- finalize_lo(lo)
        lo_done <- TRUE
      } else {
        # not readable by base read.csv [mostly will never happen]
        lo$type <- setdiff(lo$type, "csv{utils}")
      }
    }
  }

  make_shaft(lo, lo_done)
}

shaft_doc <- function(prior_shaft, lo, ignore_fty = FALSE, silent = FALSE) {
  lo_done <- FALSE

  if (!missing(prior_shaft)) {
    if (prior_shaft$shaft_done) {
      # do nothing if already done
      # return early
      return(prior_shaft)
    }

    # read info from prior_shaft
    lo <- prior_shaft$shaft_info
  }

  fn <- lo$file_name
  omit <- lo$omit


  if (!("doc" %in% omit)) {
    if (("doc" %in% lo$type) | ignore_fty) {
      if (is_available("docxtractr")) {
        if (lo$ext != "doc") {
          # need to rename the file as docxtractr detects by ext name
          tf <- tempfile(fileext = ".doc")
          file.copy(fn, tf, overwrite = TRUE)
          remove_at_end <- TRUE
        } else {
          tf <- fn
          remove_at_end <- FALSE
        }

        if (!silent) {
          if (detect_LibreOffice()) {
            message(paste0(
              "LibreOffice is present ",
              "(please wait as it may take some time to read/detect tables from possible doc file).",
              "(If it is too slow try opening LibreOffice outside this R-Session and retry)",
              "\nNote: If you want you may disable doc detection by setting omit = \"doc\"."
            ))
          } else {
            message("LibreOffice may be required for possible doc files. Check docxtractr::read_docx documentation")
          }
        }

        read_try <- suppressWarnings(suppressMessages(try(docxtractr::read_docx(tf), silent = TRUE)))
        if (inherits(read_try, "try-error")) read_try <- NULL

        if (inherits(read_try, "docx")) {
          lo$type <- "doc"
          lo$used_function <- "#possibly renamed. >>> docxtractr::read_docx >>> docxtractr::docx_extract_all_tbls"

          # read full data
          suppressWarnings(
            suppressMessages(
              ctl <- docxtractr::docx_extract_all_tbls(read_try, guess_header = FALSE)
            )
          )

          if (is.null(names(ctl))) {
            names(ctl) <- seq_along(ctl) %>% paste0("Table_", .)
          }


          lo$content <- ctl

          # decision done
          lo <- finalize_lo(lo)
          lo_done <- TRUE
        } else {
          lo$type <- setdiff(lo$type, "doc")
        }

        if (remove_at_end) {
          unlink(tf, recursive = TRUE)
        }
      }
    }
  }

  make_shaft(lo, lo_done)
}

shaft_docx <- function(prior_shaft, lo, ignore_fty = FALSE) {
  lo_done <- FALSE

  if (!missing(prior_shaft)) {
    if (prior_shaft$shaft_done) {
      # do nothing if already done
      # return early
      return(prior_shaft)
    }

    # read info from prior_shaft
    lo <- prior_shaft$shaft_info
  }

  fn <- lo$file_name
  omit <- lo$omit

  if (!("docx" %in% omit)) {
    if (("docx" %in% lo$type) | ignore_fty) {
      if (is_available("docxtractr")) {
        if (lo$ext != "docx") {
          # need to rename the file as docxtractr detects by ext name
          tf <- tempfile(fileext = ".docx")
          file.copy(fn, tf, overwrite = TRUE)
          remove_at_end <- TRUE
        } else {
          tf <- fn
          remove_at_end <- FALSE
        }

        read_try <- suppressWarnings(suppressMessages(try(docxtractr::read_docx(tf), silent = TRUE)))
        if (inherits(read_try, "try-error")) read_try <- NULL

        if (inherits(read_try, "docx")) {
          lo$type <- "docx"
          lo$used_function <- "#possibly renamed. >>> docxtractr::read_docx >>> docxtractr::docx_extract_all_tbls"

          # read full data
          suppressWarnings(
            suppressMessages(
              ctl <- docxtractr::docx_extract_all_tbls(read_try, guess_header = FALSE)
            )
          )

          if (is.null(names(ctl))) {
            names(ctl) <- seq_along(ctl) %>% paste0("Table_", .)
          }


          lo$content <- ctl

          # decision done
          lo <- finalize_lo(lo)
          lo_done <- TRUE
        } else {
          lo$type <- setdiff(lo$type, "docx")
        }

        if (remove_at_end) {
          unlink(tf, recursive = TRUE)
        }
      }
    }
  }

  make_shaft(lo, lo_done)
}

shaft_xls <- function(prior_shaft, lo, ignore_fty = FALSE) {
  lo_done <- FALSE

  if (!missing(prior_shaft)) {
    if (prior_shaft$shaft_done) {
      # do nothing if already done
      # return early
      return(prior_shaft)
    }

    # read info from prior_shaft
    lo <- prior_shaft$shaft_info
  }

  fn <- lo$file_name
  omit <- lo$omit


  if (!("xls" %in% omit)) {
    if (("xls" %in% lo$type) | ignore_fty) {
      if (is_available("xlsx")) {
        read_try <- suppressMessages(try(xlsx::loadWorkbook(fn), silent = TRUE))

        if (inherits(read_try, "try-error")) read_try <- NULL
        if (inherits(read_try, "jobjRef")) {
          lo$type <- "xls"
          lo$used_function <- "#customized xlsx based function. >>> tidycells:::read_xls_from_xlsx"
          # re read full data
          lo$content <- read_xls_from_xlsx(fn)
          # decision done
          lo <- finalize_lo(lo)
          lo_done <- TRUE
        } else {
          lo$type <- setdiff(lo$type, "xls")
        }
      }
    }
  }

  make_shaft(lo, lo_done)
}

# this can be used for xlsx also (but with less reliability)
shaft_xls_readxl <- function(prior_shaft, lo, ignore_fty = FALSE, silent = FALSE) {
  lo_done <- FALSE

  if (!missing(prior_shaft)) {
    if (prior_shaft$shaft_done) {
      # do nothing if already done
      # return early
      return(prior_shaft)
    }

    # read info from prior_shaft
    lo <- prior_shaft$shaft_info
  }

  fn <- lo$file_name
  omit <- lo$omit


  if (!("xls{readxl}" %in% omit)) {
    if (("xls" %in% lo$type) | ignore_fty) {
      if (is_available("readxl")) {
        if (!silent) {
          message("Using readxl to read xls. Manually check date and numeric cells. (for better result install xlsx package)")
        }

        read_try <- suppressMessages(try(readxl::read_excel(fn, n_max = 1), silent = TRUE))

        if (inherits(read_try, "try-error")) read_try <- NULL

        if (is.data.frame(read_try)) {
          # option open for xls and xlsx both
          lo$type <- readxl::format_from_signature(fn)
          lo$used_function <- "#customized readxl based function. >>> tidycells:::read_excel_whole_readxl"
          # re read full data
          lo$content <- read_excel_whole_readxl(fn)
          # decision done
          lo <- finalize_lo(lo)
          lo_done <- TRUE
        } else {
          this_type <- readxl::format_from_signature(fn)
          if (is.na(this_type)) this_type <- "xls"
          lo$type <- setdiff(lo$type, this_type)
        }
      }
    }
  }

  make_shaft(lo, lo_done)
}

shaft_xlsx <- function(prior_shaft, lo, ignore_fty = FALSE) {
  lo_done <- FALSE

  if (!missing(prior_shaft)) {
    if (prior_shaft$shaft_done) {
      # do nothing if already done
      # return early
      return(prior_shaft)
    }

    # read info from prior_shaft
    lo <- prior_shaft$shaft_info
  }

  fn <- lo$file_name
  omit <- lo$omit

  if (!("xlsx" %in% omit)) {
    if (("xlsx" %in% lo$type) | ignore_fty) {
      if (is_available("tidyxl")) {
        sheets_try <- try(tidyxl::xlsx_sheet_names(fn), silent = TRUE)

        if (!inherits(sheets_try, "try-error")) {
          lo$type <- "xlsx"

          # read it full
          lo$content <- tidyxl::xlsx_cells(fn) %>% split(.$sheet)
          # decision done
          lo <- finalize_lo(lo)
          lo_done <- TRUE
        } else {
          lo$type <- setdiff(lo$type, "xlsx")
        }
      }
    }
  }

  make_shaft(lo, lo_done)
}

shaft_pdf <- function(prior_shaft, lo, ignore_fty = FALSE) {
  lo_done <- FALSE

  if (!missing(prior_shaft)) {
    if (prior_shaft$shaft_done) {
      # do nothing if already done
      # return early
      return(prior_shaft)
    }

    # read info from prior_shaft
    lo <- prior_shaft$shaft_info
  }

  fn <- lo$file_name
  omit <- lo$omit

  if (!("pdf" %in% omit)) {
    if (("pdf" %in% lo$type) | ignore_fty) {
      if (is_available("tabulizer")) {
        suppressWarnings(
          suppressMessages(
            read_meta_try <- try(tabulizer::extract_metadata(fn), silent = TRUE)
          )
        )
        if (inherits(read_meta_try, "try-error")) read_meta_try <- NULL

        if (length(read_meta_try) == 0) read_meta_try <- NULL

        if (is.list(read_meta_try)) {
          lo$type <- "pdf"

          read_try <- suppressWarnings(
            suppressMessages(
              read_meta_try <- try(read_pdf_from_tabulizer(fn), silent = TRUE)
            )
          )

          if (length(read_try) > 0) {
            if (is.null(names(read_try))) {
              names(read_try) <- seq_along(read_try) %>% paste0("Table_", .)
            }
          }

          lo$content <- read_try

          # decision done
          lo <- finalize_lo(lo)
          lo_done <- TRUE
        } else {
          lo$type <- setdiff(lo$type, "pdf")
        }
      }
    }
  }

  make_shaft(lo, lo_done)
}
