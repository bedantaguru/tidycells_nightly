
this_file_ext <- function(fn) {
  x <- stringr::str_split(fn, "\\.")

  x %>% map_chr(~ {
    if (length(.x) == 1) {
      ""
    } else {
      rev(.x)[1]
    }
  })
}

is_txt_file <- function(fn) {

  # only to be carried out after magic number based detection
  # as gz, tar, zip files are uncompressed (mostly!) by readLines
  # por files are also detected as text

  # initial estimate
  # very basic implementation of  https://github.com/fralonra/isbinary/blob/master/isbinary.js
  # pre check
  encoded_txt_magics <- list(
    # UTF-8 BOM
    as.raw(c(0xef, 0xbb, 0xbf)),
    # UTF-32 BOM
    as.raw(c(0x00, 0x00, 0xfe, 0xff)),
    # UTF-32 LE BOM
    as.raw(c(0xff, 0xfe, 0x00, 0x00)),
    # GB BOM
    as.raw(c(0x84, 0x31, 0x95, 0x33)),
    # UTF-16 BE BOM
    as.raw(c(0xfe, 0xff)),
    # UTF-16 LE BOM
    as.raw(c(0xff, 0xfe))
  )

  fbytes <- readBin(fn, n = encoded_txt_magics %>% map_int(length) %>% max(), what = "raw")

  chk1 <- mn_check(fbytes, encoded_txt_magics)

  if (!chk1) {
    # read more
    fbytes <- readBin(fn, n = 512, what = "raw")
    # look for null bytes
    chk1 <- !any(as.integer(fbytes) == 0)
  }

  txt <- chk1
  embnul_chk <- tryCatch(readLines(fn, n = 100, warn = TRUE), warning = function(e) e)
  if (inherits(embnul_chk, "warning")) {
    if (any(stringr::str_detect(embnul_chk$message, "embedded nul"))) {
      txt <- FALSE
    }
  }
  txt
}

common_file_error <- function(fn, silent = TRUE, file_size_th = 1024^3) {
  if (missing(fn)) {
    abort("No file name given")
  }
  if (length(fn) != 1) {
    abort("only one file name supported")
  }
  if (!file.exists(fn)) {
    abort("file does not exists (or possibly you do not have permission)")
  }
  if (!silent) {
    f_size <- as.numeric(file.info(fn)["size"])
    if (f_size > file_size_th) {
      warn(
        paste0(
          "The file is huge! ",
          round(f_size / 1024^3, 1), " GB (approx).",
          "\nTry to opt for manual procedures or grammatical ways of cleaning / reading the data.",
          "\nThe automated methods may be quite slow."
        )
      )
    }
  }
}

detect_file_type <- function(fn) {
  if (length(fn) > 1) {
    map_chr(fn, detect_file_type)
  } else {
    common_file_error(fn)
    fmnty <- file_type_from_magic_numbers(fn)
    if ("unknown" %in% fmnty) {
      if (is_txt_file(fn)) {
        # detect text file later only
        fmnty <- "text"
      }
    }
    fmnty
  }
}
