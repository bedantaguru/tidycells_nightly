
#' @include tidycells-package.R

# `file_type_from_magic_numbers` is designed for cases which
# 1. **wand** fails to detect and still required by **tidycells**
# 2. wand is not available etc.
# 3. as of now all required files are covered by **tidycells** alone.
# 4. wand will indicate other formats only
# 5. may drop wand in future versions

# this hosts functions like mn_* this means magic_number_*

file_type_from_magic_numbers <- function(filename) {
  fbytes <- readBin(filename, n = tidycells_pkg_env$magic_numbers$mx_length, what = "raw")
  mns_this <- tidycells_pkg_env$magic_numbers
  mns_this <- mns_this[stringr::str_detect(names(mns_this), "_magic$")]
  mn_chk_res <- mns_this %>% map_lgl(~ mn_check(fbytes, .x))
  mn_chk_res_ftypes <- mn_chk_res[mn_chk_res] %>%
    names() %>%
    stringr::str_remove_all("^file_") %>%
    stringr::str_remove_all("_magic$")
  ftype <- mn_chk_res_ftypes

  if ("xls_doc" %in% mn_chk_res_ftypes) {
    # check further for doc xls ppt
    micro_lvl <- check_office_ole(fbytes, filename)
    if (!is.null(micro_lvl)) {
      ftype <- micro_lvl
    }
  }

  if ("xlsx_docx" %in% mn_chk_res_ftypes) {
    # check further for doc xls ppt
    micro_lvl <- wand_check_office_clone(fbytes, filename)
    if (!is.null(micro_lvl)) {
      ftype <- micro_lvl
    }
  }

  if (length(ftype) > 1) {
    ftype <- ftype[1]
  }

  if (length(ftype) == 0) {
    ftype <- "unknown"
  }

  if (!is.character(ftype)) {
    ftype <- "unknown"
  }

  ftype
}

mn_check <- function(bytes, pre_def_mn) {
  if (is.list(pre_def_mn)) {
    for (i in 1:length(pre_def_mn)) {
      chk <- mn_check(bytes, pre_def_mn[[i]])
      if (chk) break()
    }
    chk
  } else {
    pstart_this <- this_get_pstart_for_magic_numbers(pre_def_mn)
    pend_this <- length(pre_def_mn) + pstart_this - 1
    isTRUE(all.equal(bytes[pstart_this:pend_this], pre_def_mn, check.attributes = FALSE))
  }
}

# starting PAT and functional length for check
# to handle byte offset
this_get_pstart_for_magic_numbers <- function(x, get_length = FALSE) {
  pat <- 0L
  if (is.null(attr(x, "pstart"))) {
    pat <- 1L
  } else {
    pat <- as.integer(attr(x, "pstart"))
  }
  if (get_length) {
    return(as.integer(length(x) + pat - 1))
  }
  pat
}

this_domain_magic_numbers <- function() {
  mns <- list(mx_length = 0, file_types = NULL)

  # as xls and doc both have same magic number "D0 CF 11 E0 A1 B1 1A E1"
  # many other have it though
  # ref : https://asecuritysite.com/forensics/magic
  mns$xls_doc_magic <- as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1))
  # these are kept for implementation status checks
  mns$xls_magic <- mns$xls_doc_magic
  mns$doc_magic <- mns$xls_doc_magic
  mns$ppt_magic <- mns$xls_doc_magic
  # either xlsx or docx
  # as xlsx and docx both have same magic number "50 4B 03 04"
  mns$xlsx_docx_magic <- list(
    # check https://www.filesignatures.net/index.php?page=search&search=504B030414000600&mode=SIG
    as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x14, 0x00, 0x06, 0x00)),
    as.raw(c(0x50, 0x4b, 0x03, 0x04))
  )

  # these are kept for implementation status checks
  mns$xlsx_magic <- mns$xlsx_docx_magic
  mns$docx_magic <- mns$xlsx_docx_magic
  mns$pptx_magic <- mns$xlsx_docx_magic

  mns$pdf_magic <- as.raw(c(0x25, 0x50, 0x44, 0x46))


  # this is taken from
  # https://github.com/cran/sas7bdat/blob/c8afa85b104d7289a80e7da9a1f596f29c6d9e4a/R/sas7bdat.R#L269
  # also check
  # https://github.com/BioStatMatt/sas7bdat
  # https://github.com/dataiku/sassyreader/blob/master/src/test/java/org/eobjects/metamodel/sas/SasReaderTestBase.java
  mns$sas_magic <- as.raw(c(
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0xc2, 0xea, 0x81, 0x60,
    0xb3, 0x14, 0x11, 0xcf, 0xbd, 0x92, 0x8, 0x0,
    0x9, 0xc7, 0x31, 0x8c, 0x18, 0x1f, 0x10, 0x11
  ))

  mns$sav_magic <- list(
    # this is taken from https://www.garykessler.net/library/file_sigs.html and https://tool.lu/ja_JP/magicbytes/
    as.raw(c(
      0x24, 0x46, 0x4c, 0x32, 0x40, 0x28, 0x23, 0x29, 0x20,
      0x53, 0x50, 0x53, 0x53, 0x20, 0x44, 0x41, 0x54, 0x41, 0x20, 0x46,
      0x49, 0x4c, 0x45
    )),
    # this is taken from https://github.com/s0md3v/Dump/blob/master/static/file-signatures.json and https://www.filesignatures.net/index.php?search=sav&mode=EXT
    as.raw(c(0x24, 0x46, 0x4c, 0x32, 0x40, 0x28, 0x23, 0x29))
  )

  # this is taken from https://www.loc.gov/preservation/digital/formats/fdd/fdd000469.shtml
  mns$zsav_magic <- as.raw(c(0x24, 0x46, 0x4c, 0x33))

  mns$por_magic <- list(
    # this is taken from https://www.loc.gov/preservation/digital/formats/fdd/fdd000468.shtml
    as.raw(c(
      0xc1, 0xe2, 0xc3, 0xc9, 0xc9, 0x40, 0xe2, 0xd7, 0xe2,
      0xe2, 0x40, 0xd7, 0xd6, 0xd9, 0xe3, 0x40, 0xc6, 0xc9, 0xd3, 0xc5
    )),
    # this is taken from https://www.garykessler.net/library/magic.html
    as.raw(c(0xc1, 0xe2, 0xc3, 0xc9))
  )

  # this is taken from https://www.loc.gov/preservation/digital/formats/fdd/fdd000471.shtml
  mns$dta_magic <- as.raw(c(
    0x3c, 0x73, 0x74, 0x61, 0x74, 0x61, 0x5f, 0x64, 0x74,
    0x61, 0x3e, 0x3c, 0x68, 0x65, 0x61, 0x64, 0x65, 0x72, 0x3e, 0x3c,
    0x72, 0x65, 0x6c, 0x65, 0x61, 0x73, 0x65, 0x3e, 0x31, 0x31, 0x38,
    0x3c, 0x2f, 0x72, 0x65, 0x6c, 0x65, 0x61, 0x73, 0x65, 0x3e
  ))

  # this is taken from
  # https://en.wikipedia.org/wiki/List_of_file_signatures
  # https://asecuritysite.com/forensics/zip
  mns$zip_magic <- list(
    as.raw(c(0x50, 0x4b, 0x03, 0x04)),
    as.raw(c(0x50, 0x4b, 0x05, 0x06)),
    as.raw(c(0x50, 0x4b, 0x07, 0x08))
  )

  # this is taken from
  # https://en.wikipedia.org/wiki/List_of_file_signatures
  # this is kept for future possible release of https://github.com/jimhester/archive
  # as of now it will not do anything
  mns$rar_magic <- list(
    # RAR archive version 5.0 onwards
    as.raw(c(0x52, 0x61, 0x72, 0x21, 0x1a, 0x07, 0x01, 0x00)),
    # RAR archive version 1.50 onwards (below 5.0)
    as.raw(c(0x52, 0x61, 0x72, 0x21, 0x1a, 0x07, 0x00))
  )

  # this is taken from
  # https://en.wikipedia.org/wiki/List_of_file_signatures
  # https://en.wikipedia.org/wiki/Tar_(computing)
  # https://gist.github.com/leommoore/f9e57ba2aa4bf197ebc5
  mns$tar_magic <- list(
    as.raw(c(0x75, 0x73, 0x74, 0x61, 0x72, 0x00, 0x30, 0x30)),
    as.raw(c(0x75, 0x73, 0x74, 0x61, 0x72, 0x20, 0x20, 0x00))
  )
  attr(mns$tar_magic[[1]], "pstart") <- 258
  attr(mns$tar_magic[[2]], "pstart") <- 258

  mns$gz_magic <- list(
    # this is taken from https://asecuritysite.com/forensics/magic
    as.raw(c(0x1f, 0x8b, 0x08)),
    # this is taken from https://en.wikipedia.org/wiki/List_of_file_signatures
    as.raw(c(0x1f, 0x8b))
  )


  mns$xz_magic <- list(
    # this is taken from https://en.wikipedia.org/wiki/List_of_file_signatures
    as.raw(c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00, 0x00)),
    # this is taken from https://www.garykessler.net/library/file_sigs.html
    as.raw(c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00))
  )

  # this is taken from
  # https://en.wikipedia.org/wiki/List_of_file_signatures
  # https://www.filesignatures.net/index.php?search=BZ2&mode=EXT
  # https://www.garykessler.net/library/file_sigs.html
  mns$bz2_magic <- as.raw(c(0x42, 0x5a, 0x68))

  # this is taken from
  # https://en.wikipedia.org/wiki/List_of_file_signatures
  # note file_7z is actually 7z file this is because R will not allow --> mns$7z_magic
  mns$file_7z_magic <- as.raw(c(0x37, 0x7a, 0xbc, 0xaf, 0x27, 0x1c))

  # this is taken from https://en.wikipedia.org/wiki/List_of_file_signatures
  # this may be irrelevant as is_txt_file going to detect it
  mns$xml_magic <- as.raw(c(0x3f, 0x78, 0x6d, 0x6c, 0x20))
  attr(mns$xml_magic, "pstart") <- 2

  ################
  # max length
  mns$mx_length <- mns[stringr::str_detect(names(mns), "_magic$")] %>%
    map_int(~ if (is.list(.x)) {
      map_int(.x, this_get_pstart_for_magic_numbers, get_length = TRUE) %>% max()
    } else {
      this_get_pstart_for_magic_numbers(.x, get_length = TRUE)
    }) %>%
    max()

  ################
  mns$file_types <- mns[stringr::str_detect(names(mns), "_magic$")] %>%
    names() %>%
    stringr::str_remove_all("^file_") %>%
    stringr::str_remove_all("_magic$") %>%
    # manully adding these
    c("csv", "html", "text")

  mns
}

# save this magic_numbers for quicker access in tidycells_pkg_env
tidycells_pkg_env$magic_numbers <- this_domain_magic_numbers()


# similar to wand_check_office_clone in wand_link.R for OLE formats
check_office_ole <- function(fbytes, fpath) {
  if (missing(fbytes)) {
    fbytes <- readBin(fpath, n = 8, what = "raw")
  }

  if (mn_check(fbytes, tidycells_pkg_env$magic_numbers$xls_doc_magic)) {
    # check OLE document subheader
    # Ref : https://www.garykessler.net/library/file_sigs.html
    # note 521 is actually required I think
    more_bytes <- readBin(fpath, n = 550, what = "raw")

    # Word document subheader from https://www.filesignatures.net/index.php?search=doc&mode=EXT
    doc_magic <- as.raw(c(0xec, 0xa5, 0xc1, 0x00))
    attr(doc_magic, "pstart") <- 513

    if (mn_check(more_bytes, doc_magic)) {
      return("doc")
    }

    # Excel spreadsheet subheaders from https://www.filesignatures.net/index.php?search=xls&mode=EXT
    xls_magic <- list(
      as.raw(c(0x09, 0x08, 0x10, 0x00, 0x00, 0x06, 0x05, 0x00)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff, 0x10)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff, 0x1f)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff, 0x22)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff, 0x23)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff, 0x28)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff, 0x29))
    )
    xls_magic <- xls_magic %>% map(~ {
      attr(.x, "pstart") <- 513
      .x
    })


    if (mn_check(more_bytes, xls_magic)) {
      return("xls")
    }

    # PowerPoint presentation subheaders from https://www.filesignatures.net/index.php?search=ppt&mode=EXT
    ppt_magic <- list(
      as.raw(c(0x00, 0x6e, 0x1e, 0xf0)),
      as.raw(c(0x0f, 0x00, 0xe8, 0x03)),
      as.raw(c(0xa0, 0x46, 0x1d, 0xf0)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff, 0x0e, 0x00, 0x00, 0x00)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff, 0x1c, 0x00, 0x00, 0x00)),
      as.raw(c(0xfd, 0xff, 0xff, 0xff, 0x43, 0x00, 0x00, 0x00))
    )
    ppt_magic <- ppt_magic %>% map(~ {
      attr(.x, "pstart") <- 513
      .x
    })


    if (mn_check(more_bytes, ppt_magic)) {
      return("ppt")
    }
  }

  return(NULL)
}
