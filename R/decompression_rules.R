


# common for gz, bz2, xz
decompress_common_type_1 <- function(fn, dest_fold, fcon_f, buffer_size = 1e7){
  outfn <- basename(fn) %>% 
    this_file_ext() %>% 
    paste0(".",., "$") %>% 
    stringr::str_remove(basename(fn), .)
  outpath <- file.path(dest_fold, outfn)
  unlink(outpath, recursive = TRUE, force = TRUE)
  inf <- fcon_f(fn, open = "rb")
  on.exit(if (!is.null(inf)) close(inf))
  out_complete <- FALSE
  outf <- file(outpath, open = "wb")
  on.exit({
    if (!is.null(outf)) close(outf)
    if (!out_complete) unlink(outpath, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  
  try({
    repeat {
      bf_read <- readBin(inf, what = raw(0L), size = 1L, n = buffer_size)
      n <- length(bf_read)
      if (n == 0L) break
      writeBin(bf_read, con = outf, size = 1L)
      bf_read <- NULL
    }
    out_complete <- TRUE
  }, silent = TRUE)
  
  close(outf)
  outf <- NULL
  invisible(out_complete)
}


decompress <- function(src, dest, ...){
  UseMethod("decompress")
}

decompress.file_zip <- function(src, dest){
  utils::unzip(src, exdir = dest)
}


decompress.file_tar <- function(src, dest){
  utils::untar(src, exdir = dest)
}


decompress.file_gz <- function(src, dest){
  decompress_common_type_1(src, dest, fcon_f = gzfile)
}

decompress.file_bz2 <- function(src, dest){
  decompress_common_type_1(src, dest, fcon_f = bzfile)
}

decompress.file_xz <- function(src, dest){
  decompress_common_type_1(src, dest, fcon_f = xzfile)
}
