
#' @include tidycells-package.R

############# wand clone ###############
# Few functions rewritten from wand to #
# make it available to the users even  #
# when wand is not present             #
########################################

# Ref
# https://github.com/minad/mimemagic
# https://stackoverflow.com/questions/48188346/how-to-distinguish-xlsx-and-docx-files-from-zip-archives
# https://stackoverflow.com/questions/51432256/determine-if-a-file-is-a-zip-file-or-an-xlsx-file
# https://github.com/minad/mimemagic/blob/master/lib/mimemagic/overlay.rb


wand_check_office_clone <- function(fbytes, fpath) {
  # this is kept so that the message gets clear to the users

  "Note this function is rewritten based on https://gitlab.com/hrbrmstr/wand/blob/master/R/check-office.R"

  # I personally thank him (the author of the wand package, Bob Rudis)
  # for discovering the rules to discriminate between office formats.
  # I don't understand exact logic or I have not yet found proper reference to this rule.
  # This functionality is additional help to me (basically to tidycells).
  # this is a failsafe implementation. If I'm not completely sure I'll fall back to normal detection
  # which is understandable by me.

  if (missing(fbytes)) {
    fbytes <- readBin(fpath, n = 49, what = "raw")
  }

  p_content_types <- as.raw(c(
    0x5b, 0x43, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x5f, 0x54,
    0x79, 0x70, 0x65, 0x73, 0x5d, 0x2e, 0x78, 0x6d, 0x6c
  ))

  p_rels <- as.raw(c(0x5f, 0x72, 0x65, 0x6c, 0x73, 0x2f, 0x2e, 0x72, 0x65, 0x6c, 0x73))

  if ((all(p_content_types == fbytes[31:49])) || (all(p_rels == fbytes[31:41]))) {
    fbytes_more <- readBin(fpath, "raw", n = 4096)

    pat_word <- as.raw(c(0x77, 0x6f, 0x72, 0x64, 0x2f))
    if (length(wand_seq_in_clone(fbytes_more, pat_word)) > 0) {
      return("docx")
    }

    pat_ppt <- as.raw(c(0x70, 0x70, 0x74, 0x2f))
    if (length(wand_seq_in_clone(fbytes_more, pat_ppt)) > 0) {
      return("pptx")
    }

    pat_xl <- as.raw(c(0x78, 0x6c, 0x2f))
    if (length(wand_seq_in_clone(fbytes_more, pat_xl)) > 0) {
      return("xlsx")
    }
  }

  return(NULL)
}

wand_seq_in_clone <- function(svec, pvec) {
  # this is kept so that the message gets clear to the users

  "Note this function is rewritten based on https://gitlab.com/hrbrmstr/wand/blob/master/R/util.R"

  # I personally thank him (the author of the wand package, Bob Rudis)
  # for discovering the rules to discriminate between office formats.
  # I don't understand exact logic or I have not yet found proper reference to this rule.
  # This functionality is additional help to me (basically to tidycells).
  # this is a failsafe implementation. If I'm not completely sure I'll fall back to normal detection
  # which is understandable by me.

  this_matches <- lapply(pvec, "==", svec)
  mls <- this_matches %>%
    seq_along() %>%
    map(~ this_matches[[.x]][.x:(length(svec) - length(pvec) + .x)]) %>%
    reduce(`+`)
  which(mls == length(pvec))
}
