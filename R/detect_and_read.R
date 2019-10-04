
# File Deps: Though it is not required

#' @include file_etc.R
#' @include detect_and_read_shafts.R

# detect and read file type (and potentially read) based on content type
detect_and_read <- function(fn, silent = FALSE, omit = NULL, file_type) {
  common_file_error(fn, silent = silent)

  ext <- this_file_ext(fn)
  
  if(missing(file_type)){
    file_type <- detect_file_type(fn)
  }
  
  lo <- list(file_name = fn, type = NULL, file_type = file_type, content = NULL, ext = ext, omit = omit)

  if (file_type == "text") {
    # the file is flat file [possible csv/tsv or html]
    lo$type <- c("csv", "html")

    # try html first
    this_sft <- make_shaft(lo) %>%
      shaft_html() %>%
      shaft_csv() %>%
      shaft_csv_utils()
  }

  if (file_type != "text") {

    # default case
    lo$type <- file_type

    # explicit case of combined types
    if (file_type == "xls_doc") {
      lo$type <- c("xls", "doc")
    }

    if (file_type == "xlsx_docx") {
      lo$type <- c("xlsx", "docx")
    }

    this_sft <- make_shaft(lo) %>%
      shaft_xlsx() %>%
      shaft_docx() %>%
      shaft_docx_officer() %>% 
      shaft_xls() %>%
      shaft_xls_readxl() %>%
      shaft_doc() %>%
      shaft_pptx() %>% 
      shaft_pdf() %>% 
      shaft_haven() %>% 
      shaft_doc_ppt_pptx_LibreOffice_tabulizer()

    if (!this_sft$shaft_done) {
      # reset to the case when fails to detect type
      this_sft$shaft_info$type <- NULL
    }
  }

  return(finalize_lo(this_sft$shaft_info))
}
