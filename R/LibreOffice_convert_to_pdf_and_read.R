
# this supports many formats like doc, ppt, etc
LibreOffice_convert_to_pdf_and_read <- function(srcfile, ...){
  
  if(!is_available("tabulizer")){
    abort("tabulizer is required for this functionality")
  }
  
  if(!is_available("docxtractr")){
    abort("docxtractr is required for this functionality (though it will not be directly used)")
  }
  
  lobin <- detect_LibreOffice(return_LibreOffice_path = T)
  
  if(!is.null(lobin)){
    if(file.exists(lobin)){
      tdir <- this_temp_file("pdf_out")
      unlink(tdir, recursive = TRUE, force = TRUE)
      dir.create(tdir, showWarnings = FALSE)
      on.exit(rem_temp_file(tdir))
      
      # for pptx :: pptx:"Impress MS PowerPoint 2007 XML" (but it is not working with officer)
      # So going for tabulizer and pdf
      cmd <- sprintf('"%s" --convert-to pdf --headless --outdir "%s" "%s"', 
                     lobin, tdir, srcfile)
      
      try(system(cmd, intern = TRUE, show.output.on.console = FALSE), silent = TRUE)
      
      outfile <- list.files(tdir, full.names = TRUE)
      if(length(outfile)==1){
        dtry <- read_pdf_from_tabulizer(outfile, ...)
        if(!inherits(dtry, "try-error")) return(dtry)
      }
    }
  }
  
  NULL
  
}
