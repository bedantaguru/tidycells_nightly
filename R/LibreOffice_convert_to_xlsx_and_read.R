
# this supports xls format (alternative method)
LibreOffice_convert_to_xlsx_and_read <- function(srcfile, ...){
  
  if(!is_available("tidyxl")){
    abort("tidyxl is required for this functionality")
  }
  
  if(!is_available("docxtractr")){
    abort("docxtractr is required for this functionality (though it will not be directly used)")
  }
  
  lobin <- detect_LibreOffice(return_LibreOffice_path = T)
  
  if(!is.null(lobin)){
    if(file.exists(lobin)){
      tdir <- this_temp_file("xlsx_out")
      unlink(tdir, recursive = TRUE, force = TRUE)
      dir.create(tdir, showWarnings = FALSE)
      on.exit(rem_temp_file(tdir))
      
      #  ref : https://cgit.freedesktop.org/libreoffice/core/tree/filter/source/config/fragments/filters
      cmd <- sprintf('"%s" --convert-to xlsx:"Calc MS Excel 2007 XML" --headless --outdir "%s" "%s"', 
                     lobin, tdir, srcfile)
      
      try(system(cmd, intern = TRUE, show.output.on.console = FALSE), silent = TRUE)
      
      outfile <- list.files(tdir, full.names = TRUE)
      if(length(outfile)==1){
        dtry <- tidyxl::xlsx_cells(outfile, ...) %>% split(.$sheet)
        if(!inherits(dtry, "try-error")) return(dtry)
      }
    }
  }else{
    abort("LibreOffice required for this functionality")
  }
  
  NULL
  
}
