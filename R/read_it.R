
# @Dev


read_it <- function(x, ...){
  UseMethod("read_it")
}


read_it.exploration_findings <- function(x, omit = NULL, ...){
  
  if("with_content" %in% state(x)) return(x) # retun early
  
  sl <- x %>% split(seq(nrow(x)))
  
  if(any(!file.exists(x$path[x$is_readable]))){
    msg_once("Note: The objects 'exploration_findings' is persistent only after reading it.")
    abort("Few paths in the 'exploration_findings' is not accessible.")
  }
  
  slt <- sl %>% map(~{
    this_content <- NULL
    
    if(.x$is_readable){
      try({
        this_content <- detect_and_read(fn = .x$path, silent = TRUE, omit = omit, file_type = .x$file_type)$content %>% as_tfc
      }, silent = TRUE)
    }
    
    if(is_tfc(this_content)){
      this_content
    }else{
      NULL
    }
  })

  names(slt) <- x$well_name
  
  nt_read <- slt %>% map_lgl(is.null)
  
  x$tfc <- slt
  
  x$content <- x$tfc %>% map_chr(get_content)
  
  x$read_success  <- !nt_read
  
  x
  
}


read_it.Table_Field_Container <- function(x, refresh = FALSE, ...){
  xt <- filter(x, TRUE)
  attr(xt, "content_variation_over_TFs") <- calc_content_variation_over_TFs(xt)
  formalize_tfc(xt)
}
