

quick_spell_check <- function(){
  ft <- "inst/WORDLIST"
  if(file.exists(ft)){
    unlink(ft)
  }else{
    suppressWarnings(readLines("00_nightly_only/WORDLIST") %>% writeLines(ft))
    print(devtools::spell_check())
    unlink(ft)
  }
}

pdf_ok <- function(){
  devtools::document()
  pe <- devtools::build_manual()
  pe <- stringr::str_remove_all(pe$command[4], "--output=")
  on.exit(unlink(pe))
  file.exists(pe)
}


check_dev_mark <- function(){
  
  r_files <- list.files(full.names = TRUE, pattern = ".R$", recursive = TRUE)
  r_files <- r_files[!stringr::str_detect(r_files, "00_nightly_only/")]
  rf <- r_files %>% as.list()
  names(rf) <- r_files
  
  
  rf %>% map(readLines) %>% map(~.x[stringr::str_detect(.x,"@Dev")]) %>% map_lgl(~length(.x)>0) %>% which() %>% names() %>% print()
  rf %>% map(readLines) %>% map(~.x[stringr::str_detect(.x,"TODO")]) %>% map_lgl(~length(.x)>0) %>% which() %>% names() %>% print()
  rf %>% map(readLines) %>% map(~.x[stringr::str_detect(.x,"browser")]) %>% map_lgl(~length(.x)>0) %>% which() %>% names() %>% print()
  rf %>% map(readLines) %>% map(~.x[stringr::str_detect(.x," warning\\(")]) %>% map_lgl(~length(.x)>0) %>% which() %>% names() %>% print()
  rf %>% map(readLines) %>% map(~.x[stringr::str_detect(.x," stop\\(")]) %>% map_lgl(~length(.x)>0) %>% which() %>% names() %>% print()
  rf %>% map(readLines) %>% map(~.x[stringr::str_detect(.x,":::")]) %>% map_lgl(~length(.x)>0) %>% which() %>% names() %>% print()
  
}

TF_replace <- function(){
  
  r_files <- list.files(full.names = TRUE, pattern = ".R$", recursive = TRUE)
  r_files <- r_files[!stringr::str_detect(r_files, "00_nightly_only/")]
  rf <- r_files %>% as.list()
  names(rf) <- r_files
  
  tf_replacement <- function(code_path, write_back = TRUE){
    
    code_str <- readLines(code_path)
    
    F_str <- "^F$|[- \\=,]F$|^F[,) ]|[- \\=,]F[,) ]"
    T_str <- stringr::str_replace_all(F_str, "F","T")
    
    dc <- tibble(code = code_str)
    
    dc <- dc %>%
      mutate(fp = stringr::str_detect(code, F_str),
             tp = stringr::str_detect(code, T_str),
             fps = code %>% stringr::str_extract_all(F_str) %>% map(~.x %>% unique()),
             tps = code %>% stringr::str_extract_all(T_str) %>% map(~.x %>% unique()))
    
    dc <- dc %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      purrr::map_df(~{
        if(.x$fp){
          .code <- .x$code
          for(.fp in .x$fps[[1]]){
            .code <- stringr::str_replace_all(.code, stringr::str_replace_all(.fp, "\\)", "\\\\\\)"), stringr::str_replace_all(.fp, "F", "FALSE"))
          }
          .x$code <-.code
        }
        
        if(.x$tp){
          .code <- .x$code
          for(.tp in .x$tps[[1]]){
            .code <- stringr::str_replace_all(.code, stringr::str_replace_all(.tp, "\\)", "\\\\\\)"), stringr::str_replace_all(.tp, "T", "TRUE"))
          }
          .x$code <-.code
        }
        
        .x
      }) %>%
      ungroup()
    
    if(write_back){
      writeLines(dc$code, code_path)
    }
    
    dc %>% summarise(sum(fp|tp)) %>% pull(1)
    
  }
  
  rf %>% map_int(~tf_replacement(.x, write_back = TRUE)) %>% unique()
  
}

this_pkg_deps <- function(){
  r_files <- list.files(full.names = TRUE, pattern = ".R$", recursive = TRUE)
  r_files <- r_files[!stringr::str_detect(r_files, "00_nightly_only/")]
  rf <- r_files %>% as.list()
  names(rf) <- r_files
  
  this_dep <- rf %>% map(requirements::req_file) %>% unlist() %>% unique()
  
  xt <- desc::desc()
  
  dps <- xt$get_deps()
  this_dep_desc <- dps$package[dps$type %in% c("Imports", "Suggests")]
  
  this_dep %>% setdiff(this_dep_desc) %>% print()
  
}