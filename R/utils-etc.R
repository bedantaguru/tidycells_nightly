
this_temp_file <- function(pattern = "tc_file", tmpdir = tempdir(check = TRUE), fileext = ""){
  tf <-  tempfile(pattern = pattern, tmpdir = tmpdir, fileext = fileext)
  tidycells_pkg_env$temp_files <- c(tidycells_pkg_env$temp_files, tf) %>% unique()
  tf
}

rem_temp_file <- function(tf){
  tidycells_pkg_env$temp_files <- setdiff(tidycells_pkg_env$temp_files, tf) %>% unique()
  unlink(tf, recursive = TRUE, force = TRUE)
}

msg_once <- function(...){
  txt <- paste0(...)
  noti_this <- tibble(txt = txt)
  noti_now <- tidycells_pkg_env$notifications
  if(!is.data.frame(noti_now)){
    tidycells_pkg_env$notifications <- noti_this
    message(txt)
  }else{
    if(!(txt %in% noti_now$txt)){
      tidycells_pkg_env$notifications <- noti_now %>% bind_rows(noti_this) %>% unique()
      message(txt)
    }
  }
  
  return(invisible(txt))
  
}

string_signature <- function(x){
  x <- x[!is.na(x)] %>% 
    unique() %>% 
    as.character()
  
  dx <- tibble(original_name = x)
  
  
  dx <- dx %>% 
    mutate(name_signature = original_name %>% 
             as.character() %>% 
             tolower() %>% 
             stringr::str_replace_all("[^a-z0-9]"," ") %>% 
             stringr::str_replace_all(" +"," ") %>% 
             stringr::str_trim())
  
  dx <- dx %>% 
    filter(nchar(name_signature)>0)
  
  dx %>% 
    group_by(name_signature) %>% 
    summarise(original_name = max(original_name)) %>% 
    mutate(trim_name = stringr::str_trim(original_name))
}


state <- function(x, ...){
  UseMethod("state")
}

state.NULL <- function(x, ...){
  ""
}

state.default <- function(x, ...){
  formalize_state(attr(x, "state"))
}

formalize_state <- function(sa){
  if(is.null(sa)){
    ""
  }else{
    sa <- as.character(sa) %>% unique()
    sa <- sa[!is.na(sa)]
    sa <- sa[nchar(sa)>0]
    if(length(sa)==0) sa <- ""
    sa
  }
}


name_fix_for_list <- function(xl, name_tag = "Node", sep="_"){
  
  if(is.null(names(xl))){
    names(xl) <- paste0(name_tag, sep, seq_along(xl))
  }else{
    nms <- names(xl)
    nms <- nms[!is.na(nms)]
    nms <- nms[nchar(nms)>0]
    nms <- unique(nms)
    if(length(nms)!=length(xl)){
      nmap <- tibble(nms = names(xl), seq = seq_along(nms))
      nmap <- nmap %>% mutate(is_blank = nchar(nms)==0)
      nmap_blnk <- nmap %>% filter(is_blank)
      nmap <- nmap %>% mutate(nn = nms)
      if(nrow(nmap_blnk)>0){
        nmap_blnk <- nmap_blnk %>% mutate(nn = paste0(name_tag, sep, seq_along(nms)))
        nmap <- nmap %>% filter(!is_blank) %>% bind_rows(nmap_blnk)
      }
      
      if(any(duplicated(nmap$nn))){
        nmapl <- nmap %>% group_by(nn) %>% group_split()
        nmap <- nmapl %>% map_df(~{
          if(nrow(.x)>1){
            .x <- .x %>% mutate(nn = paste0(nn, sep, seq_along(nn)))
          }
          .x
        })
      }
      nmap <- nmap %>% arrange(seq)
      names(xl) <- nmap$nn
    }
  }
  
  xl
  
}


