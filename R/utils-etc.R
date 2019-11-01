
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


state <- function(x, ...){
  UseMethod("state")
}

state.NULL <- function(x, ...){
  ""
}

state.default <- function(x, ...){
  sa <- attr(x, "state")
  if(is.null(sa)){
    ""
  }else{
    sa[1]
  }
}

attach_state <- function(x){
  if(is.null(attr(x, "state"))){
    UseMethod("attach_state")
  }else{
    x
  }
}

attach_state.default <- function(x){
  sa <- attr(x, "state")
  if(is.null(sa)){
    attr(x, "state") <- ""
  }else{
    sa <- sa[!is.na(sa)]
    attr(x, "state") <- sa[1]
  }
  x
}

set_state <- function(x, state = ""){
  attr(x, "state") <- state
  x
}


