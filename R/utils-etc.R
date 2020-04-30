
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


# to be called like
# .fixed <- nse_to_se_colname_picker(substitute(.fixed))
nse_to_se_colname_picker <- function(sbs){
  # rlang fucntions which needs to consider
  # enquo enquos enexpr enexprs
  # or base enquote, quote, substitute can be used
  # sbs <- substitute(.exp)
  
  if(is.null(sbs)){
    return(NULL)
  }
  
  c1 <- as.character(sbs)
  
  c1 <- c1[nchar(c1)>0]
  
  c2 <- as.character(base::enquote(sbs))
  
  if(any(stringr::str_detect(c2, "c\\(|c +\\("))){
    return(setdiff(c1, "c"))
  }
  
  return(c1)
  
}

clean_string <- function(x){
  x %>% 
    tolower() %>% 
    stringr::str_replace_all("[^a-z0-9]"," ") %>% 
    stringr::str_replace_all(" +"," ") %>% 
    stringr::str_trim()
}

string_signature <- function(x){
  empt <- tibble(name_signature = character(0), original_name = character(0), 
                 trim_name = character(0))
  x <- x[!is.na(x)] %>% 
    unique() %>% 
    as_character()
  
  if(length(x) == 0) {
    return(empt)
  }
  
  dx <- tibble(original_name = x)
  
  
  dx <- dx %>% 
    mutate(name_signature = original_name %>% 
             clean_string())
  
  dx <- dx %>% 
    filter(nchar(name_signature)>0)
  
  if(nrow(dx) == 0) {
    return(empt)
  }
  
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
    sa <- as_character(sa) %>% unique()
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


# @Dev
# statistical mode
# while stat_mode may be accurate it is slow
# it is kept for reference 
# may be a quality param needed
stat_mode <- function(x){
  ux <- unique(x)
  if(length(ux)==1){
    return(ux)
  }
  m1 <- table(x) %>% which.max() %>% names()
  if(is.numeric(x)){
    m1 <- as.numeric(m1) %>% mean()
  }
  m1[1]
}

# for consistent number to char conversion
as_character <- function(x){
  # temp option change
  osp <- getOption("scipen")
  # 10^100 is big enough !
  options(scipen = 100)
  ch <- as.character(x)
  # revert back
  options(scipen = osp)
  ch
}

# like expand.grid 
# it is operating on two df
expand_df <- function(d1, d2){
  
  n1 <- nrow(d1)
  n2 <- nrow(d2)
  expnd <- expand.grid(r1 = seq(n1), r2 = seq(n2), KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  dexp1 <- d1[expnd$r1,]
  dexp2 <- d2[expnd$r2,]
  # cbind is faster than dplyr:bind_cols
  out <- cbind(dexp1, dexp2)
  
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}


