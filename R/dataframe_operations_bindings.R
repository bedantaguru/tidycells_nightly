
# this is an attempt to relocate codebase from {dplyr} to {base}
# this is simpler implementation if found suitable entire code can be 
# adopted to this framework 
# see https://github.com/r-rudra/tidycells/issues/27

# redirected binding for rename and select


rename <- function(...){
  if(getOption("tidycells.df_operator")=="dplyr"){
    dplyr::rename(...)
  }else{
    rename_base_nse(...)
  }
}


select <- function(...){
  if(getOption("tidycells.df_operator")=="dplyr"){
    dplyr::select(...)
  }else{
    select_base_nse(...)
  }
}

####################
# helpers for base #
####################

select_base_nse <- function(data, ...){
  el <- rlang::exprs(...)
  if(length(el)>0){
    sels <- as.character(el)
    if(any(stringr::str_detect(sels,"-"))){
      rems <- stringr::str_remove(sels,"-") %>% stringr::str_trim()
      data <- data[setdiff(colnames(data),rems)]
    }else{
      data <- data[as.character(el)]
      nms <- names(el)
      if(!is.null(nms)){
        nms <- nms[nchar(nms)>0]
        eln <- el[nms]
        if(length(eln)>0){
          data <- rename_base(data, new_names = nms, old_names = as.character(eln))
        }
      }
    }
  }
  data
}

rename_base <- function(data, old_names, new_names){
  cn <- colnames(data)
  cnt <- seq_along(cn)
  names(cnt) <- cn
  cn[cnt[old_names]] <- new_names
  colnames(data) <- cn
  data
}

rename_base_nse <- function(data, ...){
  el <- rlang::exprs(...)
  if(length(el)>0){
    rns <- names(el)
    ons <- as.character(el)
    data <- rename_base(data, new_names = rns, old_names = ons)
  }
  data
}