

library(dplyr)
library(stringr)
library(purrr)

bm <- readxl::read_excel("sys/build_matrix.xlsx")

get_log <- function(lurl){
  cat(lurl)
  cat("\n")
  if(!is.na(lurl)){
    log <- try(readLines(lurl), silent = TRUE)
    if(inherits(log, "try-error")){
      ""
    }else{
      log
    }
  }else{
    ""
  }
}

bm <- bm %>% mutate(log = URL %>% map(get_log))

get_r_version <- function(log){
  exp <- try(str_detect(log, "R version") %>% log[.] %>% .[[1]] %>% str_split("R version") %>% unlist() %>% rev() %>% .[[1]] %>% paste0("R version", .), silent = T)
  if(inherits(exp, "try-error")){
    ""
  }else{
    exp
  }
}

get_platform <- function(log){
  exp <- try(str_detect(log, "using platform") %>% log[.] %>% .[[1]] %>% str_split("platform: ") %>% unlist() %>% rev() %>% .[[1]], silent = T)
  if(inherits(exp, "try-error")){
    ""
  }else{
    exp
  }
}

bm <- bm %>% mutate(rv_from_log = log %>% map_chr(get_r_version),
                    pt_from_log = log %>% map_chr(get_platform))
