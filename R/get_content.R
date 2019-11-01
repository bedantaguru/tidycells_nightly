
get_content <- function(x, ...){
  UseMethod("get_content")
}

get_content.NULL <- function(x, ...){
  ""
}

get_content.cell_df <- function(x, ...){
  xn <- numeric_values_classifier(x)
  xstr <- xn %>% as_tibble %>% filter(type=="attribute") %>% pull(value) %>% 
    tolower() %>% stringr::str_trim() %>% unique() %>% paste0(collapse = " ")
  xstr
}

get_content.Table_Field_Container <- function(x, ...){
  x %>% map_chr(get_content) %>% strsplit(" ") %>% unlist %>% stringr::str_trim() %>% unique() %>% paste0(collapse = " ")
}

