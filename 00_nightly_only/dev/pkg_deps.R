


pkg_dep_list_single <- function(pkg_this, all_pkgs){
  all_pkgs %>% 
    filter(pkg == pkg_this) %>% 
    pull(Imports) %>% 
    stringr::str_split(",") %>% 
    unlist() %>% 
    stringr::str_extract("[a-zA-Z0-9]+") %>% 
    stringr::str_trim()
}

pkg_dep_list_multiple <- function(pkgs, all_pkgs){
  deps <- pkgs %>% map(~pkg_dep_list_single(.x, all_pkgs)) %>% unlist() %>% unique()
  deps <- deps[!is.na(deps)]
  deps
}

pkg_dep_list <- function(pkgs){
  
  x <- installed.packages()
  # for inclusion of base and recommended pkgs
  usr_pkgs <- x %>% tibble::as_tibble() %>% dplyr::mutate(pkg = row.names(x)) %>% dplyr::filter(is.na(Priority))
  depi <- pkgs
  depi_now <- depi
  repeat({
    depi_now <- pkg_dep_list_multiple(depi_now, usr_pkgs)
    if(length(depi_now)==0) break()
    depi <- c(depi, depi_now) %>% unique()
  })
  depi
}