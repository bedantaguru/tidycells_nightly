


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
  deps %>% intersect(all_pkgs$pkg)
}

pkg_dep_list <- function(pkgs, details = FALSE){
  
  x <- utils::installed.packages()
  # for inclusion of base and recommended pkgs
  usr_pkgs <- x %>% 
    as_tibble() %>% 
    mutate(pkg = row.names(x)) %>% 
    filter(is.na(Priority))
    # mutate(Priority = ifelse(is.na(Priority), "", Priority)) %>% 
    # filter(Priority!="base")
  depi <- pkgs
  depi_now <- depi
  repeat({
    depi_now <- pkg_dep_list_multiple(depi_now, usr_pkgs)
    if(length(depi_now)==0) break()
    depi <- c(depi, depi_now) %>% unique()
  })
  
  if(details){
    usr_pkgs %>% filter(pkg %in% depi) %>% select(Package=pkg, Version)
  }else{
    depi
  }
}