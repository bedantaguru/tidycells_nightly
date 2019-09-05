
# inspired by shiny::exprToFunction
exp_to_func <- function(expr){
  expr <- eval(substitute(substitute(expr)), parent.frame())
  eval(call("function", NULL, expr), parent.frame())
}

expression_run_safe <- function(expr, no_class = FALSE, use_lambda = TRUE){
  if(use_lambda){
    expr_fn <- rlang::as_function(expr)
  }else{
    expr_fn <- exp_to_func(expr) 
  }
  sink(tempfile())
  e <- suppressMessages(suppressWarnings(
    try(expr_fn(), silent = TRUE)
  ))
  sink(NULL)
  if(no_class & inherits(e, "try-error")){
    e <- attr(e, "condition")
  }
  e
}

deps <- function(pkg){
  if(!is.data.frame(pkg)){
    dpt <- tibble(pkg = pkg)
  }else{
    dpt <- pkg
  }
  if(nrow(dpt)==0) return(NULL)
  dpt <- dpt %>% mutate(dep = pkg %>% map(getNamespaceUsers)) %>% tidyr::unnest()
  dpt %>% bind_rows(dpt %>% select(pkg = dep) %>% deps)
}

is_pkg_loaded <- function(pkg){
  x<- utils::sessionInfo()
  return(pkg %in% c(names(x$otherPkgs), x$basePkgs))
}

# inspired by callr::default_repos
# required in non-interactive session
default_repo <- function(){
  op <- getOption("repos")
  if (!"CRAN" %in% names(op) || op[["CRAN"]] == "@CRAN@") {
    op[["CRAN"]] <- "https://cloud.r-project.org"
  }
  op
}

re_attach <- function(pkg, force_attach = FALSE){
  if(is.null(pkg)) return(invisible(NULL))
  this_deps <- deps(pkg)
  detach_list <- NULL
  prior_attached_list <- NULL
  if(nrow(this_deps)>0){
    while (nrow(this_deps)>0) {
      rem_now <- this_deps$dep[nrow(this_deps)]
      if(is_pkg_loaded(rem_now)){
        prior_attached_list <- c(rem_now, prior_attached_list) %>% unique()
      }
      unloadNamespace(rem_now)
      detach_list <- c(rem_now, detach_list) %>% unique()
      this_deps <- this_deps %>% filter(!(dep %in% rem_now))
    }
  }
  it_is_attached <- is_pkg_loaded(pkg)
  unloadNamespace(pkg)
  if(length(detach_list)>0){
    detach_list %>% map(~try(loadNamespace(.x), silent = TRUE))
  }
  if(length(prior_attached_list)>0){
    prior_attached_list %>% map(~try(attachNamespace(.x), silent = TRUE))
  }
  loadNamespace(pkg)
  if(it_is_attached | force_attach) attachNamespace(pkg)
  invisible(list(d = detach_list, a = prior_attached_list))
}
