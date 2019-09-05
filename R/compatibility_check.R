
compatibility_check <- function(expr, 
                                old_version, 
                                pkg = "tidycells", 
                                repo = default_repo(), 
                                warn_outdated = TRUE, 
                                use_lambda = FALSE){
  now_lib_path <- .libPaths()
  
  if(!use_lambda){
    ef <- exp_to_func(expr)
  }
  
  if(!stringr::str_detect(repo, "/$")){
    repo <- paste0(repo, "/")
  }
  
  on.exit(.libPaths(now_lib_path))
  
  output <- list()
  
  is_attached_now <- is_pkg_loaded(pkg)
  
  dnsl <- re_attach(pkg)
  expression_run_safe(~{
    es <- loadNamespace(pkg)
    if(is_attached_now) attachNamespace(es)
  })
  # current run
  output$current$ver <- expression_run_safe(~utils::packageVersion(pkg), no_class = TRUE)
  output$current$res <- expression_run_safe(~ef(), no_class = TRUE)
  
  # do it for remote
  temp_dir <- tempfile(pattern = "inst_check", tmpdir = tempdir(check = TRUE))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # change libpath
  .libPaths(c(temp_dir, now_lib_path))
  if(normalizePath(.libPaths()[1])!=normalizePath(temp_dir)){
    abort("Failed to change libPaths")
  }
  
  # install package
  if(!missing(old_version)){
    old_version_n <- as.numeric_version(old_version)
    old_url <-  paste0(repo, "src/contrib/Archive/",pkg,"/",pkg,"_",old_version,".tar.gz")
    loc_path <- paste0(temp_dir, "/",pkg,"_",old_version,".tar.gz")
    e <- expression_run_safe(~download.file(old_url, loc_path, quiet = TRUE))
    if(inherits(e, "try-error")){
      abort("Unable to donwload the package.")
    }else{
      # proceed
      e <- expression_run_safe(~utils::install.packages(loc_path, repos = NULL, quiet = TRUE))
      if(inherits(e, "try-error")){
        abort("Unable to install the package.")
      }else{
        # proceed
        chk_ver <- expression_run_safe(~utils::packageVersion(pkg, lib.loc = .libPaths()))
        if(chk_ver!=old_version_n){
          abort("Temporary installed version not matching with given version")
        }
      }
    }
  }else{
    #  install CRAN version
    e <- expression_run_safe(~utils::install.packages(pkg, repos = repo, quiet = TRUE))
    if(inherits(e, "try-error")){
      abort("Unable to install the package.")
    }
  }
  
  # this section need to customized for pkgs
  # all downstream dependency has to be unloaded first
  re_attach(pkg)
  expression_run_safe(~{
    es <- loadNamespace(pkg)
    if(is_attached_now) attachNamespace(es)
  })
  
  output$target$ver <- expression_run_safe(~utils::packageVersion(pkg), no_class = TRUE)
  output$target$res <- expression_run_safe(~ef(), no_class = TRUE)
  
  output$is_same <- identical(output$target$res, output$current$res)
  
  .libPaths(now_lib_path)
  re_attach(pkg)
  expression_run_safe(~{
    es <- loadNamespace(pkg)
    if(is_attached_now) attachNamespace(es)
  })
  dnsl$d %>% map(re_attach)
  dnsl$a %>% map(~re_attach(.x, force_attach = TRUE))
  unlink(temp_dir, recursive = TRUE, force = TRUE)
  
  if(warn_outdated){
    if(output$target$ver > output$current$ver){
      warn("You are using outdated version of the package.")
    }
  }
  
  output
}
