
#' A lightweight function for compatibility test
#' 
#' 
#' @description  
#' 
#' **compatibility form same package is not possible hence removing it in future dev version**
#' 
#' Convenience function for testing compatibility / dependability on a package over two  different versions. 
#' It simply runs supplied expression (`expr`) under two different version of the package. It can either to backward or forward 
#' version comparison with the installed version. If `old_version` is specified then it will compare the expression 
#' evaluation on that version and installed version. If nothing is specified in version, it will compare installed 
#' version with latest available CRAN version. 
#' 
#' **Note:**
#' 
#' * As of now, dependency management is not that great in this function. If a remote (either old or CRAN) version 
#' depend on a package which is not installed currently then it will fail to install the package. Maybe that will 
#' be taken care of in next versions of the function. (to reproduce, test using 
#' `compatibility_check(dplyr::group_split, pkg = "dplyr", old_version = "0.7.6")`, removing `bindrcpp` package (if 
#' you have it). You should have `dplyr 0.8.3` as present package.)
#' * Also, this feature is not quite related to package philosophy and potentially will be shifted to different package 
#' designated to solve the issue at hand.
#' * This should be dependent on `remotes` package. However, since it will introduce a new layer of dependency it is not
#' made depend on it. Maybe this can be done in separate dedicated package.
#' 
#' @param expr expression to run
#' @param old_version (Optional) old version of the package to test. (If omitted CRAN version will be used)
#' @param pkg The package (Default is this package)
#' @param repo The CRAN repo to download from
#' @param warn_outdated whether to warn user if the package version is outdated (Default is `TRUE`)
#' @param use_lambda whether to use `purr` / `rlang` like lambda expression (Default is `FALSE`)
#'
#' @return A list containing output with corresponding version and indicator whether results do match or not.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' compatibility_check(cli::cli_sitrep(), pkg = "cli", old_version = "1.0.0")
#' }
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
  need_new_dir <- FALSE
  inst_require <- TRUE
  if(is.null(tidycells_pkg_env$inst_pkg_for_check_dir)){
    need_new_dir <- TRUE
  }else{
    if(!file.exists(tidycells_pkg_env$inst_pkg_for_check_dir)){
      need_new_dir <- TRUE
    }else{
      # folder exists, need to check pkg version
      exists_pkg <- expression_run_safe(~{
        utils::packageVersion(pkg, lib.loc = tidycells_pkg_env$inst_pkg_for_check_dir)
      })
      
      other_pkgs <- utils::installed.packages(lib.loc = tidycells_pkg_env$inst_pkg_for_check_dir)
      other_pkgs <- other_pkgs[,"Package"]
      rem_pkgs <- setdiff(other_pkgs, pkg)
      
      if(length(rem_pkgs)>0){
        utils::remove.packages(rem_pkgs, lib = tidycells_pkg_env$inst_pkg_for_check_dir)
      }
      
      if(!missing(old_version)){
        inst_require <- !identical(as.package_version(old_version), exists_pkg)
      }else{
        #to be check for CRAN version
        avp <- available.packages()
        if(pkg %in% row.names(avp)){
          this_pkg_info <- avp[pkg, ]
          pv <- as.package_version(this_pkg_info["Version"])
          inst_require <- !identical(pv, exists_pkg)
        }
      }
      
      if(inst_require){
        # remove prior installation
        utils::remove.packages(pkg, lib = tidycells_pkg_env$inst_pkg_for_check_dir)
      }
    }
  }
  
  if(need_new_dir){
    temp_dir <- tempfile(pattern = "inst_check", tmpdir = tempdir(check = TRUE))
    tidycells_pkg_env$inst_pkg_for_check_dir <- temp_dir
  }else{
    tidycells_pkg_env$inst_pkg_for_check_dir -> temp_dir
  }
  
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # change libpath
  .libPaths(c(temp_dir, now_lib_path))
  if(normalizePath(.libPaths()[1])!=normalizePath(temp_dir)){
    abort("Failed to change libPaths")
  }
  
  # install package
  if(inst_require){
    if(!missing(old_version)){
      old_version_n <- as.package_version(old_version)
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
  }
  
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
  
  if(warn_outdated){
    if(output$target$ver > output$current$ver){
      warn("You are using outdated version of the package.")
    }
  }
  
  output
}
