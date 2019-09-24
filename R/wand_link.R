
#' @include tidycells-package.R

# Note 
# this wand is done in a non-standard way for a specific reason
# should be implemented in normal way after potential fix for (following)
# https://gitlab.com/hrbrmstr/wand/issues/1
# this is because I don’t want to rely at all on the file extension.

# this is designed for modifying pkg source on the fly
# this ideally may not work for all packages
get_pure_source_in_env <- function(pkg){
  
  if(is_available(pkg)){
    
    ns <- asNamespace(pkg)
    ns_copy <- as.list(ns, all.names=TRUE)
    ns_copy <- ns_copy %>% map_lgl(is.function) %>% ns_copy[.]
    env <- as.environment(ns_copy)
    
    fsrc_only <- function(f, env_this){
      fbody <- body(f)
      fnew <- args(f)
      body(fnew, envir = env_this) <- fbody
      fnew
    }
    
    env_out <- new.env()
    for(fn in ls(env, all.names = T)){
      env_out[[fn]] <- fsrc_only(env[[fn]], env_this = env_out)
    }
    env_out
  }else{
    emptyenv()
  }
}



if(is_available("wand")){
  env <- get_pure_source_in_env("wand")
  # disable extension based detection
  env$guess_content_type <- function(...) {"???"}

  tidycells_pkg_env$wand_link <- env

  rm(env)
}

detect_type_by_wand <- function(path){
  all_ok <- FALSE
  
  if(is_available("wand")){
    if(is.environment(tidycells_pkg_env$wand_link)){
      if(is.function(tidycells_pkg_env$wand_link$get_content_type)){
        all_ok <- TRUE
      }
    }
  }
  
  if(all_ok){
    tidycells_pkg_env$wand_link$get_content_type(path)
  }else{
    "???"
  }
}