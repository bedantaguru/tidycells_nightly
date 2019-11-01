# @Dev
# need documnetation

extra_functionalities <- function(force = FALSE){
  if(!force){
    if(!interactive()){
      abort("interactive session needed")
    }
  }
  
  message("This module may need internet access.\n\n")
  
  message(paste0("Note: this module (‘tidycells:::extra_functionalities’) is completely experimental and intentionally not exported and not being used in any module.",
                 "\nThis code enables user, few extra functionalities which are not in the code-base of tidycells. Thus the risk and other legal aspects solely lies with users who wants to use these. ",
                 "\nThe code are stored as R-data and hence is outside the validation scope of CRAN (or any other services in which tidycells codebase gets vetted). ",
                 "\nThe source code of these functionalities can be found in tidycells official website. ",
                 "\nUsers are invited to copy paste and verify the source code instead of using directly this module. \n\n"))
  Sys.sleep(1)
  if(interactive()){
    user_res <- rstudioapi_ask(title = "Going out of CRAN [warning]",
                               message = "You are going to load few functions which may interact with tidycells. However, these functionalities are not part of tidycells and is not vetted in CRAN. \nClick (or Write) ok to load these or cancel to exit.", 
                               is_question = TRUE)
    
    if(user_res=="ok") user_res <- TRUE
    
    if(!isTRUE(user_res)){
      abort("(User cancel) That's a good decision...")
    }
  }else{
    message(paste0("Running in non-interactive session.",
                   "\nI assume you (user) agree with the terms as displayed by warnings/messages.",
                   "\nWhich is applicable even if user use ‘suppressMessages’.",
                   "\n\nOnce Again:",
                   "\nYou are going to load few functions which may interact with tidycells. ",
                   "\nHowever, these functionalities are not part of tidycells and is not vetted in CRAN. "))
  }
  
  message(paste0("This is going to do following things:",
                 "\n1)	Install all suggested packages (if not present)",
                 "\n2)	Install <archive> (from https://github.com/jimhester/archive) if not present",
                 "\n3)	Install <remotes> (from CRAN) if not present (and if required)",
                 "\n4)	Enable support for 7z and rar file extraction using <archive>",
                 "\n5)	Enable plot method for exploration_findings."))
  
  try({
    # all tasks goes here
    not_available()
    
    this_suggests <- utils::installed.packages()["tidycells","Suggests"]
    this_suggests <- this_suggests %>% stringr::str_split(",|\n| ") %>% unlist()
    this_suggests <- this_suggests[nchar(this_suggests)>0]
    this_suggests <- this_suggests %>% stringr::str_detect("[a-zA-Z]") %>% this_suggests[.]
    
    inst_if_not_present <- function(pkg){
      if(!is_available(pkg)){
        try(utils::install.packages(pkg, repos = "https://cran.rstudio.com/"), silent = TRUE)
      }
    }
    
    this_suggests %>% map(inst_if_not_present)
    
    is_a_ok <- is_available("archive")
    if(is_a_ok){
      if(!("archive_extract" %in% getNamespaceExports("archive"))){
        is_a_ok <- FALSE
      }
    }
    
    # load extra_functionalities rds
    efn <- system.file("extdata","extra_functionalities.rds", package = "tidycells", mustWork = TRUE)
    ef <- readRDS(efn)
    
    if(!is_a_ok){
      if(!is_available("remotes")){
        utils::install.packages("remotes", repos = "https://cran.rstudio.com/")
      }
      Sys.sleep(0.5)
      if(is_available("remotes")){
        ef$task1_inst_github()
      }
      Sys.sleep(0.5)
    }
    
    if(is_available("archive")){
      decompress.file_7z <- ef$task2_archive_extract()
      
      decompress.file_rar <- decompress.file_7z
      
      assign("decompress.file_7z", decompress.file_7z, envir = globalenv())
      assign("decompress.file_rar", decompress.file_rar, envir = globalenv())
    }
    
    inst_if_not_present("collapsibleTree")
    inst_if_not_present("DiagrammeR")
    
    if(is_available("collapsibleTree") | is_available("DiagrammeR")){
      assign("plot.exploration_findings", ef$task3_explore_it_plot(), envir = globalenv())
    }
    
    
  }, silent = TRUE)
  
  return(invisible("You are on your own"))
}
