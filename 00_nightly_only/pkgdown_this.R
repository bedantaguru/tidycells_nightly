
# designed for proxy connection run manually in other case
if(nchar(Sys.getenv("HTTPS_PROXY"))>0){
  
  require(stringr)
  rd_bk <- tempfile()
  
  file.copy("README.Rmd", rd_bk)
  
  rd_now <- readLines("README.Rmd")
  
  if(rd_now[19]!= "```" | rd_now[18]!= "library(tidycells)"){
    stop("check manually")
  }else{
    
    rd_now <- c(rd_now[1:19], "<style>
body {
text-align: justify}
</style>", rd_now[20:length(rd_now)])
    
    rd_now <- rd_now %>% paste0(collapse = "\n")
    
    rd_now <- str_replace_all(rd_now, "https://github.com/r-rudra/tidycells/blob/master/dev-notes.md", "dev-notes.html")
    
    writeLines(rd_now, "README.Rmd")
    
    unlink("docs", recursive = TRUE, force = TRUE)
    # unlink("pkgdown/favicon", recursive = TRUE, force = TRUE)
    # this is not required after 1.4.1 
    # pkgdown::build_favicons(overwrite = TRUE)
    pkgdown::build_site()
    #pkgdown:::build_site_external()
    ##############################
    #  after pkgdown manual tasks
    ##############################
    file.copy("vignettes/ext/read_cells.svg", "docs/articles/ext/")
    unlink("README.Rmd")
    file.copy(rd_bk, "README.Rmd")
  }
  
}
