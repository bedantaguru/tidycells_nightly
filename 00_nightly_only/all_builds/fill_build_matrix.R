

library(dplyr)
library(stringr)
library(purrr)

bm <- readxl::read_excel("00_nightly_only/all_builds/all_builds_manual.xlsx")

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

saveRDS(bm, "00_nightly_only/all_builds/all_builds_with_log")

get_r_version <- function(log){
  exp <- try(str_detect(log, "using R version") %>% log[.] %>% .[[1]] %>% str_split("R version") %>% unlist() %>% rev() %>% .[[1]] %>% paste0("R version", .), silent = T)
  if(inherits(exp, "try-error")){
    exp <- try(str_detect(log, "R Under development") %>% log[.] %>% .[[1]] %>% str_split("R Under development") %>% unlist() %>% rev() %>% .[[1]] %>% paste0("R Under development", .), silent = T)
  }
  if(inherits(exp, "try-error")){
    exp <- ""
  }
  exp %>% str_split("--") %>% unlist() %>% .[[1]] %>% str_trim()
}

get_platform <- function(log){
  exp <- try(str_detect(log, "using platform") %>% log[.] %>% .[[1]] %>% str_split("platform: ") %>% unlist() %>% rev() %>% .[[1]], silent = T)
  if(inherits(exp, "try-error")){
    ""
  }else{
    exp
  }
}

get_av_tv_os_desc <- function(log){
  # Running under:
  exp <- try(str_detect(log, "Running under") %>% log[.] %>% .[[1]] %>% str_split("Running under: ") %>% unlist() %>% rev() %>% .[[1]], silent = T)
  if(inherits(exp, "try-error")){
    ""
  }else{
    exp
  }
}

bm <- bm %>% mutate(rv_from_log = log %>% map_chr(get_r_version),
                    pt_from_log = log %>% map_chr(get_platform), 
                    os_desc_from_log = log %>% map_chr(get_av_tv_os_desc))

bm <- bm %>% mutate(status_ok = log %>% map_lgl(~str_detect(.x, "Status: OK") %>% any))
bm <- bm %>% mutate(State = toupper(State))
bm <- bm %>% mutate(any_status = bm$log %>% map_lgl(~.x %>% str_detect("Status:") %>% any))

# manual variafication needed 

bm %>% filter(any_status, State!="OK")

saveRDS(bm, "00_nightly_only/all_builds/all_builds_with_log")

# these are good
bm_supervised <- bm %>% mutate(State = ifelse(any_status, "OK", State))
bm_supervised <- bm_supervised %>% mutate(`R Version` = ifelse(nchar(rv_from_log)>0, rv_from_log, `R Version`))
bm_supervised <- bm_supervised %>% mutate(Platform = ifelse(nchar(pt_from_log)>0, pt_from_log, Platform))
bm_supervised <- bm_supervised %>% mutate(`OS Description` = ifelse(nchar(os_desc_from_log)>0, os_desc_from_log, `OS Description`))

saveRDS(bm_supervised, "00_nightly_only/all_builds/all_builds_with_log_supervised")

all_runs <- bm_supervised %>% select(Package, Version, `Submit Date`, Where, `OS Type`, `OS Description`, `R Version`, `R Version Tag`, Platform, State)

# please add these in manual stages
local_builds <- all_runs %>% filter(State=="OK") %>% .[c(1,2),]
local_builds$Where <- "Local"
local_builds$`OS Type` <- "Windows"
local_builds$`OS Description`<- "Windows 10 x64 (build 17134)"
local_builds$`R Version` <-c("R version 3.6.1 (2019-07-05)","R version 3.6.2 (2019-12-12)")
local_builds$`R Version Tag` <-""
local_builds$Platform <- "x86_64-w64-mingw32/x64 (64-bit)"
local_builds$State <- "OK"

all_runs <- all_runs %>% bind_rows(local_builds)
all_runs <- all_runs %>% mutate(`Submit Date` =as.Date(`Submit Date`))

writexl::write_xlsx(all_runs, "00_nightly_only/all_builds/final_build_table.xlsx")

all_runs %>% filter(Where=="RHub")
all_runs %>% filter(Where=="RHub",State=="OK")

fine_runs <- all_runs %>% filter(State=="OK")

seqs <- tibble(Where = c("RHub", "AppVeyor", "Travis", "WinBuilder","Local"),
               prio = c(5,4,3,2,1))

fine_runs <- fine_runs %>% left_join(seqs, by = "Where")
fine_runs <- fine_runs %>% mutate(`OS Description`=ifelse(is.na(`OS Description`), "", `OS Description`),
                                  `R Version Tag`=ifelse(is.na(`R Version Tag`), "", `R Version Tag`))

to_text <- function(d){
  txt <- paste0("### ", d$Where[1])
  
  d <- d %>% mutate(trv = paste0(ifelse(nchar(`R Version Tag`)>0, paste0("(",`R Version Tag`,") "), ""), `R Version`))
  d <- d %>% mutate(tos = paste0(ifelse(nchar(`OS Description`)>0, `OS Description`, `OS Type`)," [", Platform,"]"))
  d1 <- d %>% group_by(tos) %>% summarise(rt = paste0("  * ", trv, collapse = "\n"))
  lp <- paste0("* ",d1$tos, "\n", d1$rt, collapse = "\n")
  if(d$Where[1] == "RHub"){
    paste0(txt,"\n\n","#### Successful Builds", "\n\n", lp, "\n\n" ,
           "See other builds in [Dev-Notes](https://github.com/r-rudra/tidycells/blob/master/dev-notes.md#r-hub-other-builds)","\n\n")
  }else{
    paste0(txt,"\n\n", lp, "\n")
  }
  
}

txt0 <- fine_runs %>% group_by(prio, Where) %>% group_split() %>% map_chr(to_text)

txt1 <- paste0(txt0, collapse = "\n")

writeClipboard(txt1)
