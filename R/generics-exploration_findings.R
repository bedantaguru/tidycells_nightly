

stats_of_exploration_findings <- function(x){
  sta <- state(x)
  x <- as_tibble(x)
  info <- list()
  
  info$total_files <- x$path %>% unique() %>% length()
  
  if(sta == "with_content"){
    info$with_content <- TRUE
    info$content_size <- object.size(x$tfc)
    info$total_num_of_files_read <- sum(x$read_success)
    info$read_file_types <- unique(x$file_type[x$read_success])
    chk1 <- x %>% filter(is_readable, !read_success) %>% nrow()
    info$all_readable_success <- chk1==0
  }else{
    info$with_content <- FALSE
    info$total_readable_files <- x %>% filter(is_readable) %>% pull(path) %>% unique() %>% length()
    info$total_size_readable_files <- x %>% filter(is_readable) %>% distinct(path, size) %>% pull(size) %>% sum()
    info$readable_file_types <- x %>% filter(is_readable) %>% distinct(file_type) %>% pull()
  }
  
  info
}

#' @export
print.exploration_findings <- function(x, ...) {
  st <- stats_of_exploration_findings(x)
  gfn <- attr(x, "given_fname")
  flt_tag <- ""
  if(isTRUE(attr(x, "filtered"))){
    flt_tag <- "(filtered)"
  }
  if(is.null(gfn)){
    init_str <- cli_bb(paste0("Exploration Findings ", flt_tag,":"))
  }else{
    init_str <- paste0(cli_bb("Exploration Findings  of "), cli_g(gfn[1]) ,cli_bb(paste0(" ",flt_tag,":")))
  }
  
  # for with_content 
  if(st$with_content){
    cnt_str <- paste0(
      "\n", cli_r(cli_bs()), paste0(cli_b(paste0("With ",cli_bb("content")," (files are read)"))),
      "\n", cli_r(cli_bs()), paste0(cli_b("Total read files: "), st$total_num_of_files_read),
      "\n", cli_r(cli_bs()), paste0(cli_b("Total size of read files (in memory): "), 
                                    nice_file_size(st$content_size), " ", 
                                    ifelse(st$content_size>1024, paste0(cli_b("("), cli_b(st$content_size) , cli_b(" bytes)")), "")),
      ifelse(st$total_num_of_files_read>0,
             paste0("\n", cli_r(cli_bs()), paste0(cli_b("Read file-types: "), paste0(st$read_file_types, collapse = ", "))),
             "")
    )
  }else{
    cnt_str <-""
  }
  
  if(st$with_content){
    msg <- paste0(
      init_str,
      cnt_str,
      "\n", cli_bs(), paste0(cli_b("Total files: "), st$total_files),
      ifelse(!info$all_readable_success, 
             paste0(cli_r(paste0("\n", cli_bs(), "At least one readable file ",cli_br("did not")," read successfully."))), 
             "")
    )
  }else{
    msg <- paste0(
      init_str,
      cnt_str,
      "\n", cli_bs(), paste0(cli_b("Total files: "), st$total_files),
      "\n", cli_bs(), paste0(cli_b("Total readable files: "), st$total_readable_files),
      "\n", cli_bs(), paste0(cli_b("Total size of readable files: "), nice_file_size(st$total_size_readable_files), " ", 
                             ifelse(st$content_size>1024, paste0(cli_b("("), cli_b(st$total_size_readable_files) , cli_b(" bytes)")), "")),
      ifelse(st$total_readable_files>0, 
             paste0("\n", cli_bs(), paste0(cli_b("Readable file-types: "), paste0(st$readable_file_types, collapse = ", "))),
             ""),
      "\n"
    )
  }
  
  cat(msg)
}


attach_state.exploration_findings <- function(x){
  if(hasName(x, "tfc") & hasName(x, "content") & hasName(x, "read_success")){
    x <- set_state(x, "with_content")
  }else{
    x <- set_state(x, "")
  }
  x
}

#' @export
filter.exploration_findings <- function(.data, ...){
  .datat <- as_tibble(.data)
  .datat <- filter(.datat, ...)
  class(.datat) <- exploration_findings_class
  if(nrow(.datat)!=nrow(.data)){
    attr(.datat,"filtered") <- TRUE
  }
  if(nrow(.data)>0 & nrow(.datat) == 0){
    dts <- rlang::enquos(...) %>% unlist() %>% as.character()
    dts <- dts %>% stringr::str_detect("content") %>% dts[.]
    dts <- dts %>% stringr::str_detect("[A-Z]") %>% dts[.]
    chk <- length(dts) > 0 
    if(chk){
      msg_once("Seems like you are filtering 'exploration_findings' based on content.",
               "\nKindly note that content is in lower case.",
               "\nThe string with which you are comparing should be also in lower case.",
               "\nCheck once whether that is the case.")
    }
  }
  .datat
}

