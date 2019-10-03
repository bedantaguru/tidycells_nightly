

stats_of_exploration_findings <- function(x){
  info <- list()
  
  info$total_files <- x$path %>% unique() %>% length()
  info$total_readable_files <- x %>% filter(is_readable) %>% pull(path) %>% unique() %>% length()
  info$total_size_readable_files <- x %>% filter(is_readable) %>% distinct(path, size) %>% pull(size) %>% sum()
  info$readable_file_types <- x %>% filter(is_readable) %>% distinct(file_type) %>% pull()
  
  info
}

#' @export
print.exploration_findings <- function(x, ...) {
  st <- stats_of_exploration_findings(x)
  gfn <- attr(x, "given_fname")
  if(is.null(gfn)){
    init_str <- cli_bb("Exploration Findings :")
  }else{
    init_str <- paste0(cli_bb("Exploration Findings of "), cli_g(gfn[1]) ,cli_bb(":"))
  }
  
  msg <- paste0(
    init_str,
    "\n", cli_bs(), paste0(cli_b("Total files: "), st$total_files),
    "\n", cli_bs(), paste0(cli_b("Total readable files: "), st$total_readable_files),
    "\n", cli_bs(), paste0(cli_b("Total size of readable files: "), st$total_size_readable_files),
    "\n", cli_bs(), paste0(cli_b("Readable file-types: "), paste0(st$readable_file_types, collapse = ", ")),
    "\n"
  )
  cat(msg)
}