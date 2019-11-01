
# @Dev
#' Explore recursively the content of the file or folder
#'
#' @param file_name local file or folder path
#' @param max_depth max recursion depth (default is `Inf`)
#'
#' @return a object of exploration_findings_class
#' @export
#'
#' @examples
#' fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
#' explore_it(fold)

explore_it <- function(file_name, max_depth = Inf){
  fkind <- file_kind_identifier(file_name)
  exp_raw <-explore_it_internal(fkind, max_depth = max_depth)
  exp_with_wn <- get_well_name_maps(exp_raw)
  exp_with_wn$size <- file.info(exp_with_wn$path)[["size"]]
  attr(exp_with_wn, "given_fname") <- file_name
  class(exp_with_wn) <- exploration_findings_class
  exp_with_wn
}

explore_it_output_maker <- function(fkind, root = "<root>", depth = 0, explored = TRUE, empty = FALSE, readable = FALSE){
  tibble(root = root, 
         path = normalizePath(fkind$name), 
         depth = depth,
         name = basename(fkind$name),
         is_temp_file = fkind$is_temporary_file,
         file_kind = fkind$file_kind,
         file_kind_class = class(fkind)[1],
         file_type = fkind$file_type,
         file_kind_info = list(fkind), 
         is_empty = empty, 
         is_readable = readable, 
         is_explored = explored)
}

explore_it_internal <- function(fkind, root = "<root>", depth = 0, max_depth = Inf, ...){
  UseMethod("explore_it_internal")
}

explore_it_internal.TableField <- function(fkind, root = "<root>", depth = 0,   ...){
  explore_it_output_maker(fkind, root, depth, readable = fkind$is_implemented[1])
}

explore_it_internal.TableFieldContainer <- function(fkind, root = "<root>", depth = 0,  ...){
  explore_it_output_maker(fkind, root, depth, readable = fkind$is_implemented[1])
}

explore_it_internal.FileField <- function(fkind, root = "<root>", depth = 0, max_depth = Inf, ...){
  
  files_in_field <- list.files(fkind$name, full.names = TRUE)
  
  if(length(files_in_field)==0){
    return(explore_it_output_maker(fkind, root, depth, empty = TRUE))
  }
  
  if(depth >= max_depth){
    return(explore_it_output_maker(fkind, root, depth, explored = FALSE))
  }
  
  
  out_df <- explore_it_output_maker(fkind, root, depth)
  
  fks <- files_in_field %>% map(~file_kind_identifier(.x, temporary_file = fkind$is_temporary_file))
  
  new_df <- fks %>% map_df(~explore_it_internal(.x, root = out_df$path[1], depth = depth+1, max_depth = max_depth))
  
  out_df %>% bind_rows(new_df) %>% unique()
  
}

explore_it_internal.FileFieldCompressed <- function(fkind, root = "<root>", depth = 0, max_depth = Inf,  ...){
  if(depth >= max_depth){
    return(explore_it_output_maker(fkind, root, depth, explored = FALSE))
  }
  out_df <- explore_it_output_maker(fkind, root, depth)
  newfk <- decompress_file(fkind)
  new_df <- explore_it_internal(newfk, root = out_df$path[1], depth = depth+1, max_depth = max_depth)
  
  out_df %>% bind_rows(new_df) %>% unique()
  
}

explore_it_internal.default <- function(fkind, root = "<root>", depth = 0, ...){
  explore_it_output_maker(fkind, root, depth, explored = FALSE, readable = FALSE)
}
