

explore_it <- function(file_name){
  fkind <- file_kind_identifier(file_name)
  explore_it_internal(fkind, depth = 0, max_depth = 2)
}

explore_it_internal <- function(fkind, depth = 0, max_depth = Inf, ...){
  UseMethod("explore_it_internal")
}

explore_it_internal.TableField <- function(fkind, depth,  ...){
  
}

explore_it_internal.TableFieldContainer <- function(fkind, depth,  ...){
  
}
explore_it_internal.FileField <- function(fkind, depth, max_depth = Inf, ...){
  out_df <- tibble(root = depth, 
                   id = depth+1, 
                   name = fkind$name, 
                   file_kind = list(fkind), 
                   is_empty = FALSE, 
                   content = list(NULL))
  files_in_field <- list.files(fkind$name, full.names = TRUE)
  if(length(files_in_field)==0){
    out_df$is_empty <- TRUE
    return(out_df)
  }
  new_df <-  tibble(root = depth+1, 
                    id = depth+2, 
                    name = files_in_field)
  new_df <- new_df %>% mutate(file_kind = name %>% map(file_kind_identifier))
  
  #@Dev
  xx <- new_df$file_kind %>% map(~explore_it_internal(.x, depth = depth+1, max_depth = max_depth))
  
}

explore_it_internal.FileFieldCompressed <- function(fkind, depth, max_depth = Inf,  ...){
  
}

explore_it_internal.default <- function(fkind, depth, ...){
  tibble(root = depth, 
         id = depth+1, 
         name = fkind$name, 
         file_kind = list(fkind), 
         is_empty = NA, 
         content = list(NULL))
}
