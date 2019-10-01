
#' @include tidycells-package.R

this_file_kinds <- function(){
  tibble(
    file_format = c("xls", "xlsx", "doc", "docx", 
                    "ppt", "pptx", "pdf", "tsv", "csv", "text", "html", "xml", "sas", 
                    "sav", "zsav", "por", "dta", "zip", "rar", "7z", "tar", "gz", 
                    "xz", "bz2"), 
    implemented = c(TRUE, TRUE, TRUE, TRUE, TRUE, 
                    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                    TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, 
                    TRUE), 
    file_kind = c("TableField Container", "TableField Container", 
                  "TableField Container", "TableField Container", "TableField Container", 
                  "TableField Container", "TableField Container", "TableField", 
                  "TableField", "TableField", "TableField Container", "TableField Container", 
                  "TableField", "TableField", "TableField", "TableField", "TableField", 
                  "FileField", "FileField", "FileField", "FileField", "FileField", 
                  "FileField", "FileField"), 
    compressed = c(FALSE, FALSE, FALSE, FALSE, 
                   FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                   FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE), 
    archived = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, 
                 TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
}

# save this file_kinds for quicker access in tidycells_pkg_env
tidycells_pkg_env$file_kinds <- this_file_kinds()


file_kind_maker <- function(fn, fkind, ftype, compressed, archived){
  fk <- list(name = fn, file_kind = fkind, file_type = ftype, is_compressed = compressed, is_archived = archived)
  if(fk$file_kind == "TableField"){
    class(fk) <- c("TableField", "list")
  }
  if(fk$file_kind == "TableField Container"){
    class(fk) <- c("TableFieldContainer", "list")
  }
  if(fk$file_kind == "FileField" & (fk$is_compressed | fk$is_archived)){
    class(fk) <- c("FileFieldCompressed", "list")
  }
  if(fk$file_kind == "FileField" & !fk$is_compressed){
    class(fk) <- c("FileField", "list")
  }
  fk
}

file_kind_identifier <- function(fn){
  fn <- as.character(fn)
  is_folder <- isTRUE(as.logical(file.info(fn)["isdir"]))
  if(is_folder){
    return(file_kind_maker(fn, "FileField", "folder", FALSE, FALSE))
  }
  
  file_type  <- detect_file_type(fn)
  
  if(file_type == "unknown"){
    return(file_kind_maker(fn, "unknown", file_type, FALSE, FALSE))
  }
  
  fkind_inf <- tidycells_pkg_env$file_kinds %>% filter(file_format == file_type)
  
  allowed_fkinds <- c("TableField Container", "TableField", "FileField", "unknown")
  
  if(fkind_inf$file_kind %in% allowed_fkinds){
    return(file_kind_maker(fn, fkind_inf$file_kind[1], file_type[1], fkind_inf$compressed[1], fkind_inf$archived[1]))
  }
  
  return(file_kind_maker(fn, "unknown", file_type, FALSE, FALSE))
  
}