


as_tfc <- function(x, ...){
  UseMethod("as_tfc")
}

as_tfc.list <- function(x, ...){
  xtfc <- x %>% map(as_cell_df)
  
  # name fix
  
  if(is.null(names(xtfc))){
    names(xtfc) <- paste0("Table_Field_", seq_along(xtfc))
  }else{
    nms <- names(xtfc)
    nms <- nms[!is.na(nms)]
    nms <- nms[nchar(nms)>0]
    nms <- unique(nms)
    if(length(nms)!=length(xtfc)){
      nmap <- tibble(nms = names(xtfc), seq = seq_along(nms))
      nmap <- nmap %>% mutate(is_blank = nchar(nms)==0)
      nmap_blnk <- nmap %>% filter(is_blank)
      nmap <- nmap %>% mutate(nn = nms)
      if(nrow(nmap_blnk)>0){
        nmap_blnk <- nmap_blnk %>% mutate(nn = paste0("Table_Field_", seq_along(nms)))
        nmap <- nmap %>% filter(!is_blank) %>% bind_rows(nmap_blnk)
      }
      
      if(any(duplicated(nmap$nn))){
        nmapl <- nmap %>% group_by(nn) %>% group_split()
        nmap <- nmapl %>% map_df(~{
          if(nrow(.x)>1){
            .x <- .x %>% mutate(nn = paste0(nn, seq_along(nn)))
          }
          .x
        })
      }
      nmap <- nmap %>% arrange(seq)
      names(xtfc) <- nmap$nn
    }
  }
  
  
  class(xtfc) <- Table_Field_Container_class
  xtfc
}

as_tfc.cell_df <- function(x, ...){
  xtfc <- list(Table_Field_1 = x)
  class(xtfc) <- Table_Field_Container_class
  xtfc
}

as_tfc.tbl <- function(x, ...){
  xtfc <- list(Table_Field_1 = as_cell_df(x, take_col_names = TRUE))
  class(xtfc) <- Table_Field_Container_class
  xtfc
}

as_tfc.data.frame <- as_tfc.tbl

as_tfc.matrix <- as_tfc.tbl