
# @Dev


clean_colnames_for_stack_table_block_parts <- function(x){
  x %>% stringr::str_trim() %>% tolower
}

# for single block
stack_table_block_parts <- function(dat){
  if(!is_cell_df(dat)){
    abort("A cell-df expected")
  }
  out <- list()
  mat_form <- as.matrix(dat)
  out$data <- mat_form
  mat_form[is.na(mat_form)]<-""
  out$possible_headers <- list(
    top = mat_form[1,],
    left = mat_form[,1],
    bottom = mat_form[nrow(mat_form),],
    right = mat_form[,ncol(mat_form)]
  )
  out$possible_headers <- out$possible_headers %>% map(~.x %>% clean_colnames_for_stack_table_block_parts)
  class(out) <- c(class(out), "stack_table_block_parts") %>% unique()
  out
}

# tfc for table field collection
stack_table_block <- function(tfc){
  tfc_blocks <- tfc %>% map(detect_table_block)
  tfc_blocks <- tfc_blocks %>% imap(~.x %>% mutate(gid = paste0(.y, "_",gid)) %>% split(.$gid)) %>% 
    reduce(c)
  rtab_parts <- tfc_blocks %>% map(stack_table_block_parts)
  rtab_parts_joins <- rtab_parts %>% reduce(table_block_stacker)
}



table_block_stacker <- function(rtab_prt1, rtab_prt2){
  if(inherits(rtab_prt1, "stack_table_block_parts")){
    # case of single stack_table_block_parts as 1st argument
    mrg_inf <- mergeable_info_rtab_parts(rtab_prt1, rtab_prt2)
    if(nrow(mrg_inf)>0){
      mrg <- merge_rtab_parts(rtab_prt1, rtab_prt2, mrg_inf)
      return(list(mrg))
    }else{
      return(list(rtab_prt1, rtab_prt2))
    }
  }else{
    mrg_infs <- rtab_prt1 %>% map(~mergeable_info_rtab_parts(.x, rtab_prt2))
    mrg_infs_chks <- mrg_infs %>% map_lgl(~nrow(.x)>0)
    if(any(mrg_infs_chks)){
      new_rtab_parts <- rtab_prt1
      for(tars in which(mrg_infs_chks)){
        new_rtab_parts[[tars]]<- merge_rtab_parts(new_rtab_parts[[tars]], rtab_prt2, mrg_infs[[tars]])
      }
      return(new_rtab_parts)
    }else{
      return(c(rtab_prt1, list(rtab_prt2)))
    }
  }
  list(rtab_prt1)
}


mergeable_info_rtab_parts <- function(rtab_prt1, rtab_prt2){
  
  all_possible <- expand.grid(
    n1 = names(rtab_prt1$possible_headers), 
    n2 = names(rtab_prt2$possible_headers), stringsAsFactors = FALSE)
  
  all_possible <- all_possible %>% 
    # it is basically --> dplyr::rowwise
    group_by(seq_along(n1)) %>% 
    mutate(is_match = (length(intersect(rtab_prt1$possible_headers[[n1]], 
                                        rtab_prt2$possible_headers[[n2]])) == min(
                                          length(rtab_prt1$possible_headers[[n1]]),
                                          length(rtab_prt2$possible_headers[[n2]])
                                        ))) %>% 
    ungroup() %>% 
    select(n1, n2, is_match)
  
  if(any(all_possible$is_match)){
    all_possible <- all_possible %>% filter(is_match) %>% select(-is_match)
    all_possible[1,]
  }else{
    #return empty frame
    data.frame()
  }
  
}

merge_rtab_parts <- function(rtab_prt1, rtab_prt2, mrg_inf){
  new_rtab_prt <- rtab_prt1
  d1 <- rtab_parts_transforms[[mrg_inf$n1[1]]](rtab_prt1$data)
  d2 <- rtab_parts_transforms[[mrg_inf$n2[1]]](rtab_prt2$data)
  djoin <- d1 %>% bind_rows(d2)
  djoin <- rtab_parts_transforms[[mrg_inf$n1[1]]](rtab_prt1$data, cn = TRUE) %>% bind_rows(djoin)
  djoin <- as.matrix(djoin)
  colnames(djoin) <- NULL
  
  new_rtab_prt$data <- djoin
  
  new_rtab_prt$possible_headers <- list(
    top = djoin[1,],
    left = djoin[,1],
    bottom = djoin[nrow(djoin),],
    right = djoin[,ncol(djoin)]
  )
  new_rtab_prt$possible_headers <- new_rtab_prt$possible_headers %>% map(~.x %>% clean_colnames_for_stack_table_block_parts)
  new_rtab_prt
}

rtab_parts_transforms <- list(
  top = function(x, cn = FALSE){
    if(cn){
      d <- x[1,] %>% t() %>% as_tibble
    }else{
      d <- x[-1,] %>% as_tibble
    }
    colnames(d) <- x[1,] %>% clean_colnames_for_stack_table_block_parts()
    d
  },
  left = function(x, cn = FALSE){
    if(cn){
      d <- x[,1] %>% t() %>% as_tibble()
    }else{
      d <- x[,-1] %>% t() %>% as_tibble()
    }
    colnames(d) <- x[,1] %>% clean_colnames_for_stack_table_block_parts()
    d
  },
  bottom = function(x, cn = FALSE){
    if(cn){
      d <- x[nrow(x),] %>% t() %>% as_tibble
    }else{
      d <- x[-nrow(x),] %>% as_tibble
    }
    colnames(d) <- x[nrow(x),] %>% clean_colnames_for_stack_table_block_parts()
    d
  },
  right = function(x, cn = FALSE){
    if(cn){
      d <- x[,ncol(x)] %>% t() %>% as_tibble()
    }else{
      d <- x[,-ncol(x)] %>% t() %>% as_tibble()
    }
    colnames(d) <- x[,ncol(x)] %>% clean_colnames_for_stack_table_block_parts()
    d
  }
)



