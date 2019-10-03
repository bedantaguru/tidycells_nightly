


fname_process_1 <- function(fn, root_name){
  rn <- normalizePath(root_name, winslash = "/") %>% dirname() %>% paste0("^",.,"/")
  fn_n <- normalizePath(fn, winslash = "/")
  x0 <- stringr::str_detect(fn_n, rn)
  x1 <- stringr::str_remove_all(fn_n, rn)
  x1[!x0] <- NA
  x1
}

get_mother_well_name <- function(fn_id, rels){
  fid0 <- fn_id
  
  repeat({
    root <- rels$root[rels$f_id == fid0]
    rwn <- rels$well_name[rels$path==root]
    if(!is.na(rwn)) break()
    fid0 <- rels$f_id[rels$path==root]
  })
  
  rwn
}

fname_process_2 <- function(fn_id, rels){
  fn <- rels$path[rels$f_id==fn_id] %>% normalizePath(winslash = "/")
  
  if(!stringr::str_detect(fn, "decompress")){
    abort("Unknown error  occurred in filename processing type 2.\nPlease contact developer.")
  }
  
  #fn_n <- normalizePath(fn, winslash = "/")
  mother_wn <- get_mother_well_name(fn_id, rels)
  
  if(stringr::str_detect(basename(fn), "decompress")){
    mother_wn
  }else{
    fps <- stringr::str_split(fn, "/")[[1]]
    dtags <- which(stringr::str_detect(fps, "decompress"))
    
    fps[(min(dtags)+1):length(fps)] %>% 
      c(mother_wn, .) %>% 
      as.list() %>% 
      do.call("file.path",.)
  }
  
}

# this is used in explore_it
get_well_name_maps <- function(x, only_well_names = FALSE){
  
  proc_this <- TRUE
  if(hasName(x, "well_name")){
    if(!any(is.na(x$well_name))){
      proc_this <- FALSE
    }
  }
  
  if(proc_this){
    root_name <- x %>% filter(depth == 0, root == "<root>") %>% pull(path)
    
    x <- x %>% mutate(well_name = path %>% fname_process_1(root_name = root_name))
    
    xn <- x %>% arrange(depth) %>% mutate(f_id = seq_along(path))
    
    repeat({
      # temp folders 
      not_done <- xn %>% filter(is.na(well_name)) %>% arrange(depth) %>% pull(f_id)
      
      if(length(not_done)==0) break()
      
      fid0 <- not_done[1]
      
      xn$well_name[xn$f_id==fid0] <- fname_process_2(fid0, xn)
      
    })
  }else{
    xn <- x
  }
  
  
  
  if(only_well_names){
    xn2 <- xn %>% select(root, path, well_name)
    
    xn2 <- xn2 %>% left_join(xn2 %>% select(-root), by = c("root"="path"), suffix = c("_child", "_parent")) %>% 
      distinct(well_name_parent, well_name_child) %>% filter(well_name_parent!=well_name_child | is.na(well_name_parent))
    
    return(xn2)
  }
  
  xn 
  
}