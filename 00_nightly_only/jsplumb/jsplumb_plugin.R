
library(stringr)

library(dplyr)
library(purrr)

read_param_code <- function(file_name, code){
  if(!missing(file_name)){
    code <- suppressWarnings(readLines(file_name))
  }
  
  args_str <- code %>% str_extract_all("@:[a-zA-Z_0-9]+") %>% unlist() %>% unique()
  args_list <- args_str %>% str_replace("@:","")
  
  # store current CODE
  
  CODE_NOW <- code
  
  reset <- function(){
    CODE_NOW <<- code
  }
  
  get_code_now <- function(){
    CODE_NOW
  }
  
  # function to build CODE with parameters
  build_code <- function(...){
    args_passed <- list(...)
    args_passed <- args_passed[args_list]
    
    code_new <- CODE_NOW
    
    for(arg in args_list){
      arg_to_place <- args_passed[[arg]]
      if(length(arg_to_place)>1){
        arg_to_place <- paste0(arg_to_place, collapse = "\n")
      }
      code_new <- code_new %>% str_replace_all(paste0("@:",arg),arg_to_place) %>% paste(collapse = "\n")
    }
    
    CODE_NOW <<- code_new
    
    return(code_new)
  }
  
  handle <- list(code = get_code_now,
                 reset = reset,
                 parameter_list = args_list,
                 build_code =  build_code ,
                 parameterized_code = code)
  return(handle)
}


patch_html_to_param_code <- function(file_name,
                                     js_loc_str = "<script src=",
                                     css_loc_str = "<link href=",
                                     body_loc_str = "<p>jsplumb place here</p>"){
  
  htmlnow <- suppressWarnings(readLines(file_name)) %>% paste0(collapse = "\n")
  
  patch_this <- function(cnow, what, toput, replace = FALSE){
    loc <- str_locate(cnow, what)
    if(!is.na(loc[,"start"])){
      if(replace){
        pp <- substr(cnow, 1, loc[,"start"]-1)
        ap <- substr(cnow, loc[,"end"]+1, nchar(cnow))
      }else{
        pp <- substr(cnow, 1, loc[,"start"]-1)
        ap <- substr(cnow, loc[,"start"], nchar(cnow))
      }
      
      cnow <- paste0(pp, toput, ap)
    }
    cnow
  }
  
  htmlnow %>%
    patch_this(js_loc_str, "@:flowchart_js") %>%
    patch_this(css_loc_str, "@:flowchart_css") %>%
    patch_this(body_loc_str, "@:flowchart_body", replace = TRUE)
  
}




jsplumb_injection <- function(fn, proto_loc, code_base){
  #fn <- "ok/FileTypes.html"
  fold <- fn %>% str_replace(".html", "_Files")
  
  
  # clone
  fn_orig <- fn
  fold_orig <- fold
  
  tf <- tempfile(pattern = "jsplumb_test")
  dir.create(tf)
  
  file.copy(fn_orig, tf, recursive = T, overwrite = T)
  file.copy(fold, tf, recursive = T, overwrite = T)
  
  fn <- list.files(tf, ".html$", full.names = T)
  fold <- fn %>% str_replace(".html", "_Files")
  
  
  
  # proto_loc <- "inst/proto/"
  
  paths_this <- list()
  code_link <- list()
  if(file.exists(fold)){
    dir.create(file.path(fold, "jsplumb"), showWarnings = FALSE, recursive = TRUE)
    tothis <- file.path(fold, "jsplumb")
    file.copy(file.path(proto_loc,"flowchart-simple-jsplumb.css"), tothis, overwrite = TRUE)
    file.copy(file.path(proto_loc,"flowchart-simple-jsplumb.js"), tothis, overwrite = TRUE)
    file.copy(file.path(proto_loc,"jsplumb.min.js"), tothis, overwrite = TRUE)
    file.copy(file.path(proto_loc,"jsplumbtoolkit-defaults.css"), tothis, overwrite = TRUE)
    
    paths_this$css <- file.path(basename(fold), basename(tothis), list.files(tothis, ".css$"))
    paths_this$js <- file.path(basename(fold), basename(tothis), list.files(tothis, ".js$"))
    
    # order matters
    code_link$css <- paste0('<link rel="stylesheet" href="',paths_this$css,'">') %>% rev()
    code_link$js <- paste0('<script src="',paths_this$js,'"></script>') %>% rev()
  }else{
    stop("I don't know what to do (as of now)")
  }
  
  
  param_codes <- list(
    html = fn,
    js = file.path(tothis, "flowchart-simple-jsplumb.js"),
    css = file.path(tothis, "flowchart-simple-jsplumb.css")
  )
  
  htmlcode <- param_codes$html %>% patch_html_to_param_code(js_loc_str = "<><>", css_loc_str = "<><>") %>% read_param_code(code = .)
  htmlcode <- htmlcode$build_code(flowchart_body = code_base$body) %>% read_param_code(code = .)
  jscode <- param_codes$js %>% read_param_code()
  csscode <- param_codes$css %>% read_param_code()
  
  new_code <- list()
  
  
  
  new_code$html <- htmlcode$build_code(flowchart_js = code_link$js, flowchart_css = code_link$css)
  new_code$js <- jscode$build_code(addEndpoints = code_base$js$addEndpoints, instance_connect = code_base$js$instance_connect)
  new_code$css <- csscode$build_code(locations = code_base$css)
  
  writeLines(new_code$html, param_codes$html)
  writeLines(new_code$js, param_codes$js)
  writeLines(new_code$css, param_codes$css)
  
  
  utils::browseURL(fn)
  
  lo <- list(tloc = tf, copy = function(){
    dn <- dirname(fn_orig)
    unlink(fn_orig, recursive = T, force = T)
    unlink(fold_orig, recursive = T, force = T)
    file.copy(fn, dn, overwrite = T, recursive = T)
    file.copy(fold, dn, overwrite = T, recursive = T)
  })
  invisible(lo)
}


code_base_gen <- function(node_df, edge_df){
  
  gr <- DiagrammeR::create_graph(node_df, edge_df)
  (gr %>% DiagrammeR::to_igraph() %>% igraph::layout_with_sugiyama())[[2]] %>%
    dplyr::as_tibble() %>% dplyr::rename(x = V1,y = V2) %>% dplyr::bind_cols(node_df, .) -> node_df_wloc
  
  node_with_src_tar_raw <- edge_df %>% group_by(id = from) %>% summarise(sourceAnchors = n()) %>%
    full_join(edge_df %>% group_by(id = to) %>% summarise(targetAnchors = n()), by = "id")
  node_with_src_tar_raw <- node_with_src_tar_raw %>% mutate_all(~ifelse(is.na(.x), 0, .x))
  node_with_src_tar <- node_with_src_tar_raw %>% mutate(sourceAnchors = pmin(sourceAnchors, 3), targetAnchors = pmin(targetAnchors, 3))
  
  node_df_with_src_tar <- node_df_wloc %>% left_join(node_with_src_tar, by = "id")
  
  node_df_with_src_tar <- node_df_with_src_tar %>% mutate(top = round(max(y)-y)*12+2, left = round((x-min(x))*15)+2)
  
  node_df_with_src_tar <- node_df_with_src_tar %>% mutate(targetAnchors = pmin(sourceAnchors+targetAnchors,4)-sourceAnchors)
  
  which_side <- function(id_from, id_to, lr_only = F, tb_only = F, tht = 1, thl = 5){
    ft <- node_df_with_src_tar %>% filter(id == id_from) %>% pull(top)
    fl <- node_df_with_src_tar %>% filter(id == id_from) %>% pull(left)
    tt <- node_df_with_src_tar %>% filter(id == id_to) %>% pull(top)
    tl <- node_df_with_src_tar %>% filter(id == id_to) %>% pull(left)
    
    tb <- ifelse(tt > ft + tht , "Bottom", ifelse( tt < ft - tht,"Top", "Middle"))
    lr <- ifelse(tl > fl + thl  , "Right", ifelse(tl < fl-thl, "Left", "Middle"))
    
    if(lr_only) return(lr)
    if(tb_only) return(tb)
    #paste0(tb," + ",lr )
    c(tb, lr)
  }
  
  edge_df_sides <- edge_df
  edge_df_sides <- 1:nrow(edge_df_sides) %>%
    map_df(~{
      ddd0 <- edge_df_sides[.x,]
      lt1 <- which_side(ddd0$from, ddd0$to)
      lt2 <- which_side(ddd0$to, ddd0$from)
      ddd0$tb1 <- lt1[1]
      ddd0$lr1 <- lt1[2]
      ddd0$tb2 <- lt2[1]
      ddd0$lr2 <- lt2[2]
      ddd0
    })
  
  edge_df_sides <- edge_df_sides %>%
    mutate(from_tag = recode(lr1,
                             Right = "RightMiddle",
                             Middle = "BottomCenter",
                             Left = "LeftMiddle"),
           to_tag = recode(lr2,
                           Right = "RightMiddle",
                           Middle = "TopCenter",
                           Left = "LeftMiddle"))
  
  
  all_pos <- c("TopCenter","BottomCenter","LeftMiddle", "RightMiddle")
  
  id_wise_allowed_tag <- function(id_this, tag, type = "src"){
    
    nts <- node_df_with_src_tar %>% filter(id==id_this) %>% pull(sourceAnchors)
    ntt <- node_df_with_src_tar %>% filter(id==id_this) %>% pull(targetAnchors)
    
    atags <- edge_df_sides %>% filter(from==id_this) %>% pull(from_tag) %>% unique()
    atagt <- edge_df_sides %>% filter(to==id_this) %>% pull(to_tag) %>% unique()
    
    comm <- intersect(atags, atagt)
    
    rest_pos <- setdiff(all_pos, union(atags, atagt))
    
    atagt <- atagt %>% setdiff(comm)
    
    if(type == "src"){
      allowed_tags <- atags[seq(nts)]
    }else{
      allowed_tags <- atagt[seq(ntt)]
    }
    if(tag %in% allowed_tags){
      tag
    }else{
      if(tag %in% comm & type != "src"){
        rest_pos[1]
      }else{
        allowed_tags[1]
      }
    }
  }
  
  edge_df_sides$from_tag_final <- NA
  edge_df_sides$to_tag_final <- NA
  
  for(i in 1:nrow(edge_df_sides)){
    edge_df_sides$from_tag_final[i] <- id_wise_allowed_tag(edge_df_sides$from[i], edge_df_sides$from_tag[i])
    edge_df_sides$to_tag_final[i] <- id_wise_allowed_tag(edge_df_sides$to[i], edge_df_sides$to_tag[i], type = "tar")
  }
  
  instance_connect_str <- edge_df_sides %>% mutate(paste0('instance.connect({uuids: ["Window',from,from_tag_final,'", "Window',to,to_tag_final,'"]});')) %>%
    pull() %>% paste0(collapse = "\n")
  
  addEndpoints_df <- edge_df_sides %>% group_by(id = from) %>% summarise(src = list(unique(from_tag_final))) %>%
    full_join(edge_df_sides %>% group_by(id = to) %>% summarise(tar = list(unique(to_tag_final))), by = "id")
  
  
  
  
  addEndpoints_df <- addEndpoints_df %>%
    mutate(tar_final = map2(tar, src, ~{
      if(is.null(.x)){
        setdiff(all_pos, .y)
      }else{
        .x
      }
    }),
    src_final = map2(src, tar, ~{
      if(is.null(.x)){
        setdiff(all_pos, .y)
      }else{
        .x
      }
    }))
  
  
  addEndpoints_df <- addEndpoints_df %>% mutate(tag_part = map2_chr(src_final, tar_final, ~{
    paste0(', [',paste0('"',.x,'"', collapse = ", "),'], [', paste0('"',.y,'"', collapse = ", "),']);')
  }))
  
  addEndpoints_str <- addEndpoints_df %>% mutate(paste0('_addEndpoints("Window',id,'"', tag_part)) %>% pull()
  
  
  html_body_str_part <- node_df_with_src_tar %>%
    mutate(paste0('<div class="window jtk-node" id="flowchartWindow',id,'"><strong>',label,'</strong><br/><br/></div>')) %>%
    pull() %>% paste0(collapse = "\n")
  
  html_body_str <- paste0(
    '
<div id = "flowChartContainer1">

@:flowchart_css

<div>

			<div class="jtk-simple-flowchart-canvas canvas-wide flowchart-simple jtk-surface jtk-surface-nopan" id="canvas">
                ',html_body_str_part,'
            </div>

        </div>

        @:flowchart_js

</div>
'
  )
  
  
  css_str <- node_df_with_src_tar %>% mutate(css_tag = paste0('#flowchartWindow',id,' {
    top: ',top,'em;
    left: ',left,'em;
}')) %>% pull() %>% paste(collapse="\n\n")
  
  
  code_base <- list()
  
  code_base$css <- css_str
  
  code_base$js <- list(
    addEndpoints = addEndpoints_str,
    instance_connect = instance_connect_str)
  
  code_base$body <- html_body_str
  
  code_base
}
