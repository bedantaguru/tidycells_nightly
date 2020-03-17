

#' @export
print.cell_df <- function(x, ...) {
  d <- x
  
  msg <- c(paste0(
    "A ",
    cli_bb("Cell Data Frame"),
    " (equivalently a ",
    cli_b("Table Field"),
    ")"
  ))
  
  if (hasName(d, "type")) {
    msg <- c(
      msg,
      paste0(
        "With ",
        cli_b("Value/Attribute Classification")
      )
    )
  }
  
  msg <- c(
    msg,
    paste0(
      cli_bs(), "To see cell stats, call ",
      cli_b("summary()")
    )
  )
  msg <- c(
    msg,
    paste0(
      cli_bs(), "To see cell structure, call ",
      cli_b("plot()")
    )
  )
  
  if (is_available("cli")) {
    msg <- c(
      msg,
      paste0(cli_bs(), "Content:")
    )
  }
  
  msg <- c(msg)
  cat(paste0(paste0(msg, collapse = "\n"), "\n"))
  
  if (is_available("cli")) {
    if (length(list(...))) {
      t_msg <- d %>%
        tibble::as_tibble() %>%
        format(...)
    } else {
      t_msg <- d %>%
        tibble::as_tibble() %>%
        format(n = 4, width = 40L)
    }
    
    t_msg <- t_msg[-c(1, 3)]
    t_msg[1] <- cli_b(t_msg[1])
    
    cat("\n")
    t_msg %>%
      stringr::str_split("\n") %>%
      unlist() %>%
      cli_box()
  }
}


#' @export
summary.cell_df <- function(object, ..., no_print = FALSE) {
  d <- object
  
  ds <- d %>%
    summarise(
      mnr = min(row), mxr = max(row),
      mnc = min(col), mxc = max(col),
      nch = sum(data_type == "character"),
      nnum = sum(data_type == "numeric")
    )
  ds$nrow <- nrow(d)
  
  msg <- c(paste0(
    "A ", cli_bb("Cell Data Frame"), cli_b(" (Table Field)"),
    " with ", cli_br(ds$nrow), " values"
  ))
  
  nr <- (ds$mxr - ds$mnr + 1)
  nc <- (ds$mxc - ds$mnc + 1)
  
  msg <- c(
    msg,
    paste0(cli_bs(), cli_bb("Dimension: "), cli_br(nr), " x ", cli_br(nc))
  )
  
  msg <- c(
    msg,
    paste0(cli_bs(), cli_bb("Content: "), cli_br(ds$nch), " characters and ", cli_br(ds$nnum), " numbers")
  )
  
  ddensity <- nrow(d) / (nr * nc)
  
  msg <- c(
    msg,
    paste0(cli_bs(), cli_bb("Density: "), cli_br(paste0(round(ddensity * 100, 1), "%")))
  )
  
  if (hasName(d, "type")) {
    ds2 <- d %>% summarise(
      nval = sum(type == "value"),
      natt = sum(type == "attribute"),
      nemp = sum(type == "empty")
    )
    
    msg <- c(
      msg,
      paste0(cli_bs(), cli_bb("Types: "), cli_br(ds2$nval), " values, ", cli_br(ds2$natt), " attributes and ", cli_br(ds2$nemp), " empty cells")
    )
  }
  
  if (!no_print) {
    cat(paste0(paste0(msg, collapse = "\n"), "\n"))
  }
  
  return(invisible(ds))
}

#' @export
plot.cell_df <- function(x, ...,
                         fill,
                         # ggplot2 specific
                         no_fill = FALSE,
                         adaptive_txt_size = TRUE, txt_size = 3, txt_alpha = 1, no_txt = FALSE, 
                         fill_alpha = 1,
                         background = NULL,
                         # common
                         no_plot = FALSE,
                         # DT specific
                         in_shiny = FALSE, 
                         shrink = TRUE, 
                         shrink_length = 20) {
  
  pltmode <- getOption("tidycells.plot_mode")
  
  if(is.null(pltmode)) pltmode <- "auto"
  
  if(length(pltmode)!=1) pltmode <- "auto"
  
  if(!(pltmode %in% c("auto","DT","ggplot2"))) pltmode <- "auto"
  
  if(pltmode=="auto"){
    if(is_available("ggplot2")){
      pltmode <-"ggplot2"
    }else{
      if(is_available("DT")){
        pltmode <-"DT"
      }else{
        abort("Either {ggplot2} or {DT} package is required for plotting a cell-df")
      }
    }
  }
  
  # finally either DT or ggplot2
  if(pltmode=="DT"){
    plot_cell_df_DT(x, 
                    fill, 
                    in_shiny = in_shiny,
                    shrink = shrink,
                    shrink_length = shrink_length, 
                    no_plot = no_plot, ...)
  }else{
    # pltmode=="ggplot2" case
    plot_cell_df_ggplot2(x, 
                         fill, 
                         no_fill = no_fill, 
                         adaptive_txt_size = adaptive_txt_size, 
                         txt_size = txt_size, 
                         txt_alpha = txt_alpha, 
                         no_txt = no_txt, 
                         no_plot = no_plot, 
                         fill_alpha = fill_alpha, 
                         background = background, ...)
  }
}





#' @export
as.matrix.cell_df <- function(x, ...) {
  x <- x %>% select(row, col, data_type, value)
  validate_cells(x)
  d <- x
  d <- unset_cell_df_class(d)
  m <- d %>%
    select(-data_type) %>%
    tidyr::spread(col, value) %>%
    select(-row) %>%
    as.matrix()
  colnames(m) <- NULL
  row.names(m) <- NULL
  m
}

#' @export
as.data.frame.cell_df <- function(x, row.names, optional, ...) {
  d <- x
  do <- d %>% as.matrix.cell_df()
  as.data.frame(do, stringsAsFactors = FALSE)
}


state.cell_df <- function(x, ...){
  
  st <- NULL
  
  if(hasName(x, "type")){
    st <- c(st, "va_classified")
  }
  
  if(hasName(x, "gid")){
    st <- c(st, "with_table_blocks")
  }
  
  if(!is.null(attr(x, "meta"))){
    st <- c(st, "with_meta_for_table_blocks")
  }
  
  if(!is.null(attr(x, "meta_intra_block_dist"))){
    st <- c(st, "with_meta_for_intra_block_dist")
  }
  
  formalize_state(st)
}


calc_meta_for_tf <- function(x, fresh = FALSE){
  
  if(!("with_table_blocks" %in% state(x))){
    abort("Need <table_blocks>")
  }
  
  last_meta <- attr(x, "meta")
  
  refresh <- FALSE
  if(is.null(last_meta) | !is.data.frame(last_meta) | fresh | 
     !hasName(last_meta, "gid") | !hasName(last_meta, "content") | !hasName(last_meta, "nrow") | !hasName(last_meta, "ncol") | 
     !hasName(last_meta, "size")
  ){
    refresh <- TRUE
  }
  
  # this is for obsolete meta
  if(is.data.frame(last_meta)){
    if(hasName(last_meta, "gid")){
      if(length(unique(intersect(x$gid, last_meta$gid))) < length(unique(x$gid))){
        refresh <- TRUE
      }
    }
  }
  
  if(!refresh){
    return(last_meta)
  }
  
  xl <- x %>% as_tibble() %>% split(.$gid)
  last_meta <- tibble(id = seq_along(xl), gid = xl %>% map("gid") %>% map(1) %>% unlist())
  
  xl <- xl[last_meta$gid]
  
  xl <- xl %>% map(~new_cell_df(.x, minimal = TRUE))
  
  last_meta <- last_meta %>% mutate(content = xl %>% map_chr(get_content))
  
  last_meta <- last_meta %>% mutate(nrow = xl %>% map_dbl(~max(.x$row)-min(.x$row)+1))
  
  last_meta <- last_meta %>% mutate(ncol = xl %>% map_dbl(~max(.x$col)-min(.x$col)+1))
  
  last_meta <- last_meta %>% mutate(size = ncol*nrow)
  
  last_meta
  
}

calc_meta_intra_block_dist_for_tf <- function(x, fresh = FALSE){
  
  if(!("with_table_blocks" %in% state(x))){
    abort("Need <table_blocks>")
  }
  
  lm <- attr(x, "meta_intra_block_dist")
  
  refresh <- FALSE
  if(is.null(lm) | !is.data.frame(lm) | fresh | 
     !hasName(lm, "gid1") | !hasName(lm, "gid2") | !hasName(lm, "d")
  ){
    refresh <- TRUE
  }
  
  # this is for obsolete meta_intra_block_dist
  # this check may be made more robust
  if(is.data.frame(lm)){
    if(hasName(lm, "gid1") & hasName(lm, "gid2")){
      if(length(unique(intersect(x$gid, c(lm$gid1, lm$gid2)))) < length(unique(x$gid))){
        refresh <- TRUE
      }
    }
  }
  
  if(!refresh){
    return(lm)
  }
  
  # nearby_threshold sqrt(2) gives corner cases and 1.9 is upper value than that
  intra_block_dist(x, nearby_threshold = 1.9)
  
}

# transpose operation

t.cell_df <- function(x){
  x %>% mutate(row0 = row, row = col, col = row0) %>% select(-row0)
}

# flip operation 
flip_cell_df <- function(x, by_row = TRUE){
  if(by_row){
    x %>% mutate(row = nrow(x)-row) 
  }else{
    x %>% mutate(col = ncol(x)-col)
  }
}

# rotate operation
rotate_cell_df <- function(x, clockwise = F){
  if(clockwise){
    x %>% flip_cell_df(by_row = T) %>% t
  }else{
    x %>% flip_cell_df(by_row = F) %>% t
  }
} 

mutate.cell_df <- function(.data, ..., direct = FALSE, join = 0, join_table_blocks_around = join, corner_join = FALSE, refresh = FALSE){
  dthis <- .data
  dthis <- as_tibble(dthis)
  
  if(!direct){
    dts <- rlang::enquos(...)
    gid_present <- names(dts) %>% stringr::str_detect("gid_") %>% any()
    
    if(gid_present){
      other_than_gid_present <- names(dts) %>% stringr::str_detect("gid_")
      other_than_gid_present <- any(!other_than_gid_present)
      gid_present_in_col <- colnames(dthis) %>% stringr::str_detect("gid_") %>% any()
      
      body_gid_present <- dts %>% unlist() %>% as.character() %>% stringr::str_detect("gid_") %>% any()
      
      if(other_than_gid_present){
        abort("There are other column specification in mutate apart from column(s) starting with 'gid_'. \n(you may opt for 'direct = TRUE')")
      }
      
      if(!body_gid_present){
        # @Dev#  this check need to update properly  not working for two gid cases mutate(gid_9 = gid_29+gid_13+gid_14, gid_1 = 13+1)
        abort("Group id merge specification is not correct. Example of such expression: gid_1 = gid_1 + gid_2")
      }
      
      if(gid_present_in_col){
        msg_once("column(s) starting with 'gid_' will be interpreted differently. (you may opt for 'direct = TRUE')")
      }
      
      new_gid_map <- dts %>% seq_along %>% 
        map_df(~{
          #@Dev changed from as.character to rlang::as_label for following warning
          # Warning message:
          #   Using `as.character()` on a quosure is deprecated as of rlang 0.3.0.
          # Please use `as_label()` or `as_name()` instead.
          #  this can be traces by rlang:::as.character.quosure
          gto <- dts[[.x]] %>% rlang::as_label() %>% stringr::str_extract_all("gid_[0-9]+") %>% unlist()
          tibble(gid = gto, new_gid = names(dts)[.x])
        })
      
      new_gid_map <- new_gid_map %>% mutate_all(~stringr::str_replace(.x, "gid_","")) %>% unique()
      
      #@Dev need to take dists merge in case intra block dist is there
      
      dthis <- get_group_id_join_gids(old_group_id_info = list(group_id_map = dthis), 
                                      gid_map = new_gid_map, no_group_boundary = TRUE)$group_id_map
      
      
    }else{
      dthis <- mutate(dthis, ...)
    }
    
  }else{
    dthis <- mutate(dthis, ...)
  }
  
  dout <- new_cell_df(dthis, minimal = TRUE)
  
  if(corner_join | join_table_blocks_around > 0){
    xmeta <- dout %>% 
      calc_meta_intra_block_dist_for_tf(fresh = refresh)
    
    if(corner_join){
      if(join_table_blocks_around > 0){
        # 1.5 is appx sqrt(2)+delta for corner cases
        join_table_blocks_around <- max(join_table_blocks_around, 1.5)
      }else{
        join_table_blocks_around <- 1.5
      }
    }
    
    if(join_table_blocks_around>0){
      xmeta_this <- xmeta %>% 
        filter(d <= join_table_blocks_around) %>% 
        mutate(mgid = pmin(gid1, gid2))
      
      new_gid_map <- xmeta_this %>% select(gid = gid1, new_gid = mgid) %>% 
        bind_rows(xmeta_this %>% select(gid = gid2, new_gid = mgid)) %>% unique() 
      
      dthis <- get_group_id_join_gids(old_group_id_info = list(group_id_map = dthis), 
                                      gid_map = new_gid_map, no_group_boundary = TRUE)$group_id_map
      
      dout <- new_cell_df(dthis, minimal = TRUE)
    }
    
  }
  
  dout
  
}


filter.cell_df <- function(.data, ..., keep = 0, keep_table_blocks_around = keep, refresh = FALSE, Row, Col, Relative = FALSE){
  
  x <- .data %>% as_tibble()
  
  dts_this <- rlang::enquos(...)
  dts_this <- dts_this %>% unlist() %>% as.character() %>% c(names(dts_this))
  
  if(dts_this %>% stringr::str_detect("content") %>% any()){
    
    this_meta <- calc_meta_for_tf(.data, fresh = refresh)
    
    x_fused <- x %>% left_join(this_meta, by = "gid")
    
    x_fused_flt <- filter(x_fused, ...)
    
    x_fused_flt_m <- x_fused_flt[colnames(this_meta)] %>% unique()
    x_fused_flt_x <- x_fused_flt[colnames(x)] %>% unique()
    
    xo <- x_fused_flt_x
    attr(xo, "meta") <- x_fused_flt_m
    
    if(nrow(x)>0 & nrow(x_fused_flt_x) == 0){
      
      dts <- dts_this %>% stringr::str_detect("content") %>% dts[.]
      dts <- dts %>% stringr::str_detect("[A-Z]") %>% dts[.]
      chk <- length(dts) > 0 
      if(chk){
        msg_once("Seems like you are filtering 'Table_Field' based on content.",
                 "\nKindly note that content is in lower case.",
                 "\nThe string with which you are comparing should be also in lower case.",
                 "\nCheck once whether that is the case.")
      }
    }
    
  }else{
    xo <- filter(x, ...)
  }
  
  # sallow selection by Row and Col and Relative
  if(!missing(Row) & !missing(Col)){
    if(Relative){
      Row <- Row*max(x$row)
      Col <- Col*max(x$col)
    }
    xo <- xo %>% filter(((row-Row)^2+(col-Col)^2) == min(((row-Row)^2+(col-Col)^2)))
  }else{
    if(!missing(Row)){
      if(Relative){
        Row <- Row*max(x$row)
      }
      xo <- xo %>% filter(abs(row-Row) == min(abs(row-Row)))
    }
    
    if(!missing(Col)){
      if(Relative){
        Col <- Col*max(x$col)
      }
      xo <- xo %>% filter(abs(col-Col) == min(abs(col-Col)))
    }
  }
  
  # surrounding gids
  if(keep_table_blocks_around>0){
    xmeta <- calc_meta_intra_block_dist_for_tf(.data, fresh = refresh)
    
    xmeta_this <- xmeta %>% 
      filter(gid1 %in% xo$gid | gid2 %in% xo$gid) %>% 
      filter(d <= keep_table_blocks_around)
    
    if(nrow(xmeta_this)>0){
      take_gids <- c(xmeta_this$gid1, xmeta_this$gid2, xo$gid) %>% unique()
    }else{
      # case of single gid
      take_gids <- c(xo$gid) %>% unique()
    }
    
    
    xmeta_this <- xmeta %>% 
      filter(gid1 %in% take_gids | gid2 %in% take_gids)
    
    xo <- x %>% filter(gid %in% take_gids)
    
    attr(xo, "meta_intra_block_dist") <- xmeta_this
    
  }
  
  xo %>% new_cell_df(minimal = TRUE)
  
}
