

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
                         no_fill = FALSE,
                         adaptive_txt_size = TRUE, txt_size = 3, txt_alpha = 1, no_txt = FALSE, no_plot = FALSE, 
                         background) {
  d <- x
  
  if (missing(fill)) {
    if(hasName(d, "type")){
      fill <- "type"
    }else{
      if (hasName(d, "gid")) {
        fill <- "gid"
      } else {
        fill <- "data_type"
      }
    }
    
  }
  
  if (!(fill %in% c("data_type", "type","gid"))) {
    abort("fill should be either of (data_type, type, gid)")
  }
  
  if (!hasName(d, fill)) {
    abort(paste0(fill, " is not present in supplied cell_df"))
  }
  
  
  
  if (adaptive_txt_size) {
    d <- d %>%
      mutate(nc = nchar(value), txt_size_ = (min(nc) + 10^-10) / (nc + 10^-10) * txt_size/2 + txt_size/2) %>%
      select(-nc) %>%
      rename(txt_size = txt_size_)
  }
  
  if (no_fill) {
    g <- ggplot2::ggplot(d, ggplot2::aes(col, -row, label = value))
    
    g <- g +
      ggplot2::geom_tile(color = "#00000046", alpha = 0.1, na.rm = TRUE, width = 1, height = 1)
  } else {
    if (fill == "data_type") {
      g <- ggplot2::ggplot(d, ggplot2::aes(col, -row, fill = data_type, label = value)) +
        ggplot2::labs(fill = "Data Type")
    } else {
      if (fill == "gid"){
        g <- ggplot2::ggplot(d, ggplot2::aes(col, -row, fill = as.factor(gid), label = value)) +
          ggplot2::labs(fill = "Group ID (gid)")
      }else{
        g <- ggplot2::ggplot(d, ggplot2::aes(col, -row, fill = type, label = value)) +
          ggplot2::labs(fill = "Type")
      }
      
    }
    
    if(!missing(background)){
      if(is_cell_df(background)){
        g <- g +
          ggplot2::geom_tile(data = background, mapping = ggplot2::aes(col, -row),
                             color = "#00000036", na.rm = TRUE, width = 1, height = 1, inherit.aes = F, 
                             fill = "black", alpha = 0.1)
      }else{
        abort("background must be a cell_df")
      }
    }
  
    g <- g +
      ggplot2::geom_tile(color = "#00000046", na.rm = TRUE, width = 1, height = 1) 
    
    if(fill!="gid"){
      g <- g +
        # kept for all potential plots
        ggplot2::scale_fill_manual(values = c(
          attribute = "#F8766D", value = "#00BFC4", empty = "#A3A3A3A3",
          character = "#F8766D", numeric = "#00BFC4",
          major_attr = "#F8766D", data = "#00BFC4", minor_attr = "#FACE6EE9"
        ))
    }
  }
  
  
  
  
  g <- g +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  
  if (!no_txt) {
    if (adaptive_txt_size) {
      g <- g + ggplot2::geom_text(ggplot2::aes(size = txt_size), alpha = txt_alpha, na.rm = TRUE) +
        ggplot2::scale_size_continuous(
          range = d$txt_size %>% range(),
          guide = FALSE
        )
    } else {
      g <- g + ggplot2::geom_text(size = txt_size, alpha = txt_alpha, na.rm = TRUE)
    }
  }
  
  if (!no_plot) {
    graphics::plot(g, ...)
  }
  
  return(invisible(g))
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
    st <- c(st, "with_table_block")
  }
  
  if(!is.null(attr(x, "meta"))){
    st <- c(st, "with_meta_for_table_blocks")
  }
  
  
  formalize_state(st)
}


mutate.cell_df <- function(.data, ..., direct = FALSE){
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
        abort("Group id merge specification is not correct. Example of such expression: gid_1 = gid_1 + gid_2")
      }
      
      if(gid_present_in_col){
        msg_once("column(s) starting with 'gid_' will be interpreted differently. (you may opt for 'direct = TRUE')")
      }
      
      new_gid_map <- dts %>% seq_along() %>% 
        map_df(~{
          gto <- dts[[.x]] %>% as.character() %>% stringr::str_extract_all("gid_[0-9]+") %>% unlist()
          tibble(gid = gto, new_gid = names(dts)[.x])
        })
      
      new_gid_map <- new_gid_map %>% mutate_all(~stringr::str_replace(.x, "gid_","")) %>% unique()
      
      dthis <- get_group_id_join_gids(old_group_id_info = list(group_id_map = dthis), 
                                      gid_map = new_gid_map, no_group_boundary = TRUE)$group_id_map
      
      
    }else{
      dthis <- mutate(dthis, ...)
    }
    
  }else{
    dthis <- mutate(dthis, ...)
  }
  new_cell_df(dthis, minimal = TRUE)
}


calc_meta_for_tf <- function(x, fresh = FALSE){
  
  if(!("with_table_block" %in% state(x))){
    x <- detect_table_block(x)
  }
  
  last_meta <- attr(x, "meta")
  
  refresh <- FALSE
  if(is.null(last_meta) | !is.data.frame(last_meta) | fresh | 
     !hasName(last_meta, "gid") | !hasName(last_meta, "content") | !hasName(last_meta, "nrow") | !hasName(last_meta, "ncol") | 
     !hasName(last_meta, "size")
  ){
    refresh <- TRUE
  }
  
  if(is.data.frame(last_meta)){
    if(hasName(last_meta, "gid")){
      if(length(unique(intersect(x$gid, last_meta$gid))) < length(unique(x$gid))){
        refresh <- TRUE
      }
    }
  }
  
  if(refresh){
    xl <- x %>% as_tibble() %>% split(.$gid)
    last_meta <- tibble(id = seq_along(xl), gid = xl %>% map("gid") %>% map(1) %>% unlist())
  }else{
    return(last_meta)
  }
  
  
  xl <- xl[last_meta$gid]
  
  xl <- xl %>% map(~new_cell_df(.x, minimal = TRUE))
  
  last_meta <- last_meta %>% mutate(content = xl %>% map_chr(get_content))
  
  last_meta <- last_meta %>% mutate(nrow = xl %>% map_dbl(~max(.x$row)-min(.x$row)+1))
  
  last_meta <- last_meta %>% mutate(ncol = xl %>% map_dbl(~max(.x$col)-min(.x$col)+1))
  
  last_meta <- last_meta %>% mutate(size = ncol*nrow)
  
  last_meta
  
}


filter.cell_df <- function(.data, ..., refresh = FALSE){
  
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
  
  xo %>% new_cell_df(minimal = TRUE)
  
}
