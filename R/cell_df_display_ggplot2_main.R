

plot_cell_df_ggplot2 <- function(x, ...,
                                 fill,
                                 no_fill = FALSE,
                                 adaptive_txt_size = TRUE, txt_size = 3, txt_alpha = 1, no_txt = FALSE, 
                                 no_plot = FALSE,
                                 fill_alpha = 1,
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
      ggplot2::geom_tile(color = "#00000046", na.rm = TRUE, width = 1, height = 1, alpha = fill_alpha) 
    
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