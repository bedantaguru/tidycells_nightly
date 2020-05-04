

plot_cell_df_ggplot2 <- function(d, 
                                 fill,
                                 no_fill = FALSE,
                                 adaptive_txt_size = TRUE, txt_size = 3, txt_alpha = 1, no_txt = FALSE, txt_angle = 0,
                                 no_plot = FALSE,
                                 fill_alpha = 1,
                                 background = NULL,
                                 shrink = TRUE, 
                                 shrink_length = 20, 
                                 draw_grid = T,
                                 numeric_cols = F, ...) {
  
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
  
  if(shrink){
    d <- d %>% 
      mutate(value = ifelse(
        nchar(value)>shrink_length,
        paste0(substr(value, 1, shrink_length-1), "~"), 
        value)
      )
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
    
    if(!is.null(background)){
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
      g <- g + ggplot2::geom_text(ggplot2::aes(size = txt_size), alpha = txt_alpha, na.rm = TRUE, angle = txt_angle) +
        ggplot2::scale_size_continuous(
          range = d$txt_size %>% range(),
          guide = FALSE
        )
    } else {
      g <- g + ggplot2::geom_text(size = txt_size, alpha = txt_alpha, na.rm = TRUE, angle = txt_angle)
    }
  }
  
  if(draw_grid){
    if(!is.null(background)){
      d_r <- c(d$row, background$row) %>% range()
      d_c <- c(d$col, background$col) %>% range()
    }else{
      d_r <- d$row %>% range()
      d_c <- d$col %>% range()
    }
    
    d_r <- seq(from = d_r[1], to = d_r[2], by = 1)
    d_c <- seq(from = d_c[1], to = d_c[2], by = 1)
    d_rm <- -(c(d_r[1]- 0.5, d_r + .5))
    d_cm <-  (c(d_c[1]- 0.5, d_c + .5))
    
    if(numeric_cols){
      c_AA_names <- as.character(d_c)
    }else{
      c_AA_names <- seq_of_AA(max(d_c))[d_c]
    }
    
    
    drcname <- tibble(txt = c_AA_names, col = d_c, row = d_r[1]-1) %>% 
      bind_rows(
        tibble(txt = as.character(d_r), col = d_c[1]-1, row = d_r)
      )
    
    g <- g + 
      ggplot2::geom_vline(xintercept = d_cm, color = "#42706D40", lwd = 0.5) +
      ggplot2::geom_hline(yintercept = d_rm, color = "#42706D20", lwd = 0.5) +
      ggplot2::geom_text(data = drcname, 
                         mapping = ggplot2::aes(col, -row, label = txt), 
                         color = "#11752EA0", size = 2,
                         inherit.aes = F)
  }
  
  if (!no_plot) {
    graphics::plot(g, ...)
  }
  
  return(invisible(g))
}