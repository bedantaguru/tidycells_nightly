
shiny_admap_display <- function(admap, cd, d_dat, d_att){
  
  
  library(shiny)
  library(miniUI)
  library(shinyBS)
  library(DT)
  
  
  if(missing(cd)){
    if(!(hasName(admap, "row_d") & hasName(admap, "row_a"))){
      if(missing(d_dat) | missing(d_att)){
        stop("Either admap has to be cell wise or d_dat and d_att has to be specified in order to populate cd", call. = F)
      }
      
      admap <- get_data_attr_cell_wise_map_raw(admap, d_dat, d_att)
      
    }
    cd <- (admap %>% distinct(row = row_d, col = col_d, value = data_gid)) %>% 
      bind_rows(admap %>% distinct(row = row_a, col = col_a, value = attr_gid))
    cd <- cd %>% group_by(row, col) %>% summarise(value = paste0(value, collapse = " + ")) %>% 
      mutate(data_type = "character", type = "attribute") %>% new_cell_df(minimal = T) %>% mutate(gid = "dummy")
  }
  
  if(missing(d_dat)){
    d_dat <- get_group_id(cd %>% filter(type == "value"), gid_tag = "d")
  }
  
  if(missing(d_att)){
    d_att <- get_group_id(cd %>% filter(type == "attribute"), gid_tag = "a")
  }
  
  
  if(!("row_d" %in% colnames(admap)) & !("row_a" %in% colnames(admap))){
    admap <- get_data_attr_cell_wise_map_raw(admap, d_dat, d_att)
  }
  
  if(!("row_d" %in% colnames(admap))){
    admap <- admap %>%
      # join with data_gid to attach all data-cells
      inner_join(d_dat %>%
                   select(row_d = row, col_d = col, data_gid = gid),
                 by = "data_gid"
      ) 
  }
  
  if(!("row_a" %in% colnames(admap))){
    admap <- admap %>%
      # join with data_gid to attach all data-cells
      inner_join(d_att %>%
                   select(row_a = row, col_a = col, attr_gid = gid),
                 by = "attr_gid"
      )
  }
  # cd  cell df
  # admap : cellwise raw
  
  
  
  # this selects all data gid
  if(!hasName(admap,"select_all")){
    admap$select_all <- "dummy"
  }
  
  admap_flat <- (
    admap %>% distinct(row = row_a, col = col_a) %>% mutate(type = "attr")
  ) %>% 
    bind_rows(
      admap %>% distinct(row = row_d, col = col_d) %>% mutate(type = "data")
    ) %>%
    mutate(x = col, y = -row)
  
  
  ui <- miniPage(
    # gadgetTitleBar("Data Attribute Map Inspection"),
    miniContentPanel(
      plotOutput("plot_admap", height = "100%", click = clickOpts("plot_admap_click"))
    ),
    
    ################# 
    # Control Panel #
    #################
    
    absolutePanel(
      draggable = T, 
      top = 200,
      style = "opacity: 0.9;",
      
      bsCollapse(id = "Control_Panel_collapse", open = "Control Panel",
                 bsCollapsePanel(
                   "Control Panel", 
                   div(
                     
                     ####################### 
                     # Control Panel Start #
                     #######################
                     
                     div(style = "float: left; margin: 5px;",
                         sliderInput("row_range", label = "Row Range", 
                                     min = min(cd$row), max = max(cd$row), 
                                     value = range(cd$row), step = 1L),
                         sliderInput("col_range", label = "Col Range", 
                                     min = min(cd$col), max = max(cd$col), 
                                     value = range(cd$col), step = 1L),
                         checkboxInput("no_txt", label = "No text", value = T),
                         checkboxInput("no_legend", label = "No Legend", value = F),
                         checkboxInput("draw_grid", label = "Draw Grid", value = F),
                         conditionalPanel("input.no_txt == false",
                                          sliderInput("txt_size", "Text Size", min = 1, max = 8, value = 4, step = 0.5),
                                          sliderInput("txt_angle", "Text Angle", min = 0, max = 90, value = 0, step = 15)),
                         checkboxInput("zoom", label = "Zoom", value = F)),
                     
                     div(style = "float: left; margin: 5px;",
                         selectizeInput("filter_by", "Filter AD map by:", 
                                        choices = colnames(admap), selected = "data_gid"),
                         selectizeInput("filter_by_value", "Values of Filter Column:", 
                                        choices = unique(admap$data_gid) ),
                         radioButtons("filter_by_rc","Row Col", choices = c("A","D")),
                         actionButton("traverse_filter_by_value", "Traverse", icon = icon("play")),
                         br(),
                         sliderInput("traverse_filter_by_value_delay", label = "Delay", min = 1, max = 10, value = 2, step = 1)),
                     
                     div(style = "float: left; margin: 5px;",
                         rep(list(br()), 5),
                         actionButton("swap_filter_by_cols", "Swap")),
                     
                     div(style = "float: left; margin: 5px;",
                         selectizeInput("filter_by_mapped", "Further Filter AD map by:", 
                                        choices = colnames(admap), selected = "attr_gid"),
                         selectizeInput("filter_by_mapped_value", "Values of Filter Column:", 
                                        choices = unique(admap$attr_gid),  multiple = T ),
                         radioButtons("filter_by_mapped_rc","Row Col", choices = c("A","D")))
                     
                     ##################### 
                     # Control Panel End #
                     #####################
                   ),
                   
                   style = "info"
                 ))
      
      
    )
    
    ################# 
    # Control Panel #
    #################
    , # data panel
    
    ############## 
    # Data Panel #
    ##############
    absolutePanel( 
      top = 50,
      draggable = T, 
      style = "opacity: 0.95;",
      
      bsCollapse(id = "Data_Panel_collapse",
                 bsCollapsePanel(
                   "Data", 
                   div(style = "width: 800px;",
                       ####################
                       # Data Panel Start #
                       ####################
                       
                       selectInput("which_data_to_show", label = "Display:", 
                                   choices = c("Filtered (by both filters)"="f2","Filtered (by 1st filter)"="f1", "Generated Cell-DF"="cdf")),
                       DT::dataTableOutput("data_display")
                       
                       ##################
                       # Data Panel End #
                       ##################
                       
                   ),
                   
                   style = "success"
                 ))
      
      
    ),
    ############## 
    # Data Panel #
    ##############
    
    # no fade during re calc
    tags$style(type="text/css",
               ".recalculating {opacity: 1.0;}"
    )
    
  )
  
  ########## Server ##########
  last_value_for_swap_filter_by_cols <- 0
  server <-  function(input, output, session){
    
    cdf <- reactiveVal(cd)
    admap_filtered <- reactiveVal(admap)
    admap_filtered_by_mapped <- reactiveVal(admap)
    traverse_filter_by_value_now <- reactiveVal(F)
    traverse_filter_by_value_i <- reactiveVal(1)
    
    observeEvent(input$traverse_filter_by_value,{
      play_now <- F
      if(input$traverse_filter_by_value%%2 == 1){
        play_now <- T
        updateActionButton(session, inputId = "traverse_filter_by_value", label = "Pause", icon = icon("pause"))
      }else{
        updateActionButton(session, inputId = "traverse_filter_by_value", label = "Traverse", icon = icon("play"))
      }
      traverse_filter_by_value_now(play_now)
    })
    
    observe({
      if(traverse_filter_by_value_now()){
        invalidateLater(1000*as.numeric(input$traverse_filter_by_value_delay))
        isolate({
          # admap_filtered(admap)
          # admap0 <- admap
          x <- unique(admap[[input$filter_by]]) %>% sort()
          if(traverse_filter_by_value_i()>length(x)){
            traverse_filter_by_value_i(1)
          }
          updateSelectizeInput(
            session, "filter_by_value", 
            label = "Values of Filter Column:", 
            choices = x, 
            selected = x[traverse_filter_by_value_i()])
          traverse_filter_by_value_i(traverse_filter_by_value_i()+1)
        })
      }
    })
    
    
    observe({
      updateRadioButtons(
        session, 
        "filter_by_mapped_rc","Row Col", choices = c("A","D"), 
        selected = ifelse(stringr::str_detect(input$filter_by_mapped,"attr"),"A", "D"))
    })
    
    observe({
      updateRadioButtons(
        session, 
        "filter_by_rc","Row Col", choices = c("A","D"), 
        selected = ifelse(stringr::str_detect(input$filter_by,"attr"),"A", "D"))
    })
    
    observe({
      #isolate(admap0 <- admap_filtered())
      admap0 <- admap_filtered()
      updateSelectizeInput(
        session, "filter_by_mapped_value", 
        label = "Values of Filter Column:", 
        choices = unique(admap0[[input$filter_by_mapped]]) %>% sort(), 
        selected = unique(admap0[[input$filter_by_mapped]]))
    })
    
    observe({
      # isolate(admap0 <- admap_filtered())
      admap_filtered(admap)
      admap0 <- admap
      updateSelectizeInput(
        session, "filter_by_value", 
        label = "Values of Filter Column:", 
        choices = unique(admap0[[input$filter_by]]) %>% sort())
    })
    
    observe({
      input$swap_filter_by_cols
      if(input$swap_filter_by_cols > last_value_for_swap_filter_by_cols){
        isolate(admap0 <- admap_filtered())
        f1 <- input$filter_by
        f2 <- input$filter_by_mapped
        
        updateSelectizeInput(session,
                             "filter_by", "Filter AD map by:", 
                             choices = colnames(admap0) %>% sort(), selected = f2[1])
        
        updateSelectizeInput(session,
                             "filter_by_mapped", "Further Filter AD map by:", 
                             choices = colnames(admap0) %>% sort(), selected = f1)
        last_value_for_swap_filter_by_cols <<- input$swap_filter_by_cols
      }
    })
    
    observe({
      #isolate(admap0 <- admap_filtered())
      admap0 <- admap
      admap0 <- try(admap0[admap0[[input$filter_by]]==input$filter_by_value,], silent = T)
      if(is.data.frame(admap0)){
        if(nrow(admap0)>0){
          admap_filtered(admap0)
        }
      }
    })
    
    observe({
      admap0 <- admap_filtered()
      admap0 <- try(admap0[admap0[[input$filter_by_mapped]] %in% input$filter_by_mapped_value,], silent = T)
      if(is.data.frame(admap0)){
        if(nrow(admap0)>0){
          admap_filtered_by_mapped(admap0)
        }
      }
    })
    
    observe({
      admap0 <- admap_filtered_by_mapped()
      
      d_try <- try({
        
        if(input$filter_by_rc == "A"){
          rccols_d <- c("row_a","col_a")
        }else{
          rccols_d <- c("row_d","col_d")
        }
        
        if(input$filter_by_mapped_rc == "A"){
          rccols_a <- c("row_a","col_a")
        }else{
          rccols_a <- c("row_d","col_d")
        }
        
        ddf <- admap0[c(input$filter_by,rccols_d)] %>% unique()
        adf <- admap0[c(input$filter_by_mapped,rccols_a)] %>% unique()
        
        colnames(ddf) <- c("value","row","col")
        colnames(adf) <- c("value","row","col")
        
        ddf <- ddf %>% group_by(row, col) %>% 
          summarise(value = paste0(value, collapse = " + ")) %>% 
          ungroup()
        adf <- adf %>% group_by(row, col) %>% 
          summarise(value = paste0(value, collapse = " + ")) %>% 
          ungroup()
        
        adf <- adf %>% mutate(gid = value)
        ddf <- ddf %>% mutate(gid = value)
        
        cdf0 <- rbind(ddf, adf)
        
        cdf0 <- cdf0 %>% group_by(row, col) %>% 
          summarise(value = paste0(value, collapse = " + "), gid = min(gid)) %>% 
          ungroup()
        
        cdf0$data_type = "character"
        cdf0 <- new_cell_df(cdf0, minimal = T)
        cdf0
        
      }, silent = T)
      
      if(!inherits(d_try, "try-error")){
        if(nrow(d_try)>0){
          cdf(d_try)
        }else{
          cdf(cd)
        }
      }
    })
    
    observe({
      if(input$zoom){
        cd0 <- cdf()
        updateSliderInput(session, 
                          "row_range", "Row Range", 
                          min = min(cd$row), max = max(cd$row), 
                          value = range(cd0$row))
        updateSliderInput(session, 
                          "col_range", label = "Col Range", 
                          min = min(cd$col), max = max(cd$col), 
                          value = range(cd0$col))
      }else{
        updateSliderInput(session, 
                          "row_range", "Row Range", 
                          min = min(cd$row), max = max(cd$row), 
                          value = range(cd$row))
        updateSliderInput(session, 
                          "col_range", label = "Col Range", 
                          min = min(cd$col), max = max(cd$col), 
                          value = range(cd$col))
      }
    })
    
    # handle click input
    
    observe({
      if(!is.null(input$plot_admap_click)){
        sel_d <- nearPoints(admap_flat,
                            input$plot_admap_click,
                            xvar = "x", yvar = "y", threshold = 80, addDist = T
        )
        if(nrow(sel_d)>0){
          sel_d <- sel_d %>% filter(dist_ == min(dist_))
          sel_d <- sel_d[1, ]
          if(sel_d$type=="data"){
            admap0 <- admap %>% filter(row_d == sel_d$row, col_d == sel_d$col)
          }else{
            admap0 <- admap %>% filter(row_a == sel_d$row, col_a == sel_d$col)
          }
          
          isolate({
            fbv <- admap0[[input$filter_by]][1]
            all_v <- admap[[input$filter_by]] %>% unique()
            updateSelectizeInput(
              session, "filter_by_value", 
              label = "Values of Filter Column:", 
              choices = all_v, 
              selected = fbv)
          })
          
        }
      }
    })
    
    output$plot_admap <- renderPlot({
      cd0 <- cdf()
      if(is.null(cd0$gid)){
        cd0$gid <- "1"
      }
      rr <- input$row_range %>% range()
      cr <- input$col_range %>% range()
      if(any(is.na(rr)) | is.null(rr)){
        rr <- range(cd0$row)
      }
      
      if(any(is.na(cr)) | is.null(cr)){
        cr <- range(cd0$col)
      }
      
      isolate(admapf0 <- admap_filtered())
      admapf0 <- admapf0 %>% distinct(row = row_a, col =col_a) %>% 
        bind_rows(
          admapf0 %>% distinct(row = row_d, col =col_d)
        )
      
      bg_fixed <- plot_cell_df_ggplot2(background = cd %>% 
                                         filter(row <= rr[2], row >= rr[1],
                                                col <= cr[2], col >= cr[1]), 
                                       background_only = T, 
                                       background_color = "black", background_alpha = 0.001,
                                       background_line_type = 2)
      
      bg1 <- plot_cell_df_ggplot2(background = admap_flat %>%  
                                    filter(row <= rr[2], row >= rr[1],
                                           col <= cr[2], col >= cr[1]), background_only = T, 
                                  background_color = "yellow", background_alpha = 0.08)
      
      bg2 <- plot_cell_df_ggplot2(background = admapf0 %>% 
                                    filter(row <= rr[2], row >= rr[1],
                                           col <= cr[2], col >= cr[1]), background_only = T, 
                                  background_color = "green", background_alpha = 0.05)
      
      
      cd0 %>% 
        filter(row <= rr[2], row >= rr[1],
               col <= cr[2], col >= cr[1]) %>% 
        plot_cell_df_ggplot2(no_txt = isTRUE(input$no_txt), fill = "gid", 
                             txt_size = input$txt_size,
                             txt_angle = input$txt_angle,
                             no_legend = isTRUE(input$no_legend),
                             draw_grid = input$draw_grid,
                             background = list(bg_fixed, bg1, bg2), 
                             no_plot = T) -> g
      g
    })
    
    
    output$data_display <- DT::renderDT({
      dt <- tibble()
      if(!is.null(input$which_data_to_show)){
        if(input$which_data_to_show=="f2"){
          dt <- admap_filtered_by_mapped()
        }else{
          if(input$which_data_to_show=="f1"){
            dt<-admap_filtered()
          }else{
            dt <- cdf()
          }
        }
      }
      
      dt <- dt %>% dplyr::mutate_if(is.character, as.factor)
      
      datatable(dt,
                selection = "none",
                escape = FALSE,
                rownames = FALSE,
                style = "bootstrap",
                class = "cell-border stripe",
                filter = list(
                  position = 'top', clear = TRUE
                ),
                extensions = c("KeyTable", "Scroller", "Buttons"),
                options = list(
                  pageLength = 5,
                  keys = TRUE,
                  sDom = '<"top">lrt<"bottom">ipB',
                  deferRender = TRUE,
                  scrollX = TRUE,
                  scrollY = 200,
                  scroller = TRUE,
                  buttons = list(
                    list(
                      extend = "colvis",
                      text = as.character(tags$a("Columns", style = "font-size:70%"))
                    )
                  )
                )
      )
    })
  }
  
  shinyApp(ui, server)
  
}