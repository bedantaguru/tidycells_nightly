
shiny_admap_display <- function(cd, admap, d_dat, d_att){
  
  
  require(shiny)
  require(miniUI)
  
  if(!("row_d" %in% colnames(admap))){
    admap <- get_data_attr_cell_wise_map_raw(admap, d_dat, d_att)
  }
  # cd  cell df
  # admap : cellwise raw
  
  ui <- miniPage(
    gadgetTitleBar("Data Attribute Map Inspection"),
    miniTabstripPanel(
      miniTabPanel("A-D Map",
                   icon = icon("cubes"),
                   miniContentPanel(
                     plotOutput("plot_admap", height = "100%")
                   ),
                   
                   miniButtonBlock(
                     div(sliderInput("row_range", label = "Row Range", 
                                     min = min(cd$row), max = max(cd$row), 
                                     value = range(cd$row), step = 1L),
                         sliderInput("col_range", label = "Col Range", 
                                     min = min(cd$col), max = max(cd$col), 
                                     value = range(cd$col), step = 1L),
                         checkboxInput("no_txt", label = "No text", value = F),
                         sliderInput("txt_size", "Text Size", min = 1, max = 5, value = 2, step = 0.5),
                         checkboxInput("zoom", label = "Zoom", value = T)),
                     
                     div(selectizeInput("filter_by", "Filter AD map by:", 
                                        choices = colnames(admap), selected = "data_gid"),
                         selectizeInput("filter_by_value", "Values of Filter Column:", 
                                        choices = unique(admap$data_gid) ),
                         radioButtons("filter_by_rc","Row Col", choices = c("A","D"))),
                     
                     div(actionButton("swap_filter_by_cols", "Swap")),
                     
                     div(selectizeInput("filter_by_mapped", "Further Filter AD map by:", 
                                        choices = colnames(admap), selected = "attr_gid"),
                         selectizeInput("filter_by_mapped_value", "Values of Filter Column:", 
                                        choices = unique(admap$attr_gid),  multiple = T ),
                         radioButtons("filter_by_mapped_rc","Row Col", choices = c("A","D")))
                     
                   )
      ),
      id = "now_tab_main"
    )
  )
  
  ########## Server ##########
  last_value_for_swap_filter_by_cols <- 0
  server <-  function(input, output, session){
    
    cdf <- reactiveVal(cd)
    admap_filtered <- reactiveVal(admap)
    admap_filtered_by_mapped <- reactiveVal(admap)
    
    
    
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
        choices = unique(admap0[[input$filter_by_mapped]]), 
        selected = unique(admap0[[input$filter_by_mapped]]))
    })
    
    observe({
      # isolate(admap0 <- admap_filtered())
      admap_filtered(admap)
      admap0 <- admap
      updateSelectizeInput(
        session, "filter_by_value", 
        label = "Values of Filter Column:", 
        choices = unique(admap0[[input$filter_by]]))
    })
    
    observe({
      input$swap_filter_by_cols
      if(input$swap_filter_by_cols > last_value_for_swap_filter_by_cols){
        isolate(admap0 <- admap_filtered())
        f1 <- input$filter_by
        f2 <- input$filter_by_mapped
        
        updateSelectizeInput(session,
                             "filter_by", "Filter AD map by:", 
                             choices = colnames(admap0), selected = f2)
        
        updateSelectizeInput(session,
                             "filter_by_mapped", "Further Filter AD map by:", 
                             choices = colnames(admap0), selected = f1)
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
        isolate({
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
          cdf0$data_type = "character"
          cdf0 <- new_cell_df(cdf0, minimal = T)
          cdf0
        })
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
      
      cd0 %>% 
        filter(row <= rr[2], row >= rr[1],
               col <= cr[2], col >= cr[1]) %>% 
        plot_cell_df_ggplot2(no_txt = isTRUE(input$no_txt), fill = "gid", 
                             txt_size = input$txt_size,
                             background = cd %>% 
                               filter(row <= rr[2], row >= rr[1],
                                      col <= cr[2], col >= cr[1]))
    })
  }
  
  shinyApp(ui, server)
  
}