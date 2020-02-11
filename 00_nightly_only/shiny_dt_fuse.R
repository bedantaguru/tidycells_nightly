


make_DT_this_df <- function(cdd, cdt, in_shiny = FALSE, shrink = T, shrink_length = 20, info = "info", safeMode = FALSE){
  cdd <- na_replace_this_df(cdd)
  cdt <- style_num_this_df(cdt)
  
  rowCallback <- c(
    "function(row, data, displayNum, displayIndex){",
    paste0("  var indices = [",paste0(seq(nrow(cdd))-1, collapse = ", "),"];"),
    "  if(indices.indexOf(displayIndex) > -1){",
    "    $(row).find('td:empty').addClass('notselectable');",
    "  }",
    "}"
  )
  
  callback <- c(
    "var id = $(table.table().node()).closest('.datatables').attr('id');",
    "table.on('click', 'tbody', function(){",
    "  setTimeout(function(){",
    "    var indexes = table.cells({selected:true}).indexes();",
    "    var indices = Array(indexes.length);",
    "    for(var i = 0; i < indices.length; ++i){",
    "      indices[i] = indexes[i];",
    "    }",
    "    Shiny.setInputValue(id + '_cells_selected_raw', indices);",
    "  }, 0);",
    "});"
  )
  
  fd <- bind_cols(cdd, cdt)
  
  nsht <- asNamespace("htmltools")
  
  this_dt_table_container <- nsht$tags$table(
    nsht$tags$style(type = "text/css", "th.dt_cols_of_whole_table {
  background-color: #cde6fa63; 
  border-left-width: 1px; 
  border-left-style: solid; 
  border-left-color: #ddd; 
  border-right-width: 1px; 
  border-right-style: solid; 
  border-right-color: #ddd; 
  border-bottom-width: 1px; 
  border-bottom-style: solid; 
  border-bottom-color: #ddd;
  border-top-width: 1px; 
  border-top-style: solid; 
  border-top-color: #ddd;
  }
  
  th.dt_rownum_of_whole_table{
  width: 25px;
  }
  
  td.selected{
  background-image: radial-gradient(#98d7f09e, #f59b9b57) !important;
  }
  
  td.active {
  background-image: radial-gradient(#98d7f09e, #f59b9b57) !important;
  }
  
  .dt_info_on_whole_table{
  font-size: 60%;
  width: 25px;
  text-align: center;
  transform: rotate(-45deg);
  }"),
    
    nsht$tags$thead(
      nsht$tags$tr(
        c(list(
          nsht$tags$th(
            nsht$tags$div(
              get_element_separated_list(info, nsht$tags$br()),
              class = "dt_info_on_whole_table"),
            class = "dt_rownum_of_whole_table"), 
          colnames(cdd) %>% map(~nsht$tags$th(.x, class = "dt_cols_of_whole_table")))))))
  
  
  if(shrink){
    cdfs <- list(list(targets = seq(ncol(cdd)+1, ncol(fd)), visible = FALSE),
                 list(
                   targets = seq(1, ncol(cdd)),
                   render = JS(
                     "function(data, type, row, meta) {",
                     paste0("return type === 'display' && data.length > ",shrink_length," ?"),
                     paste0("'<span title=\"' + data + '\">' + data.substr(0, ",shrink_length,") + '...</span>' : data;"),
                     "}")
                 ))
  }else{
    cdfs <- list(list(targets = seq(ncol(cdd)+1, ncol(fd)), visible = FALSE))
  }
  
  if(safeMode){
    dt <- datatable(fd,
                    escape = FALSE,
                    rownames = TRUE,
                    #style = "bootstrap4",
                    style = ifelse(in_shiny, "bootstrap", "default"),
                    container = this_dt_table_container,
                    fillContainer = FALSE,
                    class = "cell-border stripe",
                    extensions = c("KeyTable", "Scroller","FixedHeader"),
                    selection = list(target = 'cell'),
                    #editable = TRUE,
                    options = list(
                      columnDefs = cdfs,
                      pageLength = 10,
                      keys = TRUE,
                      sDom = '<"top">lrt<"bottom">pB',
                      deferRender = TRUE,
                      #scrollX = TRUE,
                      scrollX= 400,
                      scrollY = 500,
                      scroller = TRUE,
                      scrollCollapse = TRUE,
                      ordering = FALSE,
                      fixedColumns = TRUE,
                      fixedHeader = TRUE
                    ))
  }else{
    dt <- datatable(fd,
                    callback = JS(callback),
                    escape = FALSE,
                    rownames = TRUE,
                    #style = "bootstrap4",
                    style = ifelse(in_shiny, "bootstrap", "default"),
                    container = this_dt_table_container,
                    fillContainer = FALSE,
                    class = "cell-border stripe",
                    extensions = c("KeyTable", "Scroller","FixedHeader","Select"),
                    selection = "none",
                    #editable = TRUE,
                    options = list(
                      rowCallback = JS(rowCallback),
                      columnDefs = cdfs,
                      pageLength = 10,
                      keys = TRUE,
                      sDom = '<"top">lrt<"bottom">pB',
                      deferRender = TRUE,
                      #scrollX = TRUE,
                      scrollX= 400,
                      scrollY = 500,
                      scroller = TRUE,
                      scrollCollapse = TRUE,
                      select = list(style = "os", selector = "td:not(.notselectable)", items = "cell"),
                      ordering = FALSE,
                      fixedColumns = TRUE,
                      fixedHeader = TRUE
                    ))
  }
  
  # look for styles 
  # here https://rstudio.github.io/DT/010-style.html
  dt %>%
    formatStyle(
      columns = colnames(cdd),
      valueColumns = colnames(cdt),
      target = "cell",
      backgroundColor = styleEqual(
        levels = c(1,2,3,4),
        values = c("#F8766D","#00BFC4","#FACE6EE9","#A3A3A31F")))
  
}
