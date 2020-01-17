library(DT)
library(shiny)

dat <- na_replace_this_df(cdd)

rowCallback <- c(
  "function(row, data, displayNum, displayIndex){",
  paste0("  var indices = [",paste0(seq(nrow(cdd))-1, collapse = ", "),"];"),
  "  if(indices.indexOf(displayIndex) > -1){",
  "    $(row).find('td:empty').addClass('notselectable');",
  "  }",
  "}"
)

# shinyApp(
#   ui = fluidPage(
#     DTOutput("table")
#   ),
#   server = function(input, output, session) {    
#     output[["table"]] <- renderDT({
#       dat %>%
#         datatable(options = list(
#           rowCallback = JS(rowCallback), 
#           select = list(style = "multi", selector = "td:not(.notselectable)", items = "cell")
#         ), 
#         extensions = "Select", selection = "none"
#         )
#     }, server = FALSE)
#   }
# )
# 


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

shinyApp(
  ui = fluidPage(
    DTOutput("table")
  ),
  server = function(input, output, session) {    
    output[["table"]] <- renderDT({
      dat %>%
        datatable(
          callback = JS(callback),
          style = "bootstrap",
          options = list(
            rowCallback = JS(rowCallback), 
            select = list(style = "os", selector = "td:not(.notselectable)", items = "cell")
          ), 
          extensions = "Select", selection = "none"
        )
    }, server = T)
    observe({
      cat("\014")
      io <- input[["table_cells_selected_raw"]]
      if(!is.null(io)){
        #print(io)
        #print(matrix(io, ncol = 3, byrow = T))
        m <- matrix(io, ncol = 3, byrow = T)
        colnames(m) <- names(io)[seq(3)]
        m[,1] <- m[,1]+1
        m <- m[,-3]
        print(m)
      }
    })
  }
)
