##################################################################
##################################################################
##################################################################
# trying ABS example


# as_tibble() %>% t() %>% as_tibble() %>% rename(gid= V1, new_gid= V2) %>% gid_map_link_tune()%>% arrange(new_gid, gid)




c(1,2,2,3,3,4,5,6,6,10,8,10,9,10,7,11,12,11) %>% as.character() %>% matrix(nrow = 2) %>% as_tibble()%>% get_links_df %>% arrange(new_gid, gid)



c(1,2,2,3,3,4,5,6,6,10,8,10,9,10,7,11,12,11) %>% as.character() %>% matrix(nrow = 2) %>% as_tibble() %>% t() %>% as_tibble() %>% rename(gid= V1, new_gid= V2) %>% gid_map_link_tune()%>% arrange(new_gid, gid)


bench::mark(c(1,2,2,3,3,4,5,6,6,10,8,10,9,10,7,11,12,11) %>% as.character() %>% matrix(nrow = 2) %>% as_tibble()%>% get_links_df %>% arrange(new_gid, gid)
            
,            
            
            c(1,2,2,3,3,4,5,6,6,10,8,10,9,10,7,11,12,11) %>% as.character() %>% matrix(nrow = 2) %>% as_tibble() %>% t() %>% as_tibble() %>% rename(gid= V1, new_gid= V2) %>% gid_map_link_tune()%>% arrange(new_gid, gid)
)

# debug plots
quick_plot <- function(dm, d){
  cdf <- dm$group_id_map %>% mutate(value =gid) %>% as_cell_df
  plot(cdf, background = d)
}



tf0 <- read_cells("00_nightly_only/wired_blocks.xlsx")[[1]]
tf0 <- tf0 %>% detect_table_block()
tf0<- tf0 %>% mutate(value= gid)

# tf0 %>% group_by(gid) %>% summarise(lor = min(row), loc = min(col), hir = max(row), hic =max(col))
# 
# 
# get_intra_block_dist <- function(gid1, gid2, cd, quick = T, far_th = 20){
#   cd1 <- cd %>% filter(gid == gid1) %>% select(row, col) %>% as_tibble()
#   cd2 <- cd %>% filter(gid == gid2) %>% select(row, col) %>% as_tibble()
#   
#   if(quick){
#     #mnr <- min(nrow(cd1), nrow(cd2))
#     
#     # mcd1 <- cd1[sample(nrow(cd1),mnr),] %>% as.matrix()
#     # mcd2 <- cd2[sample(nrow(cd2),mnr),] %>% as.matrix()
#     # 
#     # qd <- (mcd1-mcd2)^2 %>% apply(MARGIN = 1, sum) %>% sqrt %>% min
#     
#     mcd1 <- cd1[1,] %>% as.matrix()
#     mcd2 <- cd2[1,] %>% as.matrix()
#     
#     qd <- sqrt(sum((mcd1-mcd2)^2))
#     
#     if(qd>far_th){
#       return(far_th)
#     }else{
#       return(qd)
#     }
#   }
#   
#   # if(!actual){
#   #   # this is not faster option
#   #   cd1 <- expand.grid(row = c(min(cd1$row), max(cd1$row)),
#   #               col = c(min(cd1$col), max(cd1$col)))
#   #   cd2 <- expand.grid(row = c(min(cd2$row), max(cd2$row)),
#   #                      col = c(min(cd2$col), max(cd2$col)))
#   # }
#   
#   cd12 <- cd1 %>% mutate(dummy = 1) %>% full_join(cd2 %>% mutate(dummy = 1), by = "dummy")
#   cd12 <- cd12 %>% mutate(dr = row.x - row.y, dc = col.x - col.y,
#                           d = sqrt(dr^2+dc^2))
#   min(cd12$d)
# }
# 
# intra_block_dist <- function(cd,...){
#   if(state(cd)!="with_table_block"){
#     cd <- detect_table_block(cd)
#   }
#   gids <- cd$gid %>% unique()
#   
#   all_2_gids <- expand.grid(gid1 = gids, gid2 = gids, stringsAsFactors = FALSE)
#   
#   all_2_gids <- all_2_gids %>% filter(gid1!=gid2)
#   
#   all_2_gids_upper <- all_2_gids %>% filter(gid1>gid2)
#   all_2_gids_upper <- all_2_gids_upper %>% dplyr::rowwise() %>% mutate(dist = get_intra_block_dist(gid1, gid2, cd,...)) %>% ungroup()
#   return(all_2_gids_upper)
# }
# 
# intra_block_dist(tf0, quick = F)
# intra_block_dist(tf0)

# 
# expand.grid(1:1000,1:10000)
# data.table::CJ(1:1000,1:10000)
# 
# 
# 
# bench::mark(
#   expand.grid(1:31,1:31),     
#   data.table::CJ(1:31,1:31), check = F
# )


# c_expgrd <- function(n){
#   expand.grid(V1 = seq(n), V2 = seq(n)) %>% filter(V1<V2) %>% arrange(V1, V2) %>% as_tibble()
# }
# 
# c_normal <- function(n){
#   combn(seq(n), 2) %>% t %>%  as.data.frame() %>% arrange(V1, V2) %>% as_tibble()
# }
# 
# c_mat <- function(n){
#   matrix(nrow = n, ncol = n)
# }
# 
# N <- 100
# bench::mark(
#   c_expgrd(N),
#   c_normal(N), 
#   c_mat(N), 
#   check = F
# )
# 


#require(dplyr)

real_intra_block_dist(tf0)
approx_intra_block_dist(tf0)
hybrid_intra_block_dist(tf0)

# d1 %>% inner_join(d2)->d
# 
# 



#  gain is not much also problem
dslow <- tf0 %>% real_intra_block_dist()
df <- tf0 %>% real_intra_block_dist(fast= T)

get_intra_block_dist <- function(gid1, gid2, cd, fast = FALSE){
  if(fast){
    cd1 <- cd %>% filter(gid == gid1) %>% select(row, col) %>% as_tibble()
    cd2 <- cd %>% filter(gid == gid2) %>% select(row, col) %>% as_tibble()
    
    m <- (cd1 %>% dplyr::summarize_all(mean) %>% as.numeric()) + (cd2 %>% dplyr::summarize_all(mean) %>% as.numeric())
    
    m <- m/2
    
    t1 <- cd1 %>% apply(MARGIN = 1, function(x) sum((m-x)^2))
    t2 <- cd2 %>% apply(MARGIN = 1, function(x) sum((m-x)^2))
    
    d <- (cd1[which.min(t1),] %>% as.numeric()) - cd2[which.min(t2),] %>% as.numeric()
    
    return(sqrt(sum(d^2)))
  }
  cd1 <- cd %>% filter(gid == gid1) %>% select(row, col) %>% as_tibble()
  cd2 <- cd %>% filter(gid == gid2) %>% select(row, col) %>% as_tibble()
  
  cd12 <- cd1 %>% mutate(dummy = 1) %>% full_join(cd2 %>% mutate(dummy = 1), by = "dummy")
  cd12 <- cd12 %>% mutate(dr = row.x - row.y, dc = col.x - col.y,
                          d = sqrt(dr^2+dc^2))
  min(cd12$d)
}



require(stringr)




tfc0 <- read_cells("00_nightly_only/ABSSelfExplore/20490do001_2016.xls")

tf0 <- tfc0$Table_1.12


saveRDS(tf0, "00_nightly_only/ABSSelfExplore/best_example.rds", version = 2)


tf0 <- numeric_values_classifier(tf0)




tfc0 <- read_cells("00_nightly_only/tidyABS_extdata/australian-industry.xlsx")

tfc0 <- tfc0 %>% as_tfc()

tfc1 <- tfc0 %>% filter(str_detect(content, "agriculture, forestry and fishing"))

tfc1 <- tfc1 %>% filter(str_detect(content, "key data")) 

tf0 <- tfc1 %>% pull()

tf0 <- tf0 %>% numeric_values_classifier()

tf1 <- cell_sample(tf0, n = 50)

saveRDS(tf1, "00_nightly_only/example_tuning_check_points/ex_tidyABS_extdata_australian_industry_Table_1",version = 2)

saveRDS(tf0, "00_nightly_only/example_tuning_check_points/ex_tidyABS_extdata_australian_industry_Table_4",version = 2)

saveRDS(tf0, "00_nightly_only/example_tuning_check_points/ex_tidyABS_environmental_economic_accounts_Table_6.1",version = 2)

ca0 <- analyse_cells(tf1)

##################################################################
##################################################################
##################################################################
##################################################################
############### DT plots #####################


require(tidycells)
require(DT)
require(dplyr)
require(purrr)

xl <- tidyxl::xlsx_cells("inst/extdata/marks.xlsx")
fmt <-tidyxl::xlsx_formats("inst/extdata/marks.xlsx")

cd <- as_cell_df(xl)

cd <- numeric_values_classifier(cd)

cdd <- as.data.frame(cd)


cdt <- cd %>% mutate(value = type) %>% as.data.frame()







require(shiny)

# 
# 
# std_opts <- list(
#   pageLength = 10,
#   keys = TRUE,
#   sDom = '<"top">lrt<"bottom">pB',
#   deferRender = TRUE,
#   #scrollX = TRUE,
#   scrollX= 400,
#   scrollY = 500,
#   scroller = TRUE,
#   scrollCollapse = TRUE,
#   select = list(style = 'os', items = 'cell'),
#   ordering = FALSE,
#   fixedColumns = TRUE,
#   fixedHeader = TRUE
# )

# this_dt_table_container <- nsht$tags$table(
#   nsht$tags$style(type = "text/css", "th {
#   background-color: #cde6fa63; 
#   border-right-width: 1px; 
#   border-right-style: solid; 
#   border-right-color: black; 
#   border-bottom-width: 1px; 
#   border-bottom-style: solid; 
#   border-bottom-color: black}"),
#   tableHeader(c("",colnames(cdd))))


# nsht$tags$thead(
#   nsht$tags$tr(
#     c(list(nsht$tags$th(class = "dt_info_on_whole_table"), 
#            colnames(cdd) %>% map(nsht$tags$th)))))


#nsht$tags$th()

#colnames(cdd) %>% map(nsht$tags$th)




# fuse



nshtG <- asNamespace("htmltools")

make_DT_this_df(cdd, cdt, info = list(nshtG$tags$a("attribute", style = "color:#F8766D"), 
                                      nshtG$tags$a("value", style = "color:#00BFC4")))


# 1 : F8766D : attribute / character / major_attr
# 2 : 00BFC4 : value / numeric / data
# 3 : FACE6EE9 : minor_attr
# 4 : A3A3A3A3 : empty
# 

# # for row
# cs <- tags$style(HTML("
# table.dataTable tr.selected td,
# table.dataTable td.selected .active {
# background-color: green !important;
# }
# "))
# 
# 
# 
# cs <- tags$style(HTML("
# table.dataTable tr.selected td,
# table.dataTable td.active {
# background-color: green !important;
# }
# "))


cs <- tags$style(HTML("
td.active {
background-color: green !important;
}
"))

# 
# cs <- tags$style(HTML("
# td.active {
# background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjxzdmcKICAgeG1sbnM6ZGM9Imh0dHA6Ly9wdXJsLm9yZy9kYy9lbGVtZW50cy8xLjEvIgogICB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIgogICB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiCiAgIHhtbG5zOnN2Zz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciCiAgIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIKICAgeG1sbnM6c29kaXBvZGk9Imh0dHA6Ly9zb2RpcG9kaS5zb3VyY2Vmb3JnZS5uZXQvRFREL3NvZGlwb2RpLTAuZHRkIgogICB4bWxuczppbmtzY2FwZT0iaHR0cDovL3d3dy5pbmtzY2FwZS5vcmcvbmFtZXNwYWNlcy9pbmtzY2FwZSIKICAgd2lkdGg9IjEwIgogICBoZWlnaHQ9IjEwIgogICB2ZXJzaW9uPSIxLjEiCiAgIGlkPSJzdmc4NDciCiAgIHNvZGlwb2RpOmRvY25hbWU9ImRvd25sb2FkLnN2ZyIKICAgaW5rc2NhcGU6dmVyc2lvbj0iMC45Mi4zICgyNDA1NTQ2LCAyMDE4LTAzLTExKSI+CiAgPG1ldGFkYXRhCiAgICAgaWQ9Im1ldGFkYXRhODUzIj4KICAgIDxyZGY6UkRGPgogICAgICA8Y2M6V29yawogICAgICAgICByZGY6YWJvdXQ9IiI+CiAgICAgICAgPGRjOmZvcm1hdD5pbWFnZS9zdmcreG1sPC9kYzpmb3JtYXQ+CiAgICAgICAgPGRjOnR5cGUKICAgICAgICAgICByZGY6cmVzb3VyY2U9Imh0dHA6Ly9wdXJsLm9yZy9kYy9kY21pdHlwZS9TdGlsbEltYWdlIiAvPgogICAgICA8L2NjOldvcms+CiAgICA8L3JkZjpSREY+CiAgPC9tZXRhZGF0YT4KICA8ZGVmcwogICAgIGlkPSJkZWZzODUxIiAvPgogIDxzb2RpcG9kaTpuYW1lZHZpZXcKICAgICBwYWdlY29sb3I9IiNmZmZmZmYiCiAgICAgYm9yZGVyY29sb3I9IiM2NjY2NjYiCiAgICAgYm9yZGVyb3BhY2l0eT0iMSIKICAgICBvYmplY3R0b2xlcmFuY2U9IjEwIgogICAgIGdyaWR0b2xlcmFuY2U9IjEwIgogICAgIGd1aWRldG9sZXJhbmNlPSIxMCIKICAgICBpbmtzY2FwZTpwYWdlb3BhY2l0eT0iMCIKICAgICBpbmtzY2FwZTpwYWdlc2hhZG93PSIyIgogICAgIGlua3NjYXBlOndpbmRvdy13aWR0aD0iMTkyMCIKICAgICBpbmtzY2FwZTp3aW5kb3ctaGVpZ2h0PSIxMDI3IgogICAgIGlkPSJuYW1lZHZpZXc4NDkiCiAgICAgc2hvd2dyaWQ9ImZhbHNlIgogICAgIGlua3NjYXBlOnpvb209IjQ3LjIiCiAgICAgaW5rc2NhcGU6Y3g9IjYuNzgxNjY3MiIKICAgICBpbmtzY2FwZTpjeT0iNC4xMzI4MjQ5IgogICAgIGlua3NjYXBlOndpbmRvdy14PSItOCIKICAgICBpbmtzY2FwZTp3aW5kb3cteT0iLTgiCiAgICAgaW5rc2NhcGU6d2luZG93LW1heGltaXplZD0iMSIKICAgICBpbmtzY2FwZTpjdXJyZW50LWxheWVyPSJzdmc4NDciIC8+CiAgPHJlY3QKICAgICB3aWR0aD0iMTAiCiAgICAgaGVpZ2h0PSIxMCIKICAgICBmaWxsPSJ3aGl0ZSIKICAgICBpZD0icmVjdDg0MyIKICAgICBzdHlsZT0iZmlsbDojMDA4MDAwIiAvPgogIDxwYXRoCiAgICAgZD0iTS0xLDEgbDIsLTIgICAgICAgICAgICBNMCwxMCBsMTAsLTEwICAgICAgICAgICAgTTksMTEgbDIsLTIiCiAgICAgc3Ryb2tlPSJibGFjayIKICAgICBzdHJva2Utd2lkdGg9IjEiCiAgICAgaWQ9InBhdGg4NDUiIC8+Cjwvc3ZnPgo=')
# background-repeat: repeat;
# background-color: green !important;
# text-color: black; !important;
# }
# "))



runGadget(shinyApp(
  ui = fluidPage(
    DTOutput("dt"),
    verbatimTextOutput('txt1')),
  server = function(input, output, session){
    output$dt <- renderDT({
      make_DT_this_df(cdd, cdt, in_shiny = T, info = c("value","attribute"))
    })
    
    output$txt1 <- renderPrint({
      s <- input$dt_cells_selected
      if (length(s)) {
        print(s)
      }
    })
    
  }
))






# fuse



shinyApp(
  ui = fluidPage(
    DTOutput("table")
  ),
  server = function(input, output, session) {    
    output[["table"]] <- renderDT({
      make_DT_this_df(cdd, cdt, in_shiny = T, info = list(nshtG$tags$a("attribute", style = "color:#F8766D"), 
                                            nshtG$tags$a("value", style = "color:#00BFC4")), safeMode = T)
      
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



#DataTables_Table_0 > tbody > tr:nth-child(3) > td.active

runGadget(shinyApp(
  ui = fluidPage(DTOutput("dt"),
                 verbatimTextOutput('txt1')),
  server = function(input, output, session){
    output$dt <- renderDT({
      datatable(cdd,
                escape = FALSE,
                rownames = TRUE,
                style = "bootstrap",
                fillContainer = FALSE,
                class = "cell-border stripe",
                extensions = c("KeyTable", "Scroller","FixedHeader","Select"),
                selection = list(target = 'cell'),
                #editable = TRUE,
                options = std_opts)
    }, server = F)
    
    output$txt1 <- renderPrint({
      s <- input$dt_cells_selected
      if (length(s)) {
        print(s)
      }
    })
    
  }
))



# cellranger::num_to_letter()


# 
# 
# cos <- tags$script(HTML(
#   "$(document).ready(function() {
#   $('#DataTables_Table_0').DataTable( {
#     select: {
#       style: 'os',
#       items: 'cell'
#     }
#   } );
# } );"
# ))




shinyApp(
  ui = fluidPage(
    
    DT::dataTableOutput('dt1'),
    verbatimTextOutput('txt1')
    
  ),
  server = function(input, output, session) {
    
    output$dt1 = DT::renderDataTable({
      datatable(head(iris), extensions = 'KeyTable', options = list(keys = TRUE),
                selection = list(target = 'cell'))
      
    }, server = T)
    
    # print the selected indices
    output$txt1 = renderPrint({
      s = input$dt1_cells_selected
      if (length(s)) {
        print(s)
      }
    })
    
  }
)





datatable(cdd,
          escape = FALSE,
          rownames = TRUE,
          fillContainer = FALSE,
          class = "cell-border stripe",
          extensions = c("KeyTable", "Scroller","FixedHeader","Select"),
          selection = "none",
          #editable = TRUE,
          options = list(
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
            fixedHeader = TRUE,
            select = list(style = 'os', items = 'cell') 
          ))


# see Select
# https://rstudio.github.io/DT/extensions.html


# datatable(cdd,
#           escape = FALSE,
#           rownames = FALSE,
#           class = "cell-border stripe",
#           extensions = c("KeyTable","FixedHeader","Scroller"),
#           options = list(
#             pageLength = 10,
#             keys = TRUE,
#             sDom = '<"top">lrt<"bottom">pB',
#             deferRender = TRUE,
#             #scrollX = TRUE,
#             #scrollX='400px',
#             scrollY = 400,
#             scroller = TRUE,
#             #scroller = list(rowHeight = 100),
#             scrollCollapse = TRUE,
#             ordering = FALSE,
#             fixedColumns = TRUE,
#             #fixedColumns = list(leftColumns = 1, heightMatch = 'auto'),
#             fixedHeader = TRUE
#           ))






library(DT)
library(shiny)


runGadget(shinyApp(
  ui = fluidPage(DTOutput("dt")),
  server = function(input, output){
    output$dt <- renderDT({
      datatable(head(iris),
                escape = FALSE,
                rownames = TRUE,
                style = "bootstrap",
                fillContainer = FALSE,
                class = "cell-border stripe",
                extensions = c("KeyTable", "Scroller","FixedHeader"),
                selection = list(target = 'cell'))
    })
  }
))




library(shiny)
library(DT)
cs <-   tags$style(HTML("
table.dataTable tr.selected td,
table.dataTable td.selected {
background-color: green !important;
}
"))

shinyApp(
  ui = fluidPage(
    cs,
    fluidRow(DT::dataTableOutput('tbl1')),
    fluidRow(DT::dataTableOutput('tbl2')),
    fluidRow(DT::dataTableOutput('tbl3'))
  ),
  server = function(input, output, session) {
    output$tbl1 = DT::renderDataTable(datatable(head(iris), style = "default"))
    output$tbl2 = DT::renderDataTable(datatable(head(iris), style = "bootstrap"))
    output$tbl3 = DT::renderDataTable(datatable(head(iris), style = "bootstrap4"))
  }
)


#################



require(purrr)
system.time(x <- 1:(26^4) %>% map_chr(cellranger::num_to_letter))

system.time(seq_of_AA(26^4))


# > system.time(x <- 1:(26^4) %>% map_chr(cellranger::num_to_letter))
# user  system elapsed 
# 6.45    0.00    6.45 
# > 
# > system.time(seq_of_AA(26^4))
# user  system elapsed 
# 1.54    0.07    1.61 

# n_to_l <- function(x){
#   
#   nl0 <- function(n) {
#     rems <- NULL
#     while (n > 0) {
#       rem <- ((n - 1)%%26) + 1
#       rems <- c(rem, rems)
#       n <- (n - rem)%/%26
#     }
#     paste(LETTERS[rems], collapse = "")
#   }
#   
# }



#############################
d <-tibble(x= c(1,2), y= c("a", "b"))


attr(d$x[1], "id") <- 1
attr(d$x[2], "id") <- 1

names(d$x) <-c("1","2")



# extrenal cell modification traceback
# not very easy


library(unpivotr)
library(tidyxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

library(unpivotr)
library(tidyxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

cells <-
  xlsx_cells(system.file("extdata/enron.xlsx", package = "unpivotr")) %>%
  dplyr::filter(!is_blank, between(row, 14L, 56L), col <= 20) %>%
  select(row, col, data_type, numeric, character, date)


cells <- as_cell_df(cells)
cells_orig <- cells




f1 <- function(x){
  "hi"
}

f2 <- function(x){
  "hii"
}


f3 <- function(x, y){
  "hi"
}

f4<- function(x, y){
  "hello"
}

identical(args(f1), args(f3))

identical(args(f4), args(f3))


tidy_this <- function(cells, cells_orig){
  
  if(missing(cells_orig)){
    cells_orig <- cells
  }
  
  row_headers <-
    dplyr::filter(cells, between(row, 17, 56), between(col, 2, 4)) %>%
    # mutate(character = ifelse(!is.na(character),
    #                           character,
    #                           format(date, origin="1899-12-30", "%b-%y"))) %>%
    select(row, col, value) %>%
    nest(-row) %>%
    mutate(row_header = map(data,
                            ~ str_trim(paste(.x$value, collapse = " ")))) %>%
    unnest(row_header) %>%
    mutate(col = 2L) %>%
    select(row, row_header)
  
  titles <-
    # dplyr::filter(cells, character == "Fixed Price") %>%
    inner_join(cells, dplyr::filter(cells_orig, value == "Fixed Price") %>% select(row, col), by = c("row", "col")) %>%
    select(row, col) %>%
    mutate(row = row - 1L) %>%
    inner_join(cells, by = c("row", "col"))
  
  cells %>% 
    mutate(character = value) %>% 
    partition(titles) %>%
    pull(cells) %>% 
    purrr::map_dfr(~ .x %>%
                     behead("NNW", "title") %>%
                     behead("NNW", "price") %>%
                     behead("N", "bid_offer")) %>%
    # select(-data_type, -character, -date) %>%
    select(-data_type, -character) %>%
    left_join(row_headers, by = "row") %>% 
    rename(cv = value)
}


# simple case


################################################

d1 <- tidy_this(cells)

dcells <- cells %>% mutate(value = paste0(row, "_", col)) 
d2 <- tidy_this(dcells, cells)

d2 <- d2 %>% mutate(chk_col = paste0(row, "_", col))

vcol <- d2 %>% select(-chk_col) %>% .[1:min(100, nrow(d2)),] %>% map_int(~.x %>% intersect(d2$chk_col) %>% length) %>% which.max() %>% names()

d1.1 <- d1 
colnames(d1.1)[which(colnames(d1.1)==vcol)] <- "value"

d1.1.1 <- d1.1 %>% select(row, col, value)
d1.1.2 <- d1.1 %>% select(-row, -col, -value)
orig_colnames <- colnames(d1.1.2)

cname_map <- tibble(orig = orig_colnames, cname = paste0("major_", seq_along(orig_colnames)), cname_l2 = paste0("major_col_top_", seq_along(orig_colnames), "_1"))

colnames(d1.1.2) <- cname_map$cname

d1.1.2.b <- d1.1.2
colnames(d1.1.2.b) <- cname_map$cname_l2

d1_final <- dplyr::bind_cols(d1.1.1, d1.1.2, d1.1.2.b)


d2.1 <- colnames(d2) %>% setdiff(c(vcol, "chk_col")) %>% d2[.]

d2.2  <- d2.1 %>% select(-row, -col) %>% map(~.x %>% stringr::str_replace_all("[^0-9_]+"," :: ")) %>% as_tibble()

colnames(d2.2) <- paste0("cellAddress_",cname_map$cname_l2)

d2_final <- dplyr::bind_cols(d2.1 %>% select(row, col), d2.2)


dc_this <-  d1_final %>% inner_join(d2_final, by = c("row", "col"))

attr(dc_this, "tidycells.cname_map") <- cname_map

################################################





# extra functionalities



mm <- matrix(c("tl","tr","bl","br"), nrow = 2, byrow = T)



rtab_parts_transforms$top(mm)
rtab_parts_transforms$left(mm)
rtab_parts_transforms$right(mm)
rtab_parts_transforms$bottom(mm)


rtab_parts_transforms$top(mm, cn = T)
rtab_parts_transforms$left(mm, cn = T)
rtab_parts_transforms$right(mm, cn = T)
rtab_parts_transforms$bottom(mm, cn = T)







this <- function(x){
  if(is.null(attr(x, "this"))){
    UseMethod("this")
  }else{
    x
  }
}
this.default <- function(x){
  attr(x, "this") <- "test"
  x
}



getNamespaceExports(asNamespace("tidycells"))







tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))
  
  align <- function(x) if (is.numeric(x)) "c" else "l"
  col_align <- vapply(df, align, character(1))
  
  dfm <- as.matrix(df)
  blnk_row <- rep(" ", ncol(dfm))
  # dfm <- rbind(
  #   rbind(rbind(blnk_row, colnames(dfm)), blnk_row),
  #   rbind(dfm, blnk_row))
  # dfm <- rbind(
  #   rbind(colnames(dfm), blnk_row),
  #   dfm)
  
  fr <- colnames(dfm)
  #fr <- colnames(dfm) %>% stringr::str_trim() %>% paste0("**",.,"**")
  
  # # 
  # stick <- matrix(rep("|", nrow(dfm)), nrow = nrow(dfm))
  # 
  # dfm2 <- stick
  # 
  # for(i in 1:ncol(dfm)){
  #   dfm2 <- cbind(dfm2, 
  #                 cbind(dfm[,i], stick))
  # }
  # 
  # df_final <- as.data.frame(dfm2)
  # 
  
  for(i in 1:ncol(dfm)){
    if(i%%2==0){
      dfm[,i] <- paste0("_", dfm[,i], "_")
    }
  }
  
  dfm2 <- rbind(
    blnk_row,
    fr,
    blnk_row,
    dfm,
    blnk_row)
  
  df_final <- as.data.frame(dfm2)
  
  cols <- lapply(df_final, format, ...)
  # cols[[1]] <- paste0("| ", cols[[1]], " | ")
  # if(length(cols)>1){
  #   for(i in 2:length(cols)){
  #     cols[[i]] <- paste0(cols[[i]], " | ")
  #   }
  # }
  
  cols <- lapply(cols, function(xn){
    xn[1] <- stringr::str_replace_all(xn[1], " ", "_")
    # xn[1] <- stringr::str_replace_all(xn[1], "\\|", " ")
    xn[2] <- xn[2] %>% stringr::str_trim() %>% paste0("**",.,"**")
    xn[3] <- stringr::str_replace_all(xn[3], " ", "_")
    #xn[2] <- stringr::str_replace_all(xn[2], " ", "=")
    xn[length(xn)] <- stringr::str_replace_all(xn[length(xn)], " ", "_")
    #xn[length(xn)] <- stringr::str_replace_all(xn[length(xn)], "\\|", " ")
    xn
  })
  
  
  #atag <- paste0("l",paste0(col_align, "l", collapse = ""))
  atag <- paste0(col_align, collapse = "")
  
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n#'  ")))
  
  str <- ( paste("#' \\tabular{", atag, "}{\n#'  ",
                 contents, "\n#' }\n", sep = "")
  )
  writeClipboard(str)
  cat(str)
}


tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))
  
  align <- function(x) if (is.numeric(x)) "c" else "l"
  col_align <- vapply(df, align, character(1))
  
  dfm <- as.matrix(df)
  
  fr <- colnames(dfm)
  
  for(i in 1:ncol(dfm)){
    if(i%%2==0){
      dfm[,i] <- paste0("_", dfm[,i], "_")
    }
  }
  
  
  dfm2 <- rbind(
    fr,
    dfm
  )
  
  df_final <- as.data.frame(dfm2)
  
  cols <- lapply(df_final, format, ...)
  
  cols <- lapply(cols, function(xn){
    xn[1] <- xn[1] %>% stringr::str_trim() %>% paste0("**",.,"**")
    
    xn
  })
  
  
  #atag <- paste0("l",paste0(col_align, "l", collapse = ""))
  atag <- paste0(col_align, collapse = "")
  
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n#'  ")))
  
  str <- ( paste("#' \\tabular{", atag, "}{\n#'  ",
                 contents, "\n#' }\n", sep = "")
  )
  writeClipboard(str)
  cat(str)
}


# https://github.com/fralonra/isbinary
# # UTF-8 BOM
# 0xef  0xbb  0xbf
# 
# # UTF-32 BOM
# 0x00  0x00  0xfe  0xff
# 
# # UTF-32 LE BOM
# 0xff  0xfe  0x00  0x00
# 
# # GB BOM
# 0x84  0x31  0x95  0x33
# 
# 
# # UTF-16 BE BOM
# 0xfe  0xff
# 
# # UTF-16 LE BOM
# 0xff  0xfe

# encoding another beast 
# https://ericeikrem.com/r-blog/r-dealing-with-file-encodings-using-readrguess_encoding-and-stringistri_enc_detect/

# Compound Binary File format by Microsoft
# in https://www.garykessler.net/library/file_sigs.html


# important ref 
# http://guides.dataverse.org/en/latest/user/tabulardataingest/spss.html

# ref 
# https://github.com/minad/mimemagic
# https://stackoverflow.com/questions/48188346/how-to-distinguish-xlsx-and-docx-files-from-zip-archives
# https://stackoverflow.com/questions/51432256/determine-if-a-file-is-a-zip-file-or-an-xlsx-file
# https://github.com/minad/mimemagic/blob/master/lib/mimemagic/overlay.rb

########## try ppt, pptx, docx alternative , xlsx alternative etc











file_kinds <- dat

usethis::use_data(file_kinds, internal = TRUE)


R.utils::gunzip("00_nightly_only/file_samples/multi_files.zip.gz", 
                file.path(dest, "test"), remove = FALSE, overwrite = TRUE)


# install.packages("collapsibleTree")

require(collapsibleTree)


org <- data.frame(
  Manager = c(
    NA, "Ana", "Ana", "Bill", "Bill", "Bill", "Claudette", "Claudette", "Danny",
    "Fred", "Fred", "Grace", "Larry", "Larry", "Nicholas", "Nicholas"
  ),
  Employee = c(
    "Ana", "Bill", "Larry", "Claudette", "Danny", "Erika", "Fred", "Grace",
    "Henri", "Ida", "Joaquin", "Kate", "Mindy", "Nicholas", "Odette", "Peter"
  ),
  Title = c(
    "President", "VP Operations", "VP Finance", "Director", "Director", "Scientist",
    "Manager", "Manager", "Jr Scientist", "Operator", "Operator", "Associate",
    "Analyst", "Director", "Accountant", "Accountant"
  )
)



x <- explore_it("00_nightly_only/file_samples")


# nice network


collapsibleTree::collapsibleTreeNetwork(xn2)


# fn_id <- not_done[1]; rels <- xn





# extract points
extract_points  <- x %>% filter(is_temp_file)


### 

xn <- x

xn[1,1] <-NA





rn <- dirname(xn[1,2][[1]]) %>% normalizePath(winslash = "/")
rn2 <- xn$path[stringr::str_detect(tolower(xn$path), "temp")][1] %>% dirname() %>% dirname()

xn <- xn %>% select(root, path)
xn2 <- xn %>% 
  mutate(root = root %>% 
           normalizePath(winslash = "/") %>%  
           stringr::str_remove_all(rn) %>% 
           stringr::str_remove_all(rn2), 
         path = path %>% 
           normalizePath(winslash = "/") %>%  
           stringr::str_remove_all(rn) %>% 
           stringr::str_remove_all(rn2))

collapsibleTreeNetwork(xn2)

collapsibleTreeNetwork(org, attribute = "Title")
collapsibleTreeNetwork(org)


as.data.frame(Titanic)->d
collapsibleTree(d,hierarchy = colnames(d) %>% setdiff("Freq"))

d3tree(list(root = df2tree(rootname='Titanic',
                           struct=as.data.frame(Titanic)
),
layout = 'collapse')
)




decompress_common_type_1("00_nightly_only/file_samples/multi_files.zip.gz", dest_fold = "00_nightly_only/test/", fcon_f = gzfile)



cmd <- sprintf('"%s" --headless --version',lobin)

# https://help.libreoffice.org/Common/Starting_the_Software_With_Parameters
LibreOffice_convert <- function(){
  
  lobin <- detect_LibreOffice(return_LibreOffice_path = T)
  
  tdir <- tempfile("extract")
  dir.create(tdir)
  
  srcfile
  
  cmd <- sprintf('"%s" --convert-to pptx:"Impress MS PowerPoint 2007 XML" --headless --outdir "%s" "%s"', 
                 lobin, tdir, srcfile)
  cmd <- sprintf("\"%s\" --convert-to docx:\"MS Word 2007 XML\" --headless --outdir \"%s\" \"%s\"", 
                 lobin, tdir, srcfile)
  
  cmd <- sprintf("\"%s\" --convert-to pdf --headless --outdir \"%s\" \"%s\"", 
                 lobin, tdir, srcfile)
  
  system(cmd)
  
  cmd <- sprintf("\"%s\" --convert-to docx:\"MS Word 2007 XML\" --headless --outdir \"%s\" \"%s\"", 
                 lobin, docx_dir, doc_file)
  
  
}

class(src_file) <- c("file", "file_zip")

attach_class <- function(src_file){
  
  common_file_error(src_file)
  
  if(inherits(src_file, "file")){
    return(src_file)
  }
  
  if(file.exists(src_file)){
    type <- file_type_from_magic_numbers(src_file)
  }
}

decompress <- function(src_file){
  tdir <- tempfile(pattern = "decompress", tmpdir = tempdir(check = TRUE))
  unlink(tdir, recursive = TRUE, force = TRUE)
  dir.create(tdir, showWarnings = FALSE)
  
}





docxtractr::read_docx("00_nightly_only/file_samples/two_tabs.docx") %>% docxtractr::docx_extract_all_tbls(guess_header = F)

doc <- officer::read_docx("00_nightly_only/file_samples/two_tabs.docx")
doc_content <- officer::docx_summary(doc)

#doc_content %>% filter(content_type == "table cell") %>% split(.$doc_index)



example_pptx <- system.file(package = "officer", "doc_examples/example.pptx")
doc <- read_pptx(example_pptx)
content <- pptx_summary(doc)
head(content)







doc_tables %>% map(~as_cell_df(.x) %>% as.data.frame %>% as_tibble)

xl <- officer::read_xlsx("00_nightly_only/file_samples/xlsx.csv")


######## check wand

readBin("00_nightly_only/file_samples/pdf.docx", n = 5, what = "raw") %>% rawToChar()


get_raw <- function(x){
  xtr <- paste0(" ", x) %>% stringr::str_replace_all(" "," 0x") %>% strsplit(" ") %>% unlist() 
  xtr <- xtr[nchar(xtr)>0]
  as.raw(xtr) %>% dput()
}

#PPT
# https://www.filesignatures.net/index.php?search=ppt&mode=EXT

"09 08 10 00 00 06 05 00
FD FF FF FF 10
FD FF FF FF 1F
FD FF FF FF 22
FD FF FF FF 23
FD FF FF FF 28
FD FF FF FF 29
" %>% strsplit("\n") %>% unlist() %>% map(get_raw) ->xx

dput(xx)


#Impress MS PowerPoint 2007 XML

cmd <- sprintf("\"%s\" --convert-to pptx:\"Impress MS PowerPoint 2007 XML\" --headless --outdir \"%s\" \"%s\"", 
               lo_path, docx_dir, doc_file)

cmd <- sprintf("\"%s\" -convert-to docx:\"MS Word 2007 XML\" -headless -outdir \"%s\" \"%s\"", 
               lo_path, docx_dir, doc_file)
system(cmd, show.output.on.console = FALSE)

system(cmd)

cmd <- sprintf("\"%s\" -convert-to pdf -headless -outdir \"%s\" \"%s\"", 
               lo_path, docx_dir, doc_file)




fls <- list.files("00_nightly_only/file_samples/", full.names = T)

dd <- tibble(fn = fls)

#fls %>% map(~detect_and_read_new(.x, omit = "doc"))
for(i in 1: length(fls)){
  print(i)
  detect_and_read_new(fls[i])
}


for(i in 1: length(fls)){
  print(i)
  detect_and_read(fls[i])
}


fls %>% map_lgl(is_txt_file) %>% fls[.]

#dd <- dd %>% mutate(dn_new = fn %>% map(~detect_and_read_new(.x, omit = "doc")))
#dd <- dd %>% mutate(dn_new = fn %>% map(~detect_and_read_new(.x)))

dd <- dd %>% mutate(dn_new = fn %>% map(~detect_and_read_new(.x)), dn_old = fn %>% map(~detect_and_read(.x)))

dd <- dd %>% mutate(ft =  dn_new %>% map_chr("file_type"), fty = dn_new %>% map("type") %>% map_chr(1))

dd <- dd %>% mutate(ftyo = dn_old %>% map("type") %>% map_chr(1))

dd %>% filter(fty!=ftyo)

dd %>% mutate(cchk = dn_new %>% purrr::imap_lgl(~{
  identical(.x$content, dn_old[[.y]]$content)
})) -> dd


dd %>% mutate(type = detect_file_type(fn))

dd <- dd %>% mutate(bts = fn %>% map(~readBin(.x, what = "raw", n = 20))) %>% as.data.frame()

dd <- dd %>% mutate(ext = tools::file_ext(fn))

dd %>% filter(ext=="bz2") %>% pull(bts) %>% .[[1]] %>% .[1:3] %>% dput()


wand::simplemagic_mime_db

wand::get_content_type(system.file("extdata", "messy", "csv.docx", package = "tidycells", mustWork = TRUE))

# env <- environment(wand::get_content_type)
# env2 <- as.environment(as.list(env, all.names=TRUE))

env <- as.environment(as.list(environment(wand::get_content_type), all.names=TRUE))


tf <- tempfile()
dput(wand::get_content_type, tf)
ft <- dget(tf)
unlink(tf)

env <- environment(ft)
env$guess_content_type <- function(...) {"???"}

assign("get_content_type", ft, envir = env)

# env$get_content_type <- dput(env$get_content_type, file = nullfile())
# 
# assign("get_content_type", dput(env$get_content_type, file = nullfile()), envir = env)

env$get_content_type(system.file("extdata", "messy", "csv.docx", package = "tidycells", mustWork = TRUE))



# exploring Rcpp
# https://adv-r.hadley.nz/rcpp.html

library(Rcpp)

cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

# https://github.com/gitbucket/gitbucket/wiki/How-to-Close-Reference-issues-and-pull-request

require(dplyr)
require(purrr)

# cli::cli_sitrep()

# 0.7.6

tidycells::compatibility_check(dplyr::group_split, pkg = "dplyr", old_version = "0.7.6")

# how this solves
remotes::install_version("dplyr", version = "0.7.6")

# compatibility check proto added

x0 <- current_state_of_pkgs()
cc1 <- compatibility_check(cli::cli_sitrep(), pkg = "cli", old_version = "1.0.0")
x1 <- current_state_of_pkgs()

expect_equal(x0, x1)
expect_false(cc1$is_same)

usethis::use_test("compatibility_check")

# debug(compatibility_check)
# compatibility_check(cli::cli_sitrep(), old_version = "1.0.0")
# compatibility_check(~cli::cli_sitrep(), old_version = "1.0.0")
# compatibility_check(~cli::cli_sitrep())
# compatibility_check(~utils::packageVersion("pkgbuild"), pkg = "pkgbuild")
# compatibility_check(~system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE) %>% 
#                       read_cells())


# getNamespaceExports("cli")
# exists("cli_sitrep")
# library(cli)
# exists("cli_sitrep")
# unloadNamespace("cli")
# exists("cli_sitrep")

current_state_of_pkgs <- function(){
  x <- utils::sessionInfo()
  lo <- list()
  lo$base <- sort(x$basePkgs)
  lo$ns_attached <- sort(names(x$otherPkgs))
  lo$ns_loaded <- sort(names(x$loadedOnly))
  lo
}

# getNamespaceUsers
deps_one <- function(pkg){
  ips <- utils::installed.packages() %>% as_tibble()
  ips_dep <- ips %>% select(Package, Depends, Imports) %>% 
    mutate(imps = stringr::str_extract_all(Imports, "[a-zA-Z0-9]+"), deps =  stringr::str_extract_all(Depends, "[a-zA-Z0-9]+"))
  ips_dep_this <-ips_dep %>% 
    filter((imps %>% map_lgl(~pkg %in% .x)) | (deps %>% map_lgl(~pkg %in% .x)))
  
  this_deps <- ips_dep_this$Package %>% unique()
  this_deps %>% map_lgl(isNamespaceLoaded) %>% this_deps[.]
}


#################

dcomp02 <- dcomp00 %>%
  map(~ .x %>%
        # this try should be removed if unpivotr::enhead is internalized
        # or similar behaving fucntions is developed.
        map(~try(stitch_direction(.x, ca$cell_df, trace_it = trace_it_back), silent = TRUE)))

dcomp00 <- dam %>%
  group_by(data_gid) %>%
  group_split() %>%
  map(~ .x %>%
        group_by(attr_gid, direction, attr_gid_split) %>%
        group_split())


fails <- dcomp0 %>% map(~.x %>% map_lgl(~inherits(.x, "try-error")))




stitch_direction(dcomp00[[1]][[4]], ca$cell_df)
stitch_direction(dcomp00[[1]][[2]], ca$cell_df)


#  failure for pcd

pcd <- readRDS("00_nightly_only/pcd")

ca <- analyze_cells(pcd)

# after  
# devtools::install_github("tidyverse/tidyr")
require(tidyr)

# not in cran
# desc::desc(package = "tidyr")$get_field("Repository")
"Repository" %in% desc::desc(package = "tidyr")$fields()
"URL" %in% desc::desc(package = "tidyr")$fields()

packageVersion("tidyr")

iris %>% nest(-Species, .key = "attr")
iris %>% nest(-Species, attr = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))
# this is fine
iris %>% nest(attr = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))


admap2_pass <- admap3$map


admap2_pass %>%
  rename(md = dist) %>% 
  group_by(data_gid, direction_group, attr_group) %>%
  mutate(m_dist = min(md))

admap2_pass <- admap2_pass %>%
  rename(md = dist) %>% 
  group_by(data_gid, direction_group, attr_group) %>%
  mutate(m_dist = min(md)) %>%
  ungroup() %>%
  filter(md == m_dist) %>%
  select(-md) %>%
  rename(dist = m_dist)


# internet is required for this 
Sys.setenv(http_proxy="http://172.30.1.78:3128")
Sys.setenv(HTTPS_PROXY = "http://172.30.1.78:3128")

# same vars
d0 <- structure(
  list(row = 2L, col = 1L, value = "5.1"), 
  row.names = c(NA, -1L), 
  class = c("tbl_df", "tbl", "data.frame"))

a0 <- structure(
  list(row = 1L, col = 5L, attr = "Species"), 
  row.names = c(NA,-1L), 
  class = c("tbl_df", "tbl", "data.frame"))

# remove prior installation for this reprex
# unloadNamespace("unpivotr")
# unloadNamespace("tidyr")
# remove.packages("tidyr", lib="~/R/win-library/3.6")
# remove.packages("unpivotr", lib="~/R/win-library/3.6")

# get CRAN version
devtools::install_cran("tidyr", quiet = TRUE)
devtools::install_cran("unpivotr", quiet = TRUE)

#####################
# test code (start) #
#####################
packageVersion("tidyr")
packageVersion("unpivotr")

unpivotr::enhead(d0, a0, "NNE")

#####################
# test code (end)   #
#####################


# this portion may require to run manually
unloadNamespace("unpivotr")
try(detach("package:unpivotr", unload = TRUE), silent = TRUE)
unloadNamespace("tidyr")
try(detach("package:tidyr", unload = TRUE), silent = TRUE)

# get DEV - tidyr
remove.packages("tidyr", lib="~/R/win-library/3.6")
devtools::install_github("tidyverse/tidyr", quiet = TRUE)


# same vars
d0 <- structure(
  list(row = 2L, col = 1L, value = "5.1"), 
  row.names = c(NA, -1L), 
  class = c("tbl_df", "tbl", "data.frame"))

a0 <- structure(
  list(row = 1L, col = 5L, attr = "Species"), 
  row.names = c(NA,-1L), 
  class = c("tbl_df", "tbl", "data.frame"))

#####################
# test code (start) #
#####################
packageVersion("tidyr")
packageVersion("unpivotr")

# this gives warning
unpivotr::enhead(d0, a0, "NNE")

#####################
# test code (end)   #
#####################

# this portion may require to run manually
unloadNamespace("unpivotr")
try(detach("package:unpivotr", unload = TRUE), silent = TRUE)
unloadNamespace("tidyr")
try(detach("package:tidyr", unload = TRUE), silent = TRUE)

# get DEV - unpivotr
remove.packages("unpivotr", lib="~/R/win-library/3.6")
devtools::install_github("nacnudus/unpivotr", quiet = TRUE)


#####################
# test code (start) #
#####################
packageVersion("tidyr")
packageVersion("unpivotr")

unpivotr::enhead(d0, a0, "NNE")
#####################
# test code (end)   #
#####################

#  undo changes
# unloadNamespace("unpivotr")
# unloadNamespace("tidyr")
# remove.packages("tidyr", lib="~/R/win-library/3.6")
# remove.packages("unpivotr", lib="~/R/win-library/3.6")
# devtools::install_cran("tidyr", quiet = TRUE)
# devtools::install_cran("unpivotr", quiet = TRUE)

###############################################








dam %>%
  group_by(data_gid) %>%
  group_split() ->x

y <-  x[[1]] %>%
  group_by(attr_gid, direction, attr_gid_split) %>%
  group_split()

stitch_direction(y[[1]], ca$cell_df, trace_it = FALSE)

x[[1]] %>%
  group_by(attr_gid, direction, attr_gid_split) %>%
  group_split() %>%
  map(~ try(stitch_direction(.x, ca$cell_df, trace_it = trace_it_back)))

x[[1]] %>%
  group_by(attr_gid, direction, attr_gid_split) %>%
  group_split() %>%
  map(~ stitch_direction(.x, ca$cell_df, trace_it = trace_it_back)) %>%
  reduce(fj_this)



d_att_map <- dat_boundary %>%
  split(.$gid) %>%
  map_df(~ get_direction_df(.x, datt = att_gid_map, allow_inside = TRUE))

# dev for analyze_cells

admap1_major_minor$map %>%
  rename(md = dist) %>% 
  group_by(data_gid, direction_group) %>%
  mutate(m_dist = min(md)) %>%
  ungroup() %>%
  filter(md == m_dist) %>%
  select(-md) %>%
  rename(dist = m_dist)

admap1_try <- admap0$all_map %>% 
  rename(attr_gid = gid, dist = md ) %>%
  filter(direction_group != "corner") %>%
  ai_get_data_attr_map_details(d_dat, d_att)


admap1 <- admap0$map %>%
  filter(direction_group != "corner") %>%
  ai_get_data_attr_map_details(d_dat, d_att)

admap0$map %>% group_by(attr_gid, data_gid) %>% mutate(n=n()) %>% ungroup() %>% filter(n>1)


"15000"

plot(d, no_plot = TRUE)+
  ggplot2::geom_tile(data =  d_att$group_id_map %>% filter(gid == "13001") %>% mutate(value =NA, type = "empty"))


rt <- 3; plot(d, no_plot = TRUE)+
  ggplot2::geom_tile(data =  d_dat$group_id_map %>% filter(gid == d_dat$group_id_boundary$gid[rt]) %>% mutate(value =NA, type = "empty"))


rt <- 3; plot(d, no_plot = TRUE)+
  ggplot2::geom_tile(data = d_att$group_id_map %>%  filter(gid == chk$attr_gid[rt]) %>% mutate(value =NA, type = "empty"))+
  ggplot2::geom_tile(data = admap_main$raw_map %>% distinct(gid = data_gid, row =row_d, col =col_d) %>%  filter(gid == chk$data_gid[rt]) %>% mutate(value =NA, type = "empty"))


plot(dc0$cell_df, no_plot = TRUE)+
  ggplot2::geom_tile(data = dc0$details$attr_details$group_id_map %>%  filter(gid == "130000_12000_N") %>% mutate(value =NA, type = "empty"))+
  ggplot2::geom_tile(data = dc0$details$data_details$group_id_map %>%  filter(gid == "12000") %>% mutate(value =NA, type = "empty"))


rt <- 6; am <- admap2$map %>% filter(data_gid == 12004);plot(d, no_plot = TRUE)+
  ggplot2::geom_tile(data = d_att$group_id_map %>%  filter(gid == am$attr_gid[rt]) %>% mutate(value =NA, type = "empty"))+
  ggplot2::geom_tile(data = d_dat$group_id_map %>%  filter(gid == am$data_gid[rt]) %>% mutate(value =NA, type = "empty"))

plot(d, no_plot = TRUE)+ggplot2::geom_tile(data = d_att$group_id_map %>%  filter(gid == 6001) %>% mutate(value =NA, type = "empty"))

plot(d, no_plot = TRUE)+ggplot2::geom_tile(data = d_att$group_id_map %>% filter(gid %in% unmapped_attr_gids[1]) %>% mutate(value =NA, type = "empty"))

admap$raw_map %>% filter(attr_group == "minor") %>% filter(attr_gid == unique(attr_gid)[3]) %>% ai_attach_direction()


#  check dendency badge 
# https://github.com/markvanderloo/stringdist/blob/master/README.md
# [![status](https://tinyverse.netlify.com/badge/stringdist)](https://CRAN.R-project.org/package=stringdist)


d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>% 
  readRDS()
d <- numeric_values_classifier(d)
da <- analyze_cells(d)

dc <- compose_cells(da)

dcl <- dc



# dep stringdist

# q_gram q = 3
# method=jw, p=0.1
# method='osa'
# method='jaccard'
# soundex

# xp <- stringdist::phonetic(x)
# yp <- stringdist::phonetic(y)



