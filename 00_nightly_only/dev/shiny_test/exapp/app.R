es <- readRDS('data')
shinyApp(es$ui, es$server)

