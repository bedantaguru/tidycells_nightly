
# @Dev need to create shaft
#  bad as has no limiting option

read_docx_from_officer <- function(docx_file){
  if (!is_available("officer")) {
    abort("'officer' package is required")
  }
  
  doc <- officer::read_docx(docx_file)
  doc_content <- officer::docx_summary(doc)
  
  if("table cell" %in% doc_content$content_type){
    doc_tables <- doc_content %>% 
      filter(content_type == "table cell") %>% 
      split(.$doc_index) %>% 
      map(~.x %>% select(row = row_id, col = cell_id, value = text))
    names(doc_tables) <- seq_along(doc_tables) %>% paste0("Table_", .)
  }else{
    doc_tables <- list()
  }
  
  doc_tables
}