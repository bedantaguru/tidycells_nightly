
# @Dev need to create shaft
#  bad as has no limiting option
# dep increased put in deps officer

read_pptx_from_officer <- function(pptx_file){
  if (!is_available("officer")) {
    abort("'officer' package is required")
  }
  
  ppt <- officer::read_pptx(pptx_file)
  ppt_content <- officer::pptx_summary(ppt)
  
  if("table cell" %in% ppt_content$content_type){
    ppt_tables <- ppt_content %>% filter(content_type == "table cell") %>% 
      split(.$id) %>% 
      map(~.x %>% select(row = row_id, col = cell_id, value = text))
    names(ppt_tables) <- seq_along(ppt_tables) %>% paste0("Table_", .)
  }else{
    ppt_tables <- list()
  }
  
  ppt_tables
}