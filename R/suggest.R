



cell_sample <- function(x, n = 1000){
  x %>% filter(row<min(row)+n, col<min(col)+n)
}


inspect_cells <- function(x){
  cs <- cell_sample(x)
  cs <- detect_table_block(cs)
}