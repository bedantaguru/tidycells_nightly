
#' @export
pull.list <- function(.data, var = 1){
  if(length(var) >1) var <- var[1]
  if(is.character(var)){
    var <- intersect(names(.data), var)
  }
  if(length(var)!=1) var <- 1
  .data[[var]]
}




