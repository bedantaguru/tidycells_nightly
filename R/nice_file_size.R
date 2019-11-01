

nice_file_size <- function(x, digits = 2){
  funits <- c("bytes", "KB", "MB", "GB", "TB")
  mult <- 1024
  ti <- 1
  repeat({
    rem <- x %/% mult^ti
    if(rem > 0){
      ti <- ti +1
    }else{
      break()
    }
    if(ti >= length(funits)) break()
  })
  paste0(round(x / mult^(ti-1), digits), " ", funits[ti])
}



