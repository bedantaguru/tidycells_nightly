


#' Collate Columns Based on Content
#'
#' @param composed_data output of [`compose_cells`][compose_cells()] (preferably not processed)
#' @param combine_threshold a numerical threshold (between 0-1) for content-based collation of columns. (Default 0.9)
#' @param rest_cols number of rest columns (beyond `combine_threshold` joins these many number of columns to keep)
#' @param retain_other_cols whether to keep other intermediate (and possibly not so important) columns. (Default `FALSE`)
#'
#' @return
#' @export
#'
#' @examples
#' # todo 
#' collate_columns()
collate_columns <- function(composed_data, 
                            combine_threshold = 0.9, rest_cols = Inf, retain_other_cols = FALSE){
  
  ok <- FALSE
  
  if(is.data.frame(composed_data)){
    if(all(utils::hasName(composed_data, defcols))){
      ok <- TRUE
      dcl <- composed_data %>% 
        split(.$data_block) %>% 
        map(~{
          .d <- .x
          na_c <- .d %>% map_lgl(~is.na(.x) %>% all)
          .d[!na_c]
        })
    }
  }else{
    # data.frame is a list,  first data.frame check is required
    if(is.list(composed_data)){
      if(all(map_lgl(composed_data, is.data.frame))){
        if(all(map_lgl(composed_data, ~all(utils::hasName(.x, defcols))))){
          ok <- TRUE
          dcl <- composed_data
        }
      }
    }
  }
  
  
  
  if(!ok){
    abort("The argument composed_data has to be output of compose_cells. Given composed_data has no known format.")
  }
  
  out_d <- dcl %>% reduce(reduce_2dfs, 
                          combine_th = combine_threshold, 
                          rest_cols = rest_cols, 
                          retain_other_cols = retain_other_cols)
  
  
  if(!retain_other_cols){
    out_d <- out_d[setdiff(colnames(out_d), c("row", "col", "data_block"))]
  }
  
  out_d
  
}
