

# as of now unpivotr::enhead is used 
# however the framework need to function for other enhead-like functions
# this is the scoping for potential unlinking with {unpivotr}
# possible names : enhead_mount bind_header

bind_header <- function(data_cell_df, header_cell_df, direction){
  #@Dev
  # it will create lot problem with direction name. but it will work as expected.
  if(nrow(header_cell_df)==1){
    # special case
    colnames(header_cell_df) %>% 
      setdiff(c("row","col")) %>% 
      map(~{data_cell_df[[.x]] <<- header_cell_df[[.x]]})
    
    # exit early
    return(data_cell_df)
    
  }
  # as of now unpivotr is in use
  unpivotr::enhead(data_cells = data_cell_df, 
                   header_cells = header_cell_df, 
                   direction = translate_from_tidycells_direction(direction))
}


#' get direction names compatible to tidycells
#'
#' @details Kept for compatibility. Used internally by get_direction function.
#' @keywords internal
#' @return directions as used in [`bind_header`][bind_header()] applicant (_like unpivotr package_) with directional grouping
#'
get_valid_direction_names <- function() {
  
  # direction order
  # "N" "NNW" "NNE" "ABOVE"
  # "W" "WNW" "WSW" "LEFT"
  # "S" "SSW" "SSE" "BELOW"
  # "E" "ENE" "ESE" "RIGHT"
  
  list(
    N = c("N", "NNW", "NNE", "ABOVE"),
    W = c("W", "WNW", "WSW", "LEFT"),
    S = c("S", "SSW", "SSE", "BELOW"),
    E = c("E", "ENE", "ESE", "RIGHT"),
    NE = "NNE",
    NW = "NNW",
    SE = "SSE",
    SW = "SSW"
  )
}

# this function is kept in case 
# https://github.com/nacnudus/unpivotr/issues/23 gets to CRAN and 
# https://github.com/r-rudra/tidycells/issues/19 yet to be implemented
# as of now new naming convention is not followed. 
# check https://github.com/nacnudus/unpivotr/blob/9ac06f34272689da2d99242e4c8951f37496c579/R/direction.R
# full name should be translate_from_tidycells_direction_to_unpivotr
# this function will not be required if the feature is internalized. 
translate_from_tidycells_direction <- function(tidycells_direction_name){
  # as of now https://github.com/nacnudus/unpivotr/issues/23 not in CRAN so
  # direction nomenclature used in tidycells and unpivotr are same.
  tidycells_direction_name
}
