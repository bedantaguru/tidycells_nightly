

# @Dev
# document

bind_header <- function(data_cell_df, header_cell_df, direction,
                        header_binder = getOption("tidycells.header_binder")){
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
  
  if(header_binder == "unpivotr"){
    unpivotr::enhead(data_cells = data_cell_df, 
                     header_cells = header_cell_df, 
                     direction = translate_from_tidycells_direction(direction))  
  }else{
    attach_header(dat = data_cell_df, 
                  hdr = header_cell_df, 
                  direction = direction)
  }
  
}


#' get direction names compatible to tidycells
#'
#' @details Kept for compatibility. Used internally by get_direction function.
#' @keywords internal
#' @return directions as used in [`bind_header`][bind_header()] applicant (_like unpivotr package_) with directional grouping
#'
get_valid_direction_names <- function(header_binder = getOption("tidycells.header_binder")) {
  
  if(header_binder == "unpivotr"){
    
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
    
  }else{
    
    # direction order
    # "v" "vl" "vr" "vm"
    # "h" "hu" "hd" "hm"
    # "v" "vl" "vr" "vm"
    # "h" "hu" "hd" "hm"
    
    list(
      N = c("v", "vl", "vr", "vm"),
      W = c("h", "hu", "hd", "hm"),
      S = c("v", "vl", "vr", "vm"),
      E = c("h", "hu", "hd", "hm"),
      NE = "vr",
      NW = "vl",
      SE = "vr",
      SW = "vl"
    )
    
  }
  
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


# main mother functions



#' get optimal direction 
#'
#' @param d_part parts of `data_attr_map_raw`
#' @details Used internally
#' @keywords internal
#' @return a string value denoting direction
#'

get_direction <- function(d_part) {
  directions <- get_valid_direction_names()
  
  if (d_part$direction[1] %in% names(directions)) {
    dirs <- directions[[d_part$direction[1]]]
    
    a1 <- d_part %>%
      distinct(attr_gid, row = row_a, col = col_a)
    
    if(nrow(a1)==1){
      # special case where
      # @Dev need to sort out definition
      dirs <- "direct"
    }
    
    if (length(dirs) > 1) {
      d0 <- d_part %>%
        distinct(data_gid, row = row_d, col = col_d)
      
      d0s <- d0 %>% summarise(mnr = min(row), mxr = max(row), mnc = min(col), mxc = max(col))
      
      d1 <- tibble(row = seq(from = d0s$mnr, to = d0s$mxr, by = 1), col = d0s$mnc) %>%
        bind_rows(
          tibble(col = seq(from = d0s$mnc, to = d0s$mxc, by = 1), row = d0s$mnr)
        ) %>%
        mutate(data_gid = d0$data_gid[1]) %>%
        unique()
      
      dmd <- tibble()
      for (dir in dirs) {
        dm_now <- get_direction_metric(d1, a1, dir)
        dmd <- dmd %>% bind_rows(tibble(dm = dm_now, dir = dir))
        if (dm_now == 1) break()
      }
      dmd$dir[which.max(dmd$dm)]
    } else {
      dirs[1]
    }
  } else {
    abort("direction name not known.\n(have you tampered a cell-analysis?)")
  }
}


# helpers



#' get direction metric
#'
#' @param d1 part of d_part with data_gid
#' @param a1 part of d_part with attr_gid
#' @param direction direction name 
#' should be one of [`get_valid_direction_names`][get_valid_direction_names()]
#'
#' @details Used internally by [`get_direction`][get_direction()] function
#' @keywords internal
#' @return a scaled fraction denoting coverage (1 means full coverage) for the supplied direction.
#'
get_direction_metric <- function(d1, a1, direction) {
  l1 <- try(get_direction_metric_part_raw(d1, a1, direction), silent = TRUE)
  
  if (inherits(l1, "try-error")) l1 <- 0
  if (length(l1) != 1) l1 <- 0
  if (is.na(l1)) l1 <- 0
  
  l1 / nrow(d1)
}

get_direction_metric_part_raw <- function(d1, a1, direction) {
  # suppressWarnings should be removed once unpivotr::enhead chages
  # this is happening as "All elements of `...` must be named." warning in tidyr
  # ref: https://github.com/tidyverse/tidyr/issues/714
  # ref: https://github.com/nacnudus/unpivotr/issues/26
  suppressWarnings({
    d1 %>%
      bind_header(a1, direction) %>%
      filter(!is.na(attr_gid)) %>%
      pull(attr_gid) %>%
      length()
  })
}
