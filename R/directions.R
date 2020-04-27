

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
      purrr::walk(~{data_cell_df[[.x]] <<- header_cell_df[[.x]]})
    
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


#' get header_orientation_tag names compatible to tidycells
#'
#' @details Kept for compatibility. Used internally by get_header_orientation_tag function.
#' @keywords internal
#' @return directions as used in [`bind_header`][bind_header()] applicant (_like unpivotr package_) with directional grouping
#'
get_valid_header_orientation_tags <- function(header_binder = getOption("tidycells.header_binder")) {
  
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



#' get optimal header_orientation_tag (HOT) 
#'
#' @param d_part parts of `data_attr_map_raw`
#' @details Used internally
#' @keywords internal
#' @return a string value denoting header_orientation_tag
#'

get_header_orientation_tag <- function(admap_cellwise_raw_asp_with_dim) {
  directions <- get_valid_header_orientation_tags()
  
  out_full_dim <- admap_cellwise_raw_asp_with_dim %>% 
    filter(attr_micro_gid_is_full_dim, direction_group %in% c("NS", "WE"))
  
  out_rest <- admap_cellwise_raw_asp_with_dim %>% 
    anti_join(out_full_dim, by = c("data_gid","attr_micro_gid"))
  
  directions_f4 <- directions[c("N","W","S","E")] %>% map_chr(1)
  
  out_full_dim$header_orientation_tag <- directions_f4[out_full_dim$direction]
  
  # push first case to most undesirable case
  directions_mod <- directions %>% map(~{
    if(length(.x)>1){
      c(.x[-1], .x[1])
    }else{
      .x
    }
  })
  
  d_reps <- data_gid_representative(
    admap_cellwise_raw_asp_with_dim %>% 
      filter(data_gid %in% unique(out_rest$data_gid)) %>% 
      distinct(data_gid, row = row_d, col = col_d)
  ) %>% split(.$data_gid)
  # it is not actualy representative, it is actual
  a_reps <- out_rest %>% distinct(attr_micro_gid, row = row_a, col = col_a) %>% 
    split(.$attr_micro_gid)
  
  out_rest <- out_rest %>% 
    group_by(data_gid, attr_micro_gid) %>%
    group_split() %>%
    map_df(~{
      .x %>% 
        mutate(
          header_orientation_tag = get_header_orientation_tag_part(
            .x, d_reps, a_reps, directions_mod
          )
        )
    })
  
  out <- out_full_dim %>% 
    bind_rows(out_rest)
  
  out
  
}


# helpers

get_header_orientation_tag_part <- function(d_part, d_reps, a_reps, directions) {
  
  if (d_part$direction[1] %in% names(directions)) {
    dirs <- directions[[d_part$direction[1]]]
    
    a1 <- a_reps[[d_part$attr_micro_gid[1]]]
    
    if(nrow(a1)==1){
      # special case 
      dirs <- "direct"
    }
    
    if (length(dirs) > 1) {
      
      d1 <- d_reps[[d_part$data_gid[1]]]
      
      dmd <- tibble()
      
      for (dir in dirs) {
        dm_now <- get_HOT_metric(d1, a1, dir)
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

data_gid_representative <- function(d_dat){
  
  dout <- d_dat %>% 
    group_by(row, data_gid) %>% 
    summarise(col = min(col)) %>% 
    bind_rows(
      d_dat %>% 
        group_by(col, data_gid) %>% 
        summarise(row = min(row))
    ) %>% ungroup()
  
  dout
}

#' get HOT (header_orientation_tag) metric
#'
#' @param d1 part of d_part with data_gid
#' @param a1 part of d_part with attr_gid
#' @param direction direction name 
#' should be one of [`get_valid_header_orientation_tags`][get_valid_header_orientation_tags()]
#'
#' @details Used internally by [`get_header_orientation_tag`][get_header_orientation_tag()] function
#' @keywords internal
#' @return a scaled fraction denoting coverage (1 means full coverage) for the supplied direction.
#'
get_HOT_metric <- function(d1, a1, direction) {
  l1 <- try(get_HOT_metric_part_raw(d1, a1, direction), silent = TRUE)
  
  if (inherits(l1, "try-error")) l1 <- 0
  if (length(l1) != 1) l1 <- 0
  if (is.na(l1)) l1 <- 0
  
  l1 / nrow(d1)
}

get_HOT_metric_part_raw <- function(d1, a1, direction) {
  # suppressWarnings should be removed once unpivotr::enhead chages
  # this is happening as "All elements of `...` must be named." warning in tidyr
  # ref: https://github.com/tidyverse/tidyr/issues/714
  # ref: https://github.com/nacnudus/unpivotr/issues/26
  suppressWarnings({
    d1 %>%
      bind_header(a1, direction) %>%
      filter(!is.na(attr_micro_gid)) %>%
      pull(attr_micro_gid) %>%
      length()
  })
}
