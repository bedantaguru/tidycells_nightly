

#' Get Directional Orientation for Attributes corresponding to a given data
#' group
#'
#' @description for a data block boundary this provides relative location of each attribute cells. 
#' 
#' @param dp data group boundary information (can be multiple)
#' @param allow_inside enables option for attributes which are inside the data block boundary
#' @param datt attribute data (including attribute group id)
#'
#' @details Used internally by get_data_block_information function
#' @keywords internal
#' @return Directional Orientation for Attributes
#'
get_direction_df <- function(dp, d_att, allow_inside = FALSE){
  out <- NULL
  d <- dp %>% rename(data_gid = gid)
  
  # nn : non normalized
  adnn <- expand_df(d_att, d)
  
  get_direction_df_nn(adnn, allow_inside)
}

get_direction_df_nn <- function(adnn, allow_inside = FALSE) {
  directions <- list()
  
  directions$N <- adnn %>%
    filter(
      row < r_min,
      col >= c_min,
      col <= c_max
    ) %>%
    mutate(dist = (r_min - row)) %>%
    mutate(
      direction = "N",
      direction_group = "NS"
    )
  
  directions$S <- adnn %>%
    filter(
      row > r_max,
      col >= c_min,
      col <= c_max
    ) %>%
    mutate(dist = (row - r_max)) %>%
    mutate(
      direction = "S",
      direction_group = "NS"
    )
  
  directions$W <- adnn %>%
    filter(
      col < c_min,
      row >= r_min,
      row <= r_max
    ) %>%
    mutate(dist = (c_min - col)) %>%
    mutate(
      direction = "W",
      direction_group = "WE"
    )
  
  directions$E <- adnn %>%
    filter(
      col > c_max,
      row >= r_min,
      row <= r_max
    ) %>%
    mutate(dist = (col - c_max)) %>%
    mutate(
      direction = "E",
      direction_group = "WE"
    )
  
  # corner directions
  
  directions$NW <- adnn %>%
    filter(
      row < r_min,
      col < c_min
    ) %>%
    mutate(dist = sqrt((r_min - row)^2 + (c_min - col)^2)) %>%
    mutate(
      direction = "NW",
      direction_group = "corner"
    )
  
  directions$NE <- adnn %>%
    filter(
      row < r_min,
      col > c_max
    ) %>%
    mutate(dist = sqrt((r_min - row)^2 + (c_max - col)^2)) %>%
    mutate(
      direction = "NE",
      direction_group = "corner"
    )
  
  directions$SE <- adnn %>%
    filter(
      row > r_max,
      col > c_max
    ) %>%
    mutate(dist = sqrt((r_max - row)^2 + (c_max - col)^2)) %>%
    mutate(
      direction = "SE",
      direction_group = "corner"
    )
  
  directions$SW <- adnn %>%
    filter(
      row > r_max,
      col < c_min
    ) %>%
    mutate(dist = sqrt((r_max - row)^2 + (c_min - col)^2)) %>%
    mutate(
      direction = "SW",
      direction_group = "corner"
    )
  
  if (allow_inside) {
    directions$INSIDE <- adnn %>%
      filter(
        row >= r_min,
        row <= r_max,
        col >= c_min,
        col <= c_max
      ) %>%
      mutate(dist = 0) %>%
      mutate(
        direction = "INSIDE",
        direction_group = "inside"
      )
  }
  
  
  direction_df <- directions %>% bind_rows()
  
  direction_df <- direction_df %>% select( -r_min, -r_max, -c_min, -c_max)
  
  direction_df
}

