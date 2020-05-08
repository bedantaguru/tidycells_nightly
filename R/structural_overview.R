

structural_overview <- function(d){
  rs <- rect_enclosure_stats(d)
  ass <- angular_sides_stats(d)
  list(
    well_shaped = rs$well_shaped | ass$well_shaped,
    rect_enclosure_stat = rs,
    angular_sides_stat = ass
  )
}


rect_enclosure_stats <-  function(d){
  dd <- d %>% filter(type=="value")
  d_rect <- get_group_id_boundary(dd %>% mutate(gid = "dummy"))
  d_in_d_rect <- d %>% 
    filter(row <=d_rect$r_max, row>=d_rect$r_min,
           col <=d_rect$c_max, col >= d_rect$c_min )
  d_not_in_d_rect <- d %>% 
    anti_join(d_in_d_rect, by = c("row", "col"))
  
  frac_a_in_d_rect <- mean(d_in_d_rect$type=="attribute")
  
  list(
    well_shaped = (frac_a_in_d_rect<0.01) & (nrow(d_not_in_d_rect)>0)
  )
  
}

# @Dev
# document

angular_sides_stats <- function(d, tol = .80, rem_dist = 0){
  
  achk <- (d %>% filter(type=="attribute") %>% nrow())>0
  if(!achk){
    # exit early
    return(list(
      well_sided = F,
      direction_group_stat = NULL,
      all_stat = NULL
    ))
  }
  dcorn <- list(r_max = max(d$row), r_min = min(d$row),
                c_max = max(d$col), c_min = min(d$col))
  
  l1 <- function(r, c){
    (r-dcorn$r_min)*(dcorn$c_max-dcorn$c_min)-(dcorn$r_max-dcorn$r_min)*(c-dcorn$c_min)
  }
  
  l2 <- function(r, c){
    (r-dcorn$r_min)*(dcorn$c_min-dcorn$c_max)-(dcorn$r_max-dcorn$r_min)*(c-dcorn$c_max)
  }
  
  d <- d %>% 
    mutate(l1 = l1(row, col),
           l2 = l2(row, col)) %>% 
    filter(abs(l1)>rem_dist^2, abs(l2)>rem_dist^2)
  
  d <- d %>% 
    mutate(
      angular_side_direction = ifelse(
        l1>0 & l2>0,
        "W", 
        ifelse(
          l1>0 & l2<0,
          "S",
          ifelse(
            l1<0 & l2>0,
            "N",
            ifelse(
              l1<0 & l2<0,
              "E", 
              NA_character_
            )
          )
        )
      )
    )
  
  # check it out 
  # d %>% mutate(gid = angular_side_direction) %>% plot(fill = "gid")
  d <- d %>% 
    mutate(angular_side_direction_group = 
             ifelse(
               angular_side_direction %in% c("N","S"), "NS", "WE"
             ),
           angular_side_dim = 
             ifelse(
               angular_side_direction_group == "NS", col, row
             ))
  
  # d - as : Angular_sides Stats 
  das <- d %>% 
    filter(type == "attribute") %>% 
    group_by(angular_side_direction, angular_side_direction_group) %>% 
    summarise(nattr = n(), dimattr = angular_sides_stats_fdiff(angular_side_dim))
  
  das_d <- d %>% 
    filter(type == "value") %>% 
    group_by(angular_side_direction, angular_side_direction_group) %>% 
    summarise(ndata = n(), dimdata = angular_sides_stats_fdiff(angular_side_dim))
  
  das <- das %>% 
    left_join(das_d, by = c("angular_side_direction", "angular_side_direction_group")) %>% 
    mutate(ndata = ifelse(is.na(ndata), nattr, ndata),
           dimdata = ifelse(is.na(dimdata), dimattr, dimdata))
  
  das <- das %>% 
    group_by(angular_side_direction_group) %>% 
    # af : attr frac
    # ardim : attr relative (to data) dimention (approx)
    mutate(af = nattr/sum(nattr),
           ardim = dimattr/dimdata) %>% 
    ungroup()
  
  
  # dg : dir grp
  dasdg <- das %>% 
    group_by(angular_side_direction_group) %>% 
    # attr variation
    summarise(av = angular_sides_stats_fdiff(af),
              best_angular_side = angular_side_direction[which.max(af)],
              ardim = ardim[which.max(af)])
  
  dasdg <- dasdg %>% 
    mutate(side_ok = av>tol)
  
  ws <- dasdg$side_ok %>% all()
  dim_ok <- (dasdg$ardim > 0.7) %>% all()
    
  list(
    well_sided = ws,
    dimention_ok = dim_ok,
    well_shaped = ws & (nrow(dasdg)==2) & dim_ok,
    direction_group_stat = dasdg,
    all_stat = das
  )
  
}

####### helpers
####

angular_sides_stats_fdiff <- function(x){
  if(length(x)<=1) return(1)
  (max(x)-min(x))
}