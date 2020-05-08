

# @Dev
# document

angular_sides_stats <- function(d, tol = .85){
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
    filter(l1*l2!=0)
  
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
             ifelse(angular_side_direction %in% c("N","S"), "NS", "WE"))
  
  # d - as : Angular_sides Stats 
  das <- d %>% 
    filter(type == "attribute") %>% 
    count(angular_side_direction, angular_side_direction_group, name = "nattr")
  
  das <- das %>% 
    group_by(angular_side_direction_group) %>% 
    # af : attr frac
    mutate(af = nattr/sum(nattr)) %>% 
    ungroup()
  
  
  # dg : dir grp
  dasdg <- das %>% 
    group_by(angular_side_direction_group) %>% 
    # attr variation
    summarise(av = angular_sides_stats_fdiff(af),
              best_angular_side = angular_side_direction[which.max(af)])
  
  dasdg <- dasdg %>% 
    mutate(side_ok = av>tol)
  
  list(
    well_sided = dasdg$side_ok %>% all(),
    direction_group_stat = dasdg,
    all_stat = das
  )
  
}

####### helpers
####

angular_sides_stats_fdiff <- function(x){
  if(length(x)<=1) return(1)
  abs(max(x)-min(x))
}