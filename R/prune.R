
#@Dev
# doc needed
#  this will discard same valued columns

prune <- function(x, ...){
  UseMethod("prune")
}

prune.data.frame <- function(x, ..., threshold_num_distinct = 1){
  cnd <- x %>% map_int(~.x %>% unique %>% length)
  x[intersect(names(x),names(cnd[cnd > threshold_num_distinct]))]
}

prune.list <- prune.data.frame