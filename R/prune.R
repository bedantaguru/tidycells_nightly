
#@Dev
# doc needed
#  this will discard same valued columns

prune <- function(x, ..., .threshold_num_distinct = 1, .retain_as_attr = T, .fixed = NULL){
  UseMethod("prune")
}

prune.data.frame <- function(x, ..., .threshold_num_distinct = 1, .retain_as_attr = T, .fixed = NULL){
  
  .fixed <- nse_to_se_colname_picker(substitute(.fixed))
  
  cnd <- x %>% map_int(~.x %>% unique %>% length)
  
  sel <- unique(c(intersect(names(x),names(cnd[cnd > .threshold_num_distinct])), .fixed))
  
  if(.retain_as_attr){
    rem <- setdiff(names(x), sel)
    attr(x, "pruned") <- x[rem] %>% map_chr(1) %>% c(attr(x, "pruned"),.)
  }
  
  x[sel]
}

prune.list <- prune.data.frame