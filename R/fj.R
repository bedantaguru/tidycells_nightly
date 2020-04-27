

# May need to delete
# check it or may need to revamp
# @Dev need to del
# add doc

# may need to rework
fj <- function(x, y,
               join_by, 
               ensure_unique = F) {
  c1 <- colnames(x)
  c2 <- colnames(y)
  comm_cols <- c1 %>%
    intersect(c2) %>%
    setdiff(join_by)
 
  f0 <- full_join(x, y, by = join_by, suffix = c(".fj1",".fj2"))
  if(length(comm_cols)>0){
    for(cn in comm_cols){
      fj1 <- f0[[paste0(cn, ".fj1")]]
      fj2 <- f0[[paste0(cn, ".fj2")]]
      f0[[cn]] <- ifelse(is.na(fj1), fj2, fj1)
      f0[[paste0(cn, ".fj1")]] <- NULL
      f0[[paste0(cn, ".fj2")]] <- NULL
    }
  }
  if(ensure_unique){
    f0<- f0 %>% 
      group_by(!!!rlang::syms(join_by)) %>% 
      dplyr::summarise_all(~.x[1]) %>% 
      ungroup()
  }
  f0
}
