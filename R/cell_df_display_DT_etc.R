

# attribute = "#F8766D", value = "#00BFC4", empty = "#A3A3A3A3",
# character = "#F8766D", numeric = "#00BFC4",
# major_attr = "#F8766D", data = "#00BFC4", minor_attr = "#FACE6EE9"

# 1 : F8766D : attribute / character / major_attr
# 2 : 00BFC4 : value / numeric / data
# 3 : FACE6EE9 : minor_attr
# 4 : A3A3A3A3 : empty
# 

na_replace_this_df <- function(.data){
  .data %>% 
    dplyr::mutate_all(~ifelse(is.na(.x), "", .x))
  
}

style_num_this_df <- function(.data){
  .data %>% 
    dplyr::mutate_all(~ifelse(is.na(.x), 4, .x) %>% 
                 recode(attribute = 1,
                        character = 1,
                        major_attr = 1,
                        value = 2,
                        numeric = 2, 
                        data = 2,
                        minor_attr = 3,
                        empty = 4, 
                        .default = 4))
  
}



# can be implemented in C++
seq_of_AA <- function(n){
  
  digis <- ceiling(log(n)/log(26))
  
  m <- matrix(0, nrow = n, ncol = digis)
  
  rem <- 0
  
  if(ncol(m) > 0){
    for(i in 0:(ncol(m)-1)){
      div_n <- (seq(n)-rem)/(26^i) 
      rem_n <- (div_n - 1) %% 26 +1
      rem_n <- rem_n * ifelse(seq(n) >= sum(26^seq(0, i)), 1, 0)
      rem <- rem + (26^i)*rem_n
      m[,ncol(m)-i] <- rem_n
    }
  }
  
  apply(m, 1, function(x) paste0(LETTERS[x], collapse = ""))
  
}

# CDD : Cell Df as.Data.frame
# CDT : Cell Tag as.Data.frame (CTD written as CDT)
# last one is cell_group_type (yet to implement)

get_cdd_and_cdt <- function(cd, tag_name = c("data_type","type")){
  
  tag_name <- match.arg(tag_name)
  
  cdd <- as.data.frame(cd)
  
  cdt <- cd
  cdt$value <- cdt[[tag_name]]
  cdt <- as.data.frame(cdt)
  
  colnames(cdd) <- seq_of_AA(ncol(cdd))
  
  colnames(cdt) <- paste0("style_", colnames(cdd))
  
  list(cdd = cdd, cdt = cdt)
  
}


get_element_separated_list <- function(x, sep){
  if(inherits(x, "shiny.tag")){
    n <- 1
  }else{
    n <- length(x)
  }
  
  if(n>1){
    nsep <- n-1
    out <- seq(nsep+n) %>% map(~{
      if(.x%%2==0){
        sep
      }else{
        x[(.x+1)/2]
      }
    })
  }else{
    out <- list(x)
  }
  
  out
  
}


