
# These functions may not be required by users 
# In other words these may be required for testing the package
# In future for calibration and self test modules


df_footprint <- function(df, only_content = F){
  if(only_content){
    colnames(df) <- df %>% map_chr(~.x %>% as.character %>% sort %>% digest::digest())
  }
  df <- df[sort(colnames(df))]
  df <- df %>% arrange(!!! rlang::syms(colnames(df)))
  if(only_content){
    df %>% as.matrix() %>% as.character() %>% paste0(collapse = "+")
  }else{
    df %>% as.matrix() %>% as.character() %>% paste0(collapse = "+") %>% paste0("__",paste0(colnames(df), collapse = "_"))
  }
  
}

df_equal <- function(df1, df2, only_content = F){
  identical(df_footprint(df1, only_content = only_content), df_footprint(df2, only_content = only_content))
}

