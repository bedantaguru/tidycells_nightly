

# pkg share global vars
tidycells_pkg_env <- new.env()

# used in --> is_available.R
assign("na_pkgs", NULL, envir = tidycells_pkg_env)

# used in --> file_kind_identifier.R
assign("file_kinds", NULL, envir = tidycells_pkg_env)

# used in --> file_type_from_magic_numbers.R
assign("magic_numbers", NULL, envir = tidycells_pkg_env)

# used in --> zzz.R  +  utils-etc.R
assign("temp_files", NULL, envir = tidycells_pkg_env)

# used in --> utils-etc.R
assign("notifications", NULL, envir = tidycells_pkg_env)

# used in --> common_knowledge.R
assign("common_knowledge", NULL, envir = tidycells_pkg_env)

# used in --> zzz.R
assign("options_to_clean", NULL, envir = tidycells_pkg_env)



# global constant variables
# note 
# > ordinary_compass_direction_names %>% matrix(nrow = 4)
# [,1] [,2]
# [1,] "N"  "S" 
# [2,] "NW" "SE"
# [3,] "W"  "E" 
# [4,] "SW" "NE"
# each row holds opposites
ordinary_compass_direction_names <- c("N","NW","W","SW","S","SE","E","NE")
