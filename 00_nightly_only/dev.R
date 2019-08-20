
# after  
# devtools::install_github("tidyverse/tidyr")
require(tidyr)

# not in cran
# desc::desc(package = "tidyr")$get_field("Repository")
"Repository" %in% desc::desc(package = "tidyr")$fields()
"URL" %in% desc::desc(package = "tidyr")$fields()

packageVersion("tidyr")

iris %>% nest(-Species, .key = "attr")
iris %>% nest(-Species, attr = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))
# this is fine
iris %>% nest(attr = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))



# internet is required for this 
Sys.setenv(http_proxy="http://172.30.1.78:3128")
Sys.setenv(HTTPS_PROXY = "http://172.30.1.78:3128")

# same vars
d0 <- structure(
  list(row = 2L, col = 1L, value = "5.1"), 
  row.names = c(NA, -1L), 
  class = c("tbl_df", "tbl", "data.frame"))

a0 <- structure(
  list(row = 1L, col = 5L, attr = "Species"), 
  row.names = c(NA,-1L), 
  class = c("tbl_df", "tbl", "data.frame"))

# remove prior installation for this reprex
# unloadNamespace("unpivotr")
# unloadNamespace("tidyr")
# remove.packages("tidyr", lib="~/R/win-library/3.6")
# remove.packages("unpivotr", lib="~/R/win-library/3.6")

# get CRAN version
devtools::install_cran("tidyr", quiet = T)
devtools::install_cran("unpivotr", quiet = T)

#####################
# test code (start) #
#####################
packageVersion("tidyr")
packageVersion("unpivotr")

unpivotr::enhead(d0, a0, "NNE")

#####################
# test code (end)   #
#####################


# this portion may require to run manually
unloadNamespace("unpivotr")
try(detach("package:unpivotr", unload = TRUE), silent = TRUE)
unloadNamespace("tidyr")
try(detach("package:tidyr", unload = TRUE), silent = TRUE)

# get DEV - tidyr
remove.packages("tidyr", lib="~/R/win-library/3.6")
devtools::install_github("tidyverse/tidyr", quiet = TRUE)


# same vars
d0 <- structure(
  list(row = 2L, col = 1L, value = "5.1"), 
  row.names = c(NA, -1L), 
  class = c("tbl_df", "tbl", "data.frame"))

a0 <- structure(
  list(row = 1L, col = 5L, attr = "Species"), 
  row.names = c(NA,-1L), 
  class = c("tbl_df", "tbl", "data.frame"))

#####################
# test code (start) #
#####################
packageVersion("tidyr")
packageVersion("unpivotr")

# this gives warning
unpivotr::enhead(d0, a0, "NNE")

#####################
# test code (end)   #
#####################

# this portion may require to run manually
unloadNamespace("unpivotr")
try(detach("package:unpivotr", unload = TRUE), silent = TRUE)
unloadNamespace("tidyr")
try(detach("package:tidyr", unload = TRUE), silent = TRUE)

# get DEV - unpivotr
remove.packages("unpivotr", lib="~/R/win-library/3.6")
devtools::install_github("nacnudus/unpivotr", quiet = TRUE)


#####################
# test code (start) #
#####################
packageVersion("tidyr")
packageVersion("unpivotr")

unpivotr::enhead(d0, a0, "NNE")
#####################
# test code (end)   #
#####################

#  undo changes
# unloadNamespace("unpivotr")
# unloadNamespace("tidyr")
# remove.packages("tidyr", lib="~/R/win-library/3.6")
# remove.packages("unpivotr", lib="~/R/win-library/3.6")
# devtools::install_cran("tidyr", quiet = TRUE)
# devtools::install_cran("unpivotr", quiet = TRUE)

###############################################



d_att_map <- dat_boundary %>%
  split(.$gid) %>%
  map_df(~ get_direction_df(.x, datt = att_gid_map, allow_inside = TRUE))

# dev for analyze_cells

plot(d, no_plot = T)+ggplot2::geom_tile(data = d_att$group_id_map %>% filter(gid %in% unmapped_attr_gids[1]) %>% mutate(value =NA, type = "empty"))

admap$raw_map %>% filter(attr_group == "minor") %>% filter(attr_gid == unique(attr_gid)[3]) %>% ai_attach_direction()


#  check dendency badge 
# https://github.com/markvanderloo/stringdist/blob/master/README.md
# [![status](https://tinyverse.netlify.com/badge/stringdist)](https://CRAN.R-project.org/package=stringdist)


d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>% 
  readRDS()
d <- numeric_values_classifier(d)
da <- analyze_cells(d)

dc <- compose_cells(da)

dcl <- dc



# dep stringdist

# q_gram q = 3
# method=jw, p=0.1
# method='osa'
# method='jaccard'
# soundex

# xp <- stringdist::phonetic(x)
# yp <- stringdist::phonetic(y)



