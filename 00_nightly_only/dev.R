
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



