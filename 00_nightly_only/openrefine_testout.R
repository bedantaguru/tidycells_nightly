

# need to add section further reading 
# https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth

# https://cran.r-project.org/web/packages/rrefine/vignettes/rrefine-vignette.html
# https://github.com/ChrisMuir/refinr


library(dplyr)

x <- c(
  "Clemsson University", 
  "university-of-clemson", 
  "CLEMSON", 
  "Clem son, U.", 
  "college, clemson u", 
  "M.I.T.", 
  "Technology, Massachusetts' Institute of", 
  "Massachusetts Inst of Technology", 
  "UNIVERSITY:  mit"
)



ignores <- c("university", "college", "u", "of", "institute", "inst")

x_refin <- x %>% 
  refinr::key_collision_merge(ignore_strings = ignores) %>% 
  refinr::n_gram_merge(ignore_strings = ignores)

# Create df for comparing the original values to the edited values.
# This is especially useful for larger input vectors.
inspect_results <- tibble(original_values = x, edited_values = x_refin) %>% 
  mutate(equal = original_values == edited_values)

inspect_results[!inspect_results$equal, c("original_values", "edited_values")]



######### 
library(rrefine)
#  not much helpful rrefine
#  or I could not use it.

tmp_file <- tempfile(fileext = ".csv")

write.csv(lateformeeting, file = tmp_file, row.names = FALSE)


refine_upload(file = tmp_file, project.name = "lfm_cleanup", open.browser = TRUE)
