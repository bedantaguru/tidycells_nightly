

#  it has to be added as test

options(tidycells.plot_mode = "ggplot2")

tf0 <- readRDS("00_nightly_only/ABSSelfExplore/best_example.rds")

tf0 <- numeric_values_classifier(tf0)


ca0 <- analyze_cells(tf0)
