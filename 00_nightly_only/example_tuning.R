
options(lifecycle_verbosity = "warning")

#  it has to be added as test

#non uniformity  among different reader
tf0 <- read_cells("00_nightly_only/example_tuning_check_points/master_pattern.xlsx")[[1]]
tf0 <- tf0 %>% detect_table_block()
tf0 %>% filter(Row = 1, Col = 1, keep= 5) -> xx
# xx %>% mutate(gid_29 = gid_29+gid_13, gid_13 = gid_14+gid_13) %>% plot()

xx <- xx %>% numeric_values_classifier()

ca <- analyze_cells(xx)

d0 <- compose_cells(ca)

d1 <- collate_columns(d0)

# name suggest will come here
name_suggest(d1, ca)



# 
# setdiff(colnames(d1), c("row","col","value","data_block")) %>% 
#   map(~name_suggest(.x, ca))
