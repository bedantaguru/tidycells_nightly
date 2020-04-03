
options(lifecycle_verbosity = "warning")

#  it has to be added as test

#non uniformity  among different reader
tf0 <- read_cells("00_nightly_only/example_tuning_check_points/master_pattern.xlsx")[[1]]
tf0 <- tf0 %>% detect_table_block()
tf0 %>% filter(Row = 1, Col = 1, keep= 5) -> xx
# xx %>% mutate(gid_29 = gid_29+gid_13, gid_13 = gid_14+gid_13) %>% plot()

xx <- xx %>% numeric_values_classifier()


d0 <- compose_cells(ca)

