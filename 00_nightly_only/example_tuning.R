

#  it has to be added as test


tf0 <- read_cells("00_nightly_only/example_tuning_check_points/master_pattern.xlsx")[[1]]
tf0 <- tf0 %>% detect_table_block()
tf0 %>% filter(Row = 1, Col = 1, keep= 5) -> xx
# xx %>% mutate(gid_29 = gid_29+gid_13, gid_13 = gid_14+gid_13) %>% plot()

xx <- xx %>% numeric_values_classifier()

analyze_cells(xx)

# for a attr_gid
