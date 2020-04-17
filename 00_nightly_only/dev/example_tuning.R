
options(lifecycle_verbosity = "warning")

#  it has to be added as test

#non uniformity  among different reader
tf0 <- read_cells("00_nightly_only/dev/example_tuning_check_points/master_pattern.xlsx")[[1]]
tf0 <- tf0 %>% detect_table_block()
tf0 %>% filter(Row = 1, Col = 1, keep= 5) -> xx

tf0 %>% filter(col>12, row<100) ->xx

tf0 %>%  filter(row>95) -> xx
# xx %>% mutate(gid_29 = gid_29+gid_13, gid_13 = gid_14+gid_13) %>% plot()

xx <- xx %>% numeric_values_classifier()



ca <- analyze_cells(xx)

d0 <- compose_cells(ca)
d1 <- collate_columns(d0)

# name suggest will come here
name_suggest(d1, ca)


f <- "00_nightly_only/dev/ABSSelfExplore/20490do001_2016.xls"

d_abs <- readxl::read_excel(f, sheet = "Table_1.4", col_names = F, col_types = "text")

d_abs <- d_abs %>% as_cell_df(take_col_names = F)

d_abs <- d_abs %>% numeric_values_classifier(orientation_based_attribute_recovery = T)

absd <- d_abs %>% filter(row < 108)

rm(d_abs,f)
# 
# setdiff(colnames(d1), c("row","col","value","data_block")) %>% 
#   map(~name_suggest(.x, ca))

ca <- analyse_cells(absd)

d0 <- compose_cells(ca)


# > microbenchmark::microbenchmark(ca <- analyze_cells(tf0), times = 1)
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# ca <- analyze_cells(tf0) 6.904038 6.904038 6.904038 6.904038 6.904038 6.904038     1


# > microbenchmark::microbenchmark(ca <- analyze_cells(tf0), times = 1)
# Unit: seconds
# expr     min      lq    mean  median      uq     max neval
# ca <- analyze_cells(tf0) 13.3713 13.3713 13.3713 13.3713 13.3713 13.3713     1
# >
