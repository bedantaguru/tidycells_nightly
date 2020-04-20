
options(lifecycle_verbosity = "warning")

#  it has to be added as test

#non uniformity  among different reader
tf0 <- read_cells("00_nightly_only/dev/example_tuning_check_points/master_pattern.xlsx")[[1]]
tf0 <- tf0 %>% numeric_values_classifier()
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



usethis::use_rcpp(name = "is_attachable_cpp")

# microbenchmark::microbenchmark(get_raw_map_for_ai_get_data_attr_map(gbds$normal, gbds$flat), get_raw_map_for_ai_get_data_attr_map(gbds$normal, d_dat), times = 2)
# Unit: milliseconds
# expr      min       lq     mean   median       uq
# get_raw_map_for_ai_get_data_attr_map(gbds$normal, gbds$flat) 278.9467 278.9467 351.5267 351.5267 424.1067
# get_raw_map_for_ai_get_data_attr_map(gbds$normal, d_dat) 280.0970 280.0970 281.0712 281.0712 282.0454
# max neval
# 424.1067     2
# 282.0454     2


#gbds <- get_group_id_boundary(d_dat, need_both = T)
# dg_sides <- get_raw_map_for_ai_get_data_attr_map(gbds$normal, gbds$flat)
# dg_sides <- get_raw_map_for_ai_get_data_attr_map(gbds$normal, d_dat)
# 
# gbds <- get_group_id_boundary(d_dat)
# 
# dg_sides <- ai_get_data_attr_map(gbds, d_dat)$map
# 
# dg_sides$mapping_strength <- NULL
# 
# dg_sides <- dg_sides %>%  rename(g1 = data_gid, g2 =attr_gid) %>% filter(g1!=g2)
# 
# dg_sides <- dg_sides %>% mutate(gid1 = pmin(g1, g2), gid2 = pmax(g1, g2))
# 
# 
# dg_sides <- dg_sides %>% group_by(gid1, gid2) %>% filter(g1==gid1) %>% select(-g1, -g2)
# 
# 
