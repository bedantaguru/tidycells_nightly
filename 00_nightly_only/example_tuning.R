

#  it has to be added as test

#non uniformity  among different reader
tf0 <- read_cells("00_nightly_only/example_tuning_check_points/master_pattern.xlsx")[[1]]
tf0 <- tf0 %>% detect_table_block()
tf0 %>% filter(Row = 1, Col = 1, keep= 5) -> xx
# xx %>% mutate(gid_29 = gid_29+gid_13, gid_13 = gid_14+gid_13) %>% plot()

xx <- xx %>% numeric_values_classifier()

ca <- analyze_cells(xx)

compose_cells(ca)

# for a attr_gid

stitch_direction(dcomp00[[1]][[1]], xx)


# we are at ai_get_sync_names_for_attr_gid_splits
# load 
admap_cellwise_raw <- readRDS("00_nightly_only/admap_cellwise_raw_asp.rds")



dcomp0 %>% map("data_block") %>% map_chr(1)->tt

dc <- (dcomp0[tt=="d1"] %>% reduce(fj)) %>% 
  bind_rows(dcomp0[tt=="d2"] %>% reduce(fj))


unique(tt) %>% map(~{
  dcomp0[tt==.x] %>% reduce(fj)
})


PTXQC::LCS