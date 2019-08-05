


test_that("collate_columns works", {
  d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>%
    readRDS()
  d <- numeric_values_classifier(d)
  da <- analyze_cells(d)

  expect_output(
    dc1 <- compose_cells(da, print_attribute_overview = TRUE),
    "Nakshatra Gayen, Titas Gupta, Ujjaini Gayen, Utsyo Roy"
  )
  dc2 <- compose_cells(da, post_process = FALSE)
  cc1 <- collate_columns(dc1)
  cc2 <- collate_columns(dc2)

  expect_equal(cc1, cc2)
  expect_identical(
    cc1 %>% unlist() %>% unique() %>% sort(),
    cc2 %>% unlist() %>% unique() %>% sort()
  )

  #  all School should appear in single column
  expect_equal(
    cc1 %>% purrr::map_lgl(~ any(stringr::str_detect(.x, "School"))) %>% sum(),
    1
  )
  # all Male/Female should appear in single column
  expect_equal(
    cc1 %>% purrr::map_lgl(~ any(stringr::str_detect(.x, "Male"))) %>% sum(),
    1
  )
  # strict test (may be dropped in future)
  expect_equal(
    cc1 %>% map(~ .x %>%
      unique() %>%
      length()) %>% unlist() %>% sort() %>% as.numeric(),
    c(2, 2, 3, 12, 12)
  )
})
