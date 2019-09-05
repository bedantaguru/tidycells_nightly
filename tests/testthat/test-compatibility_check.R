
source("testlib/compatibility_check_test.R")

test_that("compatibility_check works I", {
  
  # skip on CRAN
  skip_on_cran()
  
  skip_if_offline()
  
  skip_if_not_installed("cli")
  
  if(utils::packageVersion("cli")<as.numeric_version("1.1.0")){
    skip("Neen cli 1.1.0")
  }
  
  x0 <- current_state_of_pkgs()
  cc1 <- compatibility_check(cli::cli_sitrep(), pkg = "cli", old_version = "1.0.0")
  x1 <- current_state_of_pkgs()
  
  expect_equal(x0, x1)
  expect_false(cc1$is_same)
})
