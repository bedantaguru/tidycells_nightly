
# this file is required by shiny module tests


# may need to run
# devtools::load_all()
# after record test you may do
# this function is kept for test development and not for test to run.
pull_shiny_test <-function(x){
  testf <- list.files(x$app_dir, pattern = "test", include.dirs = TRUE, full.names = TRUE)
  here_testf <- file.path("tests/testthat/testshiny", x$name)
  dir.create(here_testf, showWarnings = FALSE, recursive = TRUE)
  file.copy(testf, here_testf, overwrite = TRUE, recursive = TRUE)
}

temp_app_create <- function(es, name){
  if(missing(name)){
    stop("give name")
  }
  td <- tempdir(check = TRUE)
  td_this <- tempfile(pattern = "shiny_test_app_dir", tmpdir = td)
  unlink(td_this, recursive = TRUE)
  dir.create(td_this, showWarnings = FALSE)
  tf_for_es <- file.path(td_this, "data")
  tf_for_app <- file.path(td_this, "app.R")
  saveRDS(es, file = tf_for_es)
  # code_this <- paste0("es <- readRDS('",normalizePath(tf_for_es, winslash = "/"),"')\n",
  #                     "shinyApp(es$ui, es$server)\n")
  code_this <- paste0("es <- readRDS('data')\n",
                      "shinyApp(es$ui, es$server)\n")
  writeLines(code_this, tf_for_app)
  list(app_dir = td_this, app = tf_for_app, es = tf_for_es, name = name)
}

# if set options(LOCAL_TEST_IN_SHINYTEST = TRUE)
# it will be tested from project root instead of "tests/testthat/"
copy_test_to_temp_app <- function(x){
  if(identical(getOption("LOCAL_TEST_IN_SHINYTEST"), TRUE)){
    # for internal checks only
    message("Testing in local environment. Reading SHINYTEST from project root.")
    here_testf <- file.path("tests/testthat/testshiny", x$name, "tests")
  }else{
    here_testf <- file.path("testshiny", x$name, "tests")
  }
  if(file.exists(here_testf)){
    file.copy(here_testf, x$app_dir, overwrite = TRUE, recursive = TRUE)
  }
}

clean_temp_app <- function(x){
  unlink(x$app)
  unlink(x$es)
  unlink(x$app_dir, recursive = TRUE)
}

# need to set
# options(TEST_IMAGE_IN_SHINYTEST = TRUE)
# or
# Sys.setenv(TEST_IMAGE_IN_SHINYTEST = "true")
# for testing snapshots
image_test <- function(enable_now){
  # disable on Travis
  if(identical(Sys.getenv("TRAVIS"), "true")){
    return(FALSE)
  }

  if(!missing(enable_now)){
    if(identical(enable_now, TRUE)){
      # sets it for further calls
      Sys.setenv(TEST_IMAGE_IN_SHINYTEST = "true")
    }
  }
  # unless TEST_IMAGE_IN_SHINYTEST is set to TRUE it will not run image comparison
  opt_chk <- identical(getOption("TEST_IMAGE_IN_SHINYTEST"), TRUE)
  env_chk <- identical(Sys.getenv("TEST_IMAGE_IN_SHINYTEST"), "true")

  if(env_chk | opt_chk){
    return(TRUE)
  }

  return(FALSE)
}

test_temp_app <- function(x, test_img){
  copy_test_to_temp_app(x)

  img_chk <- image_test()
  if(!missing(test_img)){
    if(!identical(test_img, TRUE)){
      img_chk <- FALSE
    }
  }

  if(img_chk){
    message("shintest: checking images")
  }else{
    message("shintest: NOT checking images")
  }

  shinytest::expect_pass(shinytest::testApp(x$app_dir, compareImages = img_chk))

  clean_temp_app(x)
}

inst_deps <- function(x){
  if(rlang::is_installed("shinytest")){
    if(!shinytest::dependenciesInstalled()){
      shinytest::installDependencies()
    }
  }
}
