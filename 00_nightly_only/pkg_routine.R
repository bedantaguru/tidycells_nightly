
tools::toTitleCase(tolower("Read Tabular Data from Diverse Sources and Easily Make Them Tidy"))

"Read Tabular Data from Diverse Sources and Easily Make Them Tidy"

covr::package_coverage(line_exclusions = 
                         c("R/shiny_etc.R","R/shiny_main.R","R/shiny_parts_server.R","R/shiny_parts_server_components.R","R/shiny_parts_ui.R","R/visual_functions.R"))


# covr::coveralls(line_exclusions = c("R/shiny_etc.R","R/shiny_main.R","R/shiny_parts_server.R","R/shiny_parts_server_components.R","R/shiny_parts_ui.R","R/visual_functions.R"))

# if you have tidyxl installed
d <- system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE) %>% 
  read_cells(at_level = "make_cells") %>% 
  .[[1]]

# or you may do 
d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>% 
  readRDS()
d <- numeric_values_classifier(d)
da <- analyze_cells(d)
dc <- compose_cells(da, print_attribute_overview = TRUE)

# bit tricky and tedious unless you do print_attribute_overview = TRUE in above line
dcfine <- dc %>% 
  dplyr::mutate(name = dplyr::case_when(
    data_block == 1 ~ major_row_left_2_1,
    data_block == 2 ~ major_col_bottom_1_1,
    data_block == 3 ~ major_row_left_1_1
  ),
  sex = dplyr::case_when(
    data_block == 1 ~ major_row_left_1_1,
    data_block == 2 ~ major_col_bottom_2_1,
    data_block == 3 ~ minor_row_right_1_1
  ),
  school = dplyr::case_when(
    data_block == 1 ~ minor_col_top_1_1,
    data_block == 2 ~ minor_corner_topLeft_1_1,
    data_block == 3 ~ minor_col_top_1_1
  )) %>% 
  dplyr::select(school,sex, name, value)






#read_cells(list(d), from_level = 2)

# marks <- d
# 
# usethis::use_data(marks)

# failing on OSX
# https://stackoverflow.com/questions/38231896/r-unable-to-install-rcpp-package-for-r-3-3-1-on-osx-el-capitan

# appvyor link fix
# this is good
# https://johnmuschelli.com/neuroc/getting_ready_for_submission/
# Set up package ---------------------------------------------
library(usethis)

use_pkgdown()

# not running need to understand
# use_pkgdown_travis()
pkgdown::build_favicon()
pkgdown::build_site()
#pkgdown:::build_site_external()

usethis:::use_readme_rmd()

# r hub thing
# follow https://r-hub.github.io/rhub/articles/rhub.html
x <- platforms()
require(purrr)
# validate_email(email = "nil.gayen@gmail.com", token = "ed728b8460a7460081331fa6ca2e10b7")
rh <- x$name %>% map(~check(platform = .x, check_args = "--as-cran", show_status = FALSE))





####### start stable run ######
# these need to run in stable only
usethis::use_travis()
usethis::use_appveyor()
# for this you may consult https://github.com/krlmlr/r-appveyor#readme
usethis::use_coverage(type = "codecov")
usethis::use_coverage(type = "coveralls")

####### end stable run ######

#  this does not work
# you may use https://help.appveyor.com/discussions/questions/1164-how-do-i-run-a-script-after-all-matrix-possibilities-succeed
# on_success_all: or
# on_success: in appveyor
# this does not work

# devtools::build_win()

devtools::check_win_devel()

# http://r-pkgs.had.co.nz/release.html

# other few badges
# https://cran.r-project.org/web/packages/badgecreatr/vignettes/general_use_of_the_package.html

# https://github.com/GuangchuangYu/badger

# sticker for this
library(hexbin)
library(ggplot2)
# later todo
sticker("00_nightly_only/logo_bg.jpg",
        package = "tidycells",
        p_size=8, s_x=1, s_y=.75, s_width=.6,
        filename="00_nightly_only/logo.png")


library(covr)

codecov(token = "63912816-2943-4a4a-82c2-6c5192c89735")

library(tictoc)

tic()
covr::package_coverage(type = "all")
toc()
# 70.57 sec elapsed

covr::report()

# we use unit tests ok
tic()
covr::package_coverage()
toc()
# 43.86 sec elapsed

use_readme_rmd()
use_readme_md()
# use_git_config(user.name = "Indranil Gayen", user.email = "nil.gayen@gmail.com")
git_sitrep()

# https://usethis.r-lib.org/articles/articles/usethis-setup.html

covr::report()

use_coverage()
use_travis()
use_github()

use_mit_license("Indranil Gayen")

# https://dillinger.io/
use_vignette("tidycells-intro", title = "Tidycells Package")


use_addin(addin = "visual_crop")
use_addin(addin = "visual_data_block_inspection")
use_addin(addin = "visual_orientation_modification")
use_addin(addin = "visual_traceback")
use_addin(addin = "visual_va_classify")

use_roxygen_md()

use_pipe()

use_package("dplyr", min_version = utils::packageVersion("dplyr"))
use_package("purrr", min_version = utils::packageVersion("purrr"))
use_package("unpivotr", min_version = utils::packageVersion("unpivotr"))



use_package("tidyr")
use_package("ggplot2")
use_package("rlang")
use_package("stringr", min_version = utils::packageVersion("stringr"))

#need to make optional
# maybe not required as dplyr depends on cli
use_package("cli", type = "Suggests")

use_package("methods")
use_package("utils")
use_package("stats")
use_package("graphics")

# sugests

use_package("stringdist", type = "Suggests")

use_package("rstudioapi", type = "Suggests")
use_package("readr", type = "Suggests")
use_package("tidyxl", type = "Suggests")

use_package("plotly", type = "Suggests")
use_package("DT", type = "Suggests")
use_package("shiny", type = "Suggests")
use_package("miniUI", type = "Suggests")

# for read module

#'XML' 'docxtractr' 'readxl' 'tabulizer' 'xlsx'
use_package("XML", type = "Suggests")
use_package("docxtractr", type = "Suggests")
use_package("readxl", type = "Suggests")
use_package("tabulizer", type = "Suggests")
use_package("xlsx", type = "Suggests")


# to use dplyr
# ref
# https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-385731301

use_tibble()

use_testthat()
# remember
# Use of context() is no longer recommend

use_test(name = "as_cell_df")
use_test(name = "cells_to_df")
use_test(name = "VA_classifier")
use_test(name = "analyze_cells")
use_test(name = "read_cells")
use_test(name = "read_cells_real")
use_test(name = "print_cell_df")
use_test(name = "print_cell_analysis")
use_test(name = "collate_columns")

# check https://github.com/r-lib/devtools/issues/1912
# use saveRDS(version = 2)
saveRDS(enron_from_unpivotr_processed,
        "~/RBI/WorkSpace/R_Packages/Nightly/tidycells/tests/testthat/testdata/enron_from_unpivotr_processed.rds",
        version = 2)
saveRDS(WSS,
        "~/RBI/WorkSpace/R_Packages/Nightly/tidycells/tests/testthat/testdata/WSS.rds",
        version = 2)


#ref : https://masalmon.eu/2017/06/17/automatictools/

# devtools::check_cran() :---> https://github.com/r-lib/revdepcheck



quick_spell_check <- function(){
  ft <- "inst/WORDLIST"
  if(file.exists(ft)){
    unlink(ft)
  }else{
    suppressWarnings(readLines("00_nightly_only/WORDLIST") %>% writeLines(ft))
    print(devtools::spell_check())
    unlink(ft)
  }
}

devtools::install(build_vignettes = TRUE)
devtools::document()
devtools::build_manual()
styler::style_pkg()
lintr::lint_package()
devtools::spell_check()
quick_spell_check()
goodpractice::gp()





# pdf manual

# debug whic man causes it

library(tidyverse)

check_man <- function(){
  man_files <- list.files("man", full.names = TRUE)
  dir.create("man_backup", showWarnings = FALSE)
  file.copy(man_files, "man_backup")
  unlink(man_files)
  n_man_file <- list.files("man_backup", full.names = TRUE)
  tp <- tempdir()
  
  for_a_man <- function(mn){
    cat("Checking: ", basename(mn))
    file.copy(mn, "man")
    devtools::build_manual(path = tp)
    chk <- length(list.files(tp, ".pdf$")) > 0
    d0 <- tibble(man_file = basename(mn), test = chk)
    unlink(list.files(tp, ".pdf$", full.names = TRUE))
    unlink(list.files("man", full.names = TRUE))
    cat(" ... Done!", ifelse(chk, "ok","issues"),"\n")
    d0
  }
  
  all_checks <- n_man_file %>% map_df(for_a_man)
  
  file.copy(n_man_file, "man")
  unlink(tp, recursive = TRUE)
  unlink("man_backup", recursive = TRUE)
  all_checks
}


devtools::build_manual()


# https://github.com/hadley/strict
library(strict)
# pack <- "tidycells"
# path <- find.package(pack)
# system(paste(shQuote(file.path(R.home("bin"), "R")),
#              "CMD", "Rd2pdf", shQuote(path), "--no-clean"))

# global variable issue
# put all in x
# x %>%  str_split(" |\n") %>% unlist() %>% str_trim() %>% unique() %>% paste0(collapse = '","') %>% cat()

# https://journal.r-project.org/submissions.html

# my orcid

# https://orcid.org/0000-0003-0197-1944

# ref : https://kbroman.org/pkg_primer/

# https://cran.r-project.org/web/packages/submission_checklist.html

r_files <- list.files(full.names = TRUE, pattern = ".R$", recursive = TRUE)

require(tidyverse)

r_files %>% map(requirements::req_file) %>% unlist() %>% unique()

rf <- r_files %>% as.list()
names(rf) <- r_files
# fix TRUE --> TRUE
# fix FALSE --> FALSE
#rf %>% map(readLines) %>% map(~.x[str_detect(.x,"[- \\=]F")]) %>% unlist()
#rf %>% map(readLines) %>% map(~.x[str_detect(.x,"not\\(")]) %>% unlist()

prettycode::prettycode()

#migrate to rlang::warn and rlang::abort


rf %>% map(readLines) %>% map(~.x[str_detect(.x,":::")]) %>% map_lgl(~length(.x)>0) %>% which() %>% names()

rf %>% map(readLines) %>% map(~.x[str_detect(.x,":::")]) %>% unlist()
rf %>% map(readLines) %>% map(~.x[str_detect(.x,"tidycells")]) %>% unlist()

rf %>% map(readLines) %>% map(~.x[str_detect(.x,"magrittr")]) %>% unlist() %>% unique()
rf %>% map(readLines) %>% map(~.x[str_detect(.x,"\\$details\\$")]) %>% unlist() ->x
x[!str_detect(x, "data_attr_map_raw")]

rf %>% map(readLines) %>% map(~.x[str_detect(.x,"TODO")]) %>% unlist()
rf %>% map(readLines) %>% map(~.x[str_detect(.x,"@Dev")]) %>% unlist()
rf %>% map(readLines) %>% map(~.x[str_detect(.x,"browser")]) %>% unlist()

rf %>% map(readLines) %>% map(~.x[str_detect(.x," function\\(")])
rf %>% map(readLines) %>% map(~.x[str_detect(.x,"message\\(")]) %>% unlist()
rf %>% map(readLines) %>% map(~.x[str_detect(.x,"warning\\(")]) %>% unlist()
rf %>% map(readLines) %>% map(~.x[str_detect(.x,"stop\\(")]) %>% unlist() %>% names
rf %>% map(readLines) %>% map(~.x[str_detect(.x,"ggplot2::geom_")]) %>% unlist() %>% names

rf %>% map(readLines) %>% map(~.x[str_detect(.x,"call\\.")]) %>% unlist()

rf %>% map(readLines) %>% imap_dfr(~tibble(fn = .y, nc = max(max(nchar(.x))), ln = which.max(nchar(.x)))) %>% arrange(desc(nc))

tf_replacement <- function(code_path, write_back = TRUE){
  
  code_str <- readLines(code_path)
  
  F_str <- "^F$|[- \\=,]F$|^F[,) ]|[- \\=,]F[,) ]"
  T_str <- str_replace_all(F_str, "F","T")
  
  dc <- tibble(code = code_str)
  
  dc <- dc %>%
    mutate(fp = str_detect(code, F_str),
           tp = str_detect(code, T_str),
           fps = code %>% str_extract_all(F_str) %>% map(~.x %>% unique()),
           tps = code %>% str_extract_all(T_str) %>% map(~.x %>% unique()))
  
  dc <- dc %>%
    rowwise() %>%
    group_split() %>%
    map_df(~{
      if(.x$fp){
        .code <- .x$code
        for(.fp in .x$fps[[1]]){
          .code <- str_replace_all(.code, str_replace_all(.fp, "\\)", "\\\\\\)"), str_replace_all(.fp, "F", "FALSE"))
        }
        .x$code <-.code
      }
      
      if(.x$tp){
        .code <- .x$code
        for(.tp in .x$tps[[1]]){
          .code <- str_replace_all(.code, str_replace_all(.tp, "\\)", "\\\\\\)"), str_replace_all(.tp, "T", "TRUE"))
        }
        .x$code <-.code
      }
      
      .x
    }) %>%
    ungroup()
  
  if(write_back){
    writeLines(dc$code, code_path)
  }
  
  dc %>% summarise(sum(fp|tp)) %>% pull(1)
  
}


rf %>% map_int(~tf_replacement(.x, write_back = TRUE)) %>% unique()



#  global variable checks



# explicitly loaded on the fly
suggest_deps <- c("shiny","miniUI", "dplyr", "purrr", "stringr", "xlsx")

nsf <- suggest_deps %>% map(~getNamespaceExports(.x))

global_vars <- ". RN ad ag aid attr_gid attr_gid_split attr_group c_dim c_dim_data
    c_max c_max.x c_max.y c_min c_min.x c_min.y ccn cdt cell_group_type
    ch cid cn cn_id cn_id_ cname cname_new cname_ord coc col_a col_d
    col_names col_orig d d1 d2 data_block data_gid data_type data_types
    date_raw decision dir_n direction direction_basic direction_group
    dist dist_ dty dummy_order file_type full_dim full_dim_orig g_id_a
    g_id_e g_id_v gid id implemented is_blank is_blank_not_num_c
    is_blank_not_num_cb is_full_dim_present is_num m_dist md n_att n_dirs
    natural_gid nc new_attr_gid new_attr_group new_dist new_gid new_type
    new_type_c new_type_r not_num_c not_num_cb nt num_and_pm num_c
    num_c_len num_try optional_cols package pkg_installed present_num_c_b
    r r_dim r_dim_data r_max r_max.x r_max.y r_min r_min.x r_min.y
    raw_value rc rc_n rel_dim rid row_a row_d row_orig sheet
    support_possible this_attr_max_rel txt txt_orig txt_size_ type
    val_type value value_chk"

global_vars <- global_vars %>% str_split(" ") %>% unlist() %>%
  str_trim %>% unique()

global_vars <- global_vars[nchar(global_vars)>0]

global_vars <- setdiff(global_vars,c("lo_find",".jnew"))


nsf_g <- nsf %>% map(~intersect(.x, global_vars))

nsf_g %>%  reduce(intersect)

rest_g <- global_vars %>%  setdiff(unlist(nsf_g))

gloomy <- function(x, nb = 4){
  x <- sort(x)
  d0 <- tibble(x, blocks = ((seq(x) %% nb)==0) %>% cumsum())
  d0 %>% split(.$blocks) %>%
    map(~.x$x %>% paste0(collapse = '","') %>% paste0('"',., '",')) %>% unlist() %>%
    paste0(collapse = "\n")
}


rest_g %>% gloomy() %>% cat()
cat("\n")
nsf_g[[1]] %>% gloomy() %>% cat()
cat("\n")
nsf_g[[2]] %>% gloomy() %>% cat()

nsf_g[[3]]
