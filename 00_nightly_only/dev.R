

require(dplyr)
require(purrr)

# cli::cli_sitrep()

# compatibility check proto added

expression_run_safe <- function(expr, no_class = FALSE){
  expr_fn <- rlang::as_function(expr)
  sink(tempfile())
  e <- suppressMessages(suppressWarnings(
    try(expr_fn(), silent = TRUE)
  ))
  sink(NULL)
  if(no_class & inherits(e, "try-error")){
    e <- attr(e, "condition")
  }
  e
}

compatibility_check <- function(expr, old_version, pkg = "tidycells", repo = getOption("repos"), warn_outdated = TRUE){
  now_lib_path <- .libPaths()
  
  
  if(!stringr::str_detect(repo, "/$")){
    repo <- paste0(repo, "/")
  }
  
  on.exit(.libPaths(now_lib_path))
  
  output <- list()
  
  dnsl <- re_attach(pkg)
  expression_run_safe(~{
    es <- loadNamespace(pkg)
    attachNamespace(es)
  })
  # current run
  output$current$ver <- expression_run_safe(~utils::packageVersion(pkg, lib.loc = .libPaths()))
  output$current$res <- expression_run_safe(expr, no_class = TRUE)
  
  # do it for remote
  temp_dir <- tempfile(pattern = "inst_check", tmpdir = tempdir(check = TRUE))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # change libpath
  .libPaths(c(temp_dir, now_lib_path))
  if(normalizePath(.libPaths()[1])!=normalizePath(temp_dir)){
    stop("Failed to change libPaths")
  }
  
  # install package
  if(!missing(old_version)){
    old_version_n <- as.numeric_version(old_version)
    old_url <-  paste0(repo, "src/contrib/Archive/",pkg,"/",pkg,"_",old_version,".tar.gz")
    loc_path <- paste0(temp_dir, "/",pkg,"_",old_version,".tar.gz")
    e <- expression_run_safe(~download.file(old_url, loc_path, quiet = TRUE))
    if(inherits(e, "try-error")){
      stop("Unable to donwload the package.")
    }else{
      # proceed
      e <- expression_run_safe(~utils::install.packages(loc_path, repos = NULL, quiet = TRUE))
      if(inherits(e, "try-error")){
        stop("Unable to install the package.")
      }else{
        # proceed
        chk_ver <- expression_run_safe(~utils::packageVersion(pkg, lib.loc = .libPaths()))
        if(chk_ver!=old_version_n){
          stop("Temporary installed version not matching with given version")
        }
      }
    }
  }else{
    #  install CRAN version
    e <- expression_run_safe(~utils::install.packages(pkg, repos = repo, quiet = TRUE))
    if(inherits(e, "try-error")){
      stop("Unable to install the package.")
    }
  }
  
  # this section need to customized for pkgs
  # all downstream dependency has to be unloaded first
  re_attach(pkg)
  expression_run_safe(~{
    es <- loadNamespace(pkg)
    attachNamespace(es)
  })
  
  
  
  output$target$ver <- expression_run_safe(~utils::packageVersion(pkg), no_class = TRUE)
  output$target$res <- expression_run_safe(expr, no_class = TRUE)
  
  output$is_same <- identical(output$target$res, output$current$res)
  
  .libPaths(now_lib_path)
  re_attach(pkg)
  expression_run_safe(~{
    es <- loadNamespace(pkg)
    attachNamespace(es)
  })
  dnsl %>% map(re_attach)
  unlink(temp_dir, recursive = TRUE, force = TRUE)
  
  if(warn_outdated){
    if(output$target$ver > output$current$ver){
      warning("You are using outdated version of the package.")
    }
  }
  
  output
}

# compatibility_check(~cli::cli_sitrep(), old_version = "1.0.0")
# compatibility_check(~cli::cli_sitrep())
# compatibility_check(~utils::packageVersion("pkgbuild"), pkg = "pkgbuild")
# compatibility_check(~system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE) %>% 
#                       read_cells())


# getNamespaceExports("cli")
# exists("cli_sitrep")
# library(cli)
# exists("cli_sitrep")
# unloadNamespace("cli")
# exists("cli_sitrep")

deps <- function(pkg){
  if(!is.data.frame(pkg)){
    dpt <- tibble(pkg = pkg)
  }else{
    dpt <- pkg
  }
  if(nrow(dpt)==0) return(NULL)
  dpt <- dpt %>% mutate(dep = pkg %>% map(getNamespaceUsers)) %>% tidyr::unnest()
  dpt %>% bind_rows(dpt %>% select(pkg = dep) %>% deps)
}


re_attach <- function(pkg){
  if(is.null(pkg)) return(invisible(NULL))
  this_deps <- deps(pkg)
  detach_list <- NULL
  if(nrow(this_deps)>0){
    while (nrow(this_deps)>0) {
      unloadNamespace(this_deps$dep[nrow(this_deps)])
      detach_list <- c(this_deps$dep[nrow(this_deps)], detach_list) %>% unique()
      this_deps <- this_deps %>% filter(!(dep %in% detach_list))
    }
  }
  unloadNamespace(pkg)
  if(length(detach_list)>0){
    detach_list %>% map(~try(loadNamespace(.x), silent = TRUE))
  }
  loadNamespace(pkg)
  invisible(detach_list)
}

# getNamespaceUsers
deps_one <- function(pkg){
  ips <- utils::installed.packages() %>% as_tibble()
  ips_dep <- ips %>% select(Package, Depends, Imports) %>% 
    mutate(imps = stringr::str_extract_all(Imports, "[a-zA-Z0-9]+"), deps =  stringr::str_extract_all(Depends, "[a-zA-Z0-9]+"))
  ips_dep_this <-ips_dep %>% 
    filter((imps %>% map_lgl(~pkg %in% .x)) | (deps %>% map_lgl(~pkg %in% .x)))
  
  this_deps <- ips_dep_this$Package %>% unique()
  this_deps %>% map_lgl(isNamespaceLoaded) %>% this_deps[.]
}


#################

dcomp02 <- dcomp00 %>%
  map(~ .x %>%
        # this try should be removed if unpivotr::enhead is internalized
        # or similar behaving fucntions is developed.
        map(~try(stitch_direction(.x, ca$cell_df, trace_it = trace_it_back), silent = TRUE)))

dcomp00 <- dam %>%
  group_by(data_gid) %>%
  group_split() %>%
  map(~ .x %>%
        group_by(attr_gid, direction, attr_gid_split) %>%
        group_split())


fails <- dcomp0 %>% map(~.x %>% map_lgl(~inherits(.x, "try-error")))




stitch_direction(dcomp00[[1]][[4]], ca$cell_df)
stitch_direction(dcomp00[[1]][[2]], ca$cell_df)


#  failure for pcd

pcd <- readRDS("00_nightly_only/pcd")

ca <- analyze_cells(pcd)

# after  
# devtools::install_github("tidyverse/tidyr")
require(tidyr)

# not in cran
# desc::desc(package = "tidyr")$get_field("Repository")
"Repository" %in% desc::desc(package = "tidyr")$fields()
"URL" %in% desc::desc(package = "tidyr")$fields()

packageVersion("tidyr")

iris %>% nest(-Species, .key = "attr")
iris %>% nest(-Species, attr = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))
# this is fine
iris %>% nest(attr = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))


admap2_pass <- admap3$map


admap2_pass %>%
  rename(md = dist) %>% 
  group_by(data_gid, direction_group, attr_group) %>%
  mutate(m_dist = min(md))

admap2_pass <- admap2_pass %>%
  rename(md = dist) %>% 
  group_by(data_gid, direction_group, attr_group) %>%
  mutate(m_dist = min(md)) %>%
  ungroup() %>%
  filter(md == m_dist) %>%
  select(-md) %>%
  rename(dist = m_dist)


# internet is required for this 
Sys.setenv(http_proxy="http://172.30.1.78:3128")
Sys.setenv(HTTPS_PROXY = "http://172.30.1.78:3128")

# same vars
d0 <- structure(
  list(row = 2L, col = 1L, value = "5.1"), 
  row.names = c(NA, -1L), 
  class = c("tbl_df", "tbl", "data.frame"))

a0 <- structure(
  list(row = 1L, col = 5L, attr = "Species"), 
  row.names = c(NA,-1L), 
  class = c("tbl_df", "tbl", "data.frame"))

# remove prior installation for this reprex
# unloadNamespace("unpivotr")
# unloadNamespace("tidyr")
# remove.packages("tidyr", lib="~/R/win-library/3.6")
# remove.packages("unpivotr", lib="~/R/win-library/3.6")

# get CRAN version
devtools::install_cran("tidyr", quiet = TRUE)
devtools::install_cran("unpivotr", quiet = TRUE)

#####################
# test code (start) #
#####################
packageVersion("tidyr")
packageVersion("unpivotr")

unpivotr::enhead(d0, a0, "NNE")

#####################
# test code (end)   #
#####################


# this portion may require to run manually
unloadNamespace("unpivotr")
try(detach("package:unpivotr", unload = TRUE), silent = TRUE)
unloadNamespace("tidyr")
try(detach("package:tidyr", unload = TRUE), silent = TRUE)

# get DEV - tidyr
remove.packages("tidyr", lib="~/R/win-library/3.6")
devtools::install_github("tidyverse/tidyr", quiet = TRUE)


# same vars
d0 <- structure(
  list(row = 2L, col = 1L, value = "5.1"), 
  row.names = c(NA, -1L), 
  class = c("tbl_df", "tbl", "data.frame"))

a0 <- structure(
  list(row = 1L, col = 5L, attr = "Species"), 
  row.names = c(NA,-1L), 
  class = c("tbl_df", "tbl", "data.frame"))

#####################
# test code (start) #
#####################
packageVersion("tidyr")
packageVersion("unpivotr")

# this gives warning
unpivotr::enhead(d0, a0, "NNE")

#####################
# test code (end)   #
#####################

# this portion may require to run manually
unloadNamespace("unpivotr")
try(detach("package:unpivotr", unload = TRUE), silent = TRUE)
unloadNamespace("tidyr")
try(detach("package:tidyr", unload = TRUE), silent = TRUE)

# get DEV - unpivotr
remove.packages("unpivotr", lib="~/R/win-library/3.6")
devtools::install_github("nacnudus/unpivotr", quiet = TRUE)


#####################
# test code (start) #
#####################
packageVersion("tidyr")
packageVersion("unpivotr")

unpivotr::enhead(d0, a0, "NNE")
#####################
# test code (end)   #
#####################

#  undo changes
# unloadNamespace("unpivotr")
# unloadNamespace("tidyr")
# remove.packages("tidyr", lib="~/R/win-library/3.6")
# remove.packages("unpivotr", lib="~/R/win-library/3.6")
# devtools::install_cran("tidyr", quiet = TRUE)
# devtools::install_cran("unpivotr", quiet = TRUE)

###############################################








dam %>%
  group_by(data_gid) %>%
  group_split() ->x

y <-  x[[1]] %>%
  group_by(attr_gid, direction, attr_gid_split) %>%
  group_split()

stitch_direction(y[[1]], ca$cell_df, trace_it = FALSE)

x[[1]] %>%
  group_by(attr_gid, direction, attr_gid_split) %>%
  group_split() %>%
  map(~ try(stitch_direction(.x, ca$cell_df, trace_it = trace_it_back)))

 x[[1]] %>%
   group_by(attr_gid, direction, attr_gid_split) %>%
   group_split() %>%
   map(~ stitch_direction(.x, ca$cell_df, trace_it = trace_it_back)) %>%
   reduce(fj_this)



d_att_map <- dat_boundary %>%
  split(.$gid) %>%
  map_df(~ get_direction_df(.x, datt = att_gid_map, allow_inside = TRUE))

# dev for analyze_cells

admap1_major_minor$map %>%
  rename(md = dist) %>% 
    group_by(data_gid, direction_group) %>%
    mutate(m_dist = min(md)) %>%
    ungroup() %>%
    filter(md == m_dist) %>%
    select(-md) %>%
    rename(dist = m_dist)

admap1_try <- admap0$all_map %>% 
  rename(attr_gid = gid, dist = md ) %>%
  filter(direction_group != "corner") %>%
  ai_get_data_attr_map_details(d_dat, d_att)


admap1 <- admap0$map %>%
  filter(direction_group != "corner") %>%
  ai_get_data_attr_map_details(d_dat, d_att)

admap0$map %>% group_by(attr_gid, data_gid) %>% mutate(n=n()) %>% ungroup() %>% filter(n>1)


"15000"

plot(d, no_plot = TRUE)+
  ggplot2::geom_tile(data =  d_att$group_id_map %>% filter(gid == "13001") %>% mutate(value =NA, type = "empty"))


rt <- 3; plot(d, no_plot = TRUE)+
  ggplot2::geom_tile(data =  d_dat$group_id_map %>% filter(gid == d_dat$group_id_boundary$gid[rt]) %>% mutate(value =NA, type = "empty"))


rt <- 3; plot(d, no_plot = TRUE)+
  ggplot2::geom_tile(data = d_att$group_id_map %>%  filter(gid == chk$attr_gid[rt]) %>% mutate(value =NA, type = "empty"))+
  ggplot2::geom_tile(data = admap_main$raw_map %>% distinct(gid = data_gid, row =row_d, col =col_d) %>%  filter(gid == chk$data_gid[rt]) %>% mutate(value =NA, type = "empty"))


plot(dc0$cell_df, no_plot = TRUE)+
  ggplot2::geom_tile(data = dc0$details$attr_details$group_id_map %>%  filter(gid == "130000_12000_N") %>% mutate(value =NA, type = "empty"))+
  ggplot2::geom_tile(data = dc0$details$data_details$group_id_map %>%  filter(gid == "12000") %>% mutate(value =NA, type = "empty"))


rt <- 6; am <- admap2$map %>% filter(data_gid == 12004);plot(d, no_plot = TRUE)+
  ggplot2::geom_tile(data = d_att$group_id_map %>%  filter(gid == am$attr_gid[rt]) %>% mutate(value =NA, type = "empty"))+
  ggplot2::geom_tile(data = d_dat$group_id_map %>%  filter(gid == am$data_gid[rt]) %>% mutate(value =NA, type = "empty"))

plot(d, no_plot = TRUE)+ggplot2::geom_tile(data = d_att$group_id_map %>%  filter(gid == 6001) %>% mutate(value =NA, type = "empty"))

plot(d, no_plot = TRUE)+ggplot2::geom_tile(data = d_att$group_id_map %>% filter(gid %in% unmapped_attr_gids[1]) %>% mutate(value =NA, type = "empty"))

admap$raw_map %>% filter(attr_group == "minor") %>% filter(attr_gid == unique(attr_gid)[3]) %>% ai_attach_direction()


#  check dendency badge 
# https://github.com/markvanderloo/stringdist/blob/master/README.md
# [![status](https://tinyverse.netlify.com/badge/stringdist)](https://CRAN.R-project.org/package=stringdist)


d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>% 
  readRDS()
d <- numeric_values_classifier(d)
da <- analyze_cells(d)

dc <- compose_cells(da)

dcl <- dc



# dep stringdist

# q_gram q = 3
# method=jw, p=0.1
# method='osa'
# method='jaccard'
# soundex

# xp <- stringdist::phonetic(x)
# yp <- stringdist::phonetic(y)



