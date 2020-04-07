

### rhub
# rhub results expires soon
require(rhub)
x <- platforms()
require(purrr)
require(dplyr)
require(stringr)

# re validate always
# validate_email(email = "nil.gayen@gmail.com", token = "<do fresh>")

rh <- x$name %>% map(~check(platform = .x, check_args = "--as-cran", show_status = FALSE))

# after run
# y <- x$description %>% str_replace(", x86, 32 bit"," x86 32 bit") %>% str_remove_all("\\(experimental\\)") %>% str_split(",") %>% map(~c(.x[1], str_trim(paste0(.x[-1], collapse = ""))))
# 
# cr <- rh %>% map(~try(.x$cran_summary(), silent = T))

# after run
rhub_res <- rhub::list_my_checks()

rhub_gist <- rhub_res


rhub_gist <- rhub_gist %>% mutate(submit_date = as.Date(submitted), raw_state = status, state = ifelse(status=="preperror", "preperror", "ok"), reason_note = result %>% map_chr(~.x[c("notes", "warnings","errors")] %>% unlist %>% str_trim%>% paste0(collapse = "")))
y <- rhub_gist$platform %>% map_chr("description") %>% str_replace(", x86, 32 bit"," x86 32 bit") %>% str_remove_all("\\(experimental\\)") %>% str_split(",") %>% map(~c(.x[1], str_trim(paste0(.x[-1], collapse = ""))))

rhub_gist <- rhub_gist %>% mutate(os_desc = y %>% map_chr(1), r_version_tag = y %>% map_chr(2), os_type = rhub_gist$platform %>% map_chr("os-type"))

rhub_gist <- rhub_gist %>% mutate(where = "RHub", r_version = "", platform = "", url = paste0("https://builder.r-hub.io/status/original/", id))

rhub_gist_final <- rhub_gist %>% select(package, version, where, os_type, os_desc, r_version, r_version_tag, platform, state, reason_note, url)

saveRDS(rhub_gist_final, "00_nightly_only/all_builds/rhub")
saveRDS(rhub_res, "00_nightly_only/all_builds/rhub_raw")




# app vayor

require(rvest)

require(RSelenium)

av_link <- "https://ci.appveyor.com/project/bedantaguru/tidycells"

rd <- rsDriver(browser = "firefox", check = F)

rd$client$navigate(av_link)
Sys.sleep(5)
wp <- rd$client$getPageSource()[[1]] %>% read_html()

blds <- wp %>% html_nodes('.ng-binding[ng-href*="build"]')

av_res <- tibble(where = "AppVeyor", os_type = "Windows", os_desc= "",
                 r_version = "" , r_version_tag = blds %>% html_text() %>% str_remove("Environment:") %>% str_trim(),
                 platform = "", state = "OK", reason_note = "", 
                 URL = blds %>% html_attr("href") %>% str_split("/job/") %>% map_chr(2) %>% paste0("https://ci.appveyor.com/api/buildjobs/",.,"/log"))

saveRDS(av_res, "00_nightly_only/all_builds/AppVeyor")

# rd$client$open()

#  travis

tv_link <- "https://travis-ci.org/r-rudra/tidycells"

rd$client$navigate(tv_link)
Sys.sleep(5)
wp <- rd$client$getPageSource()[[1]] %>% read_html()

blds <- wp %>% html_node(".jobs-list") %>% html_nodes("li")

tv_res <- tibble(where="Travis", os_type= blds %>% html_nodes(".job-os") %>% html_attr("class") %>% str_remove("job-os "),
                 os_desc = "", r_version = "" , 
                 r_version_tag = blds %>% html_nodes(".job-lang") %>% html_text() %>% str_trim(),
                 platform = "", state = "OK", reason_note = "", 
                 URL = blds %>% html_node("a") %>% html_attr("href") %>% str_split("/jobs/") %>% map_chr(2) %>% paste0("https://api.travis-ci.org/v3/job/",.,"/log.txt"))


saveRDS(tv_res, "00_nightly_only/all_builds/Travis")

all_cloud <- list(rhub_gist_final %>% rename(URL =url), av_res, tv_res) %>% bind_rows()

all_cloud %>% mutate(submit_date = Sys.Date()-1)-> all_cloud

all_cloud <- all_cloud %>% select(package, version, submit_date, where, os_type, os_desc, r_version, r_version_tag, platform, state, reason_note, URL)

writexl::write_xlsx(all_cloud, "00_nightly_only/all_builds/all_clouds.xlsx")

