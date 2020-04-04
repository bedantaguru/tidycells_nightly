
ads_score <- function(ads) {
  ads[ads < 0] <- 0
  ads[ads > 10^10] <- 10^10
  ads[is.nan(ads)] <- 0
  (stats::median(ads, na.rm = TRUE) * 12 + mean(ads, na.rm = TRUE) * 6 + min(ads, na.rm = TRUE) * 1 + max(ads, na.rm = TRUE) * 1) / 20
}

character_set <- function(x){
  x %>% 
    tolower() %>% 
    stringr::str_split("") %>% 
    unlist() %>% 
    unique()
}

similarity_score <- function(x, y) {
  
  comm <- intersect(x, y)
  bscore <- 1 - length(comm) / mean(length(x), length(y))

  if (is.na(bscore)) bscore <- 0
  if (bscore > 10^10) bscore <- 10^10
  if (bscore < 0) bscore <- 0
  
  # score based on char sets
  cx <- character_set(x)
  cy <- character_set(y)
  comm_c <- intersect(cx, cy)
  bscore_c <- 1 - length(comm_c) / mean(length(cx), length(cy))
  
  if (is.na(bscore_c)) bscore_c <- 0
  if (bscore_c > 10^10) bscore_c <- 10^10
  if (bscore_c < 0) bscore_c <- 0
  

  # Levenshtein distance
  ads <- utils::adist(x, y)

  ads_l <- list(ads)

  if (is_available("stringdist")) {
    suppressMessages(suppressWarnings({
      mthds <- c("osa", "jaccard", "soundex", "jw")
      ads_sdl <- mthds %>% 
        map(~ stringdist::stringdistmatrix(x, y, method = .x, p = 0.1, q = 3))
      ads_l <- c(ads_l, ads_sdl)
    }))
  }

  ascore <- ads_l %>% map_dbl(ads_score)
  c(ascore, bscore, bscore_c)
}
