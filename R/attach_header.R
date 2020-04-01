



attach_header <- function(dat, hdr, 
                          direction = c("v","h","vl","vr","vm","hu","hd","hm","direct")){
  direction <- match.arg(direction)
  do.call(paste0("attach_header_",direction), list(dat = dat, hdr = hdr))
}

#### micro functions

attach_header_v <- function(dat, hdr){
  hdr$row <- NULL
  inner_join(dat, hdr, by = "col", suffix = c("",".header"))
}

attach_header_h <- function(dat, hdr){
  hdr$col <- NULL
  inner_join(dat, hdr, by = "row", suffix = c("",".header"))
}

# jk: join key
attach_header_vl <- function(dat, hdr){
  hdr$jk <- hdr$col
  hdr$col <- NULL
  hdr$row <- NULL
  dat <- dat %>% 
    mutate(jk = header_data_dir_cuts(col, 
                                     hdr$jk %>% unique() %>% sort()))
  do <- inner_join(dat, hdr, by = "jk", suffix = c("",".header"))
  do$jk <- NULL
  do
}

attach_header_vr <- function(dat, hdr){
  hdr$jk <- hdr$col
  hdr$col <- NULL
  hdr$row <- NULL
  dat <- dat %>% 
    mutate(jk = header_data_dir_cuts(col, 
                                     hdr$jk %>% unique() %>% sort(),
                                     direction = -1))
  do <- inner_join(dat, hdr, by = "jk", suffix = c("",".header"))
  do$jk <- NULL
  do
}

attach_header_vm <- function(dat, hdr){
  hdr$jk <- hdr$col
  hdr$col <- NULL
  hdr$row <- NULL
  dat <- dat %>% 
    mutate(jk = header_data_dir_cuts_mid(col, 
                                     hdr$jk %>% unique() %>% sort()))
  do <- inner_join(dat, hdr, by = "jk", suffix = c("",".header"))
  do$jk <- NULL
  do
}


attach_header_hu <- function(dat, hdr){
  hdr$jk <- hdr$row
  hdr$col <- NULL
  hdr$row <- NULL
  dat <- dat %>% 
    mutate(jk = header_data_dir_cuts(row, 
                                     hdr$jk %>% unique() %>% sort()))
  do <- inner_join(dat, hdr, by = "jk", suffix = c("",".header"))
  do$jk <- NULL
  do
}

attach_header_hd <- function(dat, hdr){
  hdr$jk <- hdr$row
  hdr$col <- NULL
  hdr$row <- NULL
  dat <- dat %>% 
    mutate(jk = header_data_dir_cuts(row, 
                                     hdr$jk %>% unique() %>% sort(),
                                     direction = -1))
  do <- inner_join(dat, hdr, by = "jk", suffix = c("",".header"))
  do$jk <- NULL
  do
}

attach_header_hm <- function(dat, hdr){
  hdr$jk <- hdr$row
  hdr$col <- NULL
  hdr$row <- NULL
  dat <- dat %>% 
    mutate(jk = header_data_dir_cuts_mid(row, 
                                         hdr$jk %>% unique() %>% sort()))
  do <- inner_join(dat, hdr, by = "jk", suffix = c("",".header"))
  do$jk <- NULL
  do
}

attach_header_direct <- function(dat, hdr){
  hdr$jk <- 1
  hdr$col <- NULL
  hdr$row <- NULL
  dat <- dat %>% 
    mutate(jk = 1)
  do <- inner_join(dat, hdr, by = "jk", suffix = c("",".header"))
  do$jk <- NULL
  do
}

#### helpers

header_data_dir_cuts <- function(x, hdr, direction = 1){
  # direction will be either +1 or -1 (infact any +ve or -ve!)
  # hdr should be sorted increasingly
  if(direction<0){
    hdr <- rev(-hdr)
    x <- -x
  }
  fi <- findInterval(x, hdr)
  fi[fi==0]<-NA
  sign(direction)*hdr[fi]
}

header_data_dir_cuts_mid <- function(x, hdr){
  # hdr should be sorted increasingly
  hdr_mids <- (hdr[-1]+hdr[-length(hdr)])/2
  hdr_mids <- c(-Inf, hdr_mids)
  fi <- findInterval(x, hdr_mids)
  fi[fi==0]<-NA
  hdr[fi]
}

