

# d <- tempfile()
# 
# dir.create(d)
# 
# archive::archive_extract("00_nightly_only/file_samples/dummy_file.7z", dir = d)
# archive::archive_extract("00_nightly_only/file_samples/dummy_file.rar", dir = d)
# 
# archive::archive_write()

# ref : https://github.com/jimhester/archive

# may not be in CRAN https://github.com/jimhester/archive/issues/28

remotes::install_github("jimhester/archive")

decompress.file_7z <- function(src, dest){
  archive::archive_extract(src, dest)
}

decompress.file_rar <- decompress.file_7z
