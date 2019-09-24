
#' @include tidycells-package.R

# `file_type_from_magic_numbers` is designed for cases which 
# 1. **wand** fails to detect and still required by **tidycells**
# 2. wand is not available etc.

# this hosts functions like mn_* this means magic_number_*

file_type_from_magic_numbers <- function(filename){
  fbytes <- readBin(filename, n = tidycells_pkg_env$magic_numbers$mx_length, what = "raw")
  mns_this <- tidycells_pkg_env$magic_numbers
  mns_this$mx_length <- NULL
  mn_chk_res <- mns_this %>% map_lgl(~mn_check(fbytes, .x))
  
}

mn_check <- function(bytes, pre_def_mn){
  if(is.list(pre_def_mn)){
    for(i in 1:length(pre_def_mn)){
      chk <- mn_check(fbytes, pre_def_mn[[i]])
      if(chk) break()
    }
    chk
  }else{
    identical(bytes[1:length(pre_def_mn)], pre_def_mn)
  }
  
}

this_domain_magic_numbers <- function(){
  
  mns <- list(mx_length = 0)
  
  # as xls and doc both have same magic number "D0 CF 11 E0 A1 B1 1A E1"
  # many other have it though
  # ref : https://asecuritysite.com/forensics/magic
  mns$xls_doc_magic <- as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1))
  # either xlsx or docx
  # as xlsx and docx both have same magic number "50 4B 03 04"
  mns$xlsx_docx_magic <- as.raw(c(0x50, 0x4b, 0x03, 0x04))
  
  mns$pdf_magic <- as.raw(c(0x25, 0x50, 0x44, 0x46))
  
  
  # this is taken from 
  # https://github.com/cran/sas7bdat/blob/c8afa85b104d7289a80e7da9a1f596f29c6d9e4a/R/sas7bdat.R#L269
  # also check 
  # https://github.com/BioStatMatt/sas7bdat
  # https://github.com/dataiku/sassyreader/blob/master/src/test/java/org/eobjects/metamodel/sas/SasReaderTestBase.java
  mns$sas_magic  <- as.raw(c(0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
                         0x0, 0x0, 0x0, 0x0, 0xc2,0xea,0x81,0x60,
                         0xb3,0x14,0x11,0xcf,0xbd,0x92,0x8, 0x0,
                         0x9, 0xc7,0x31,0x8c,0x18,0x1f,0x10,0x11))
  
  mns$sav_magic <- list(
    # this is taken from https://www.garykessler.net/library/file_sigs.html and https://tool.lu/ja_JP/magicbytes/
    as.raw(c(0x24, 0x46, 0x4c, 0x32, 0x40, 0x28, 0x23, 0x29, 0x20, 
             0x53, 0x50, 0x53, 0x53, 0x20, 0x44, 0x41, 0x54, 0x41, 0x20, 0x46, 
             0x49, 0x4c, 0x45)),
    # this is taken from https://github.com/s0md3v/Dump/blob/master/static/file-signatures.json and https://www.filesignatures.net/index.php?search=sav&mode=EXT
    as.raw(c(0x24, 0x46, 0x4c, 0x32, 0x40, 0x28, 0x23, 0x29))
  )
  
  # this is taken from https://www.loc.gov/preservation/digital/formats/fdd/fdd000471.shtml
  mns$dta_magic <- as.raw(c(0x3c, 0x73, 0x74, 0x61, 0x74, 0x61, 0x5f, 0x64, 0x74, 
                        0x61, 0x3e, 0x3c, 0x68, 0x65, 0x61, 0x64, 0x65, 0x72, 0x3e, 0x3c, 
                        0x72, 0x65, 0x6c, 0x65, 0x61, 0x73, 0x65, 0x3e, 0x31, 0x31, 0x38, 
                        0x3c, 0x2f, 0x72, 0x65, 0x6c, 0x65, 0x61, 0x73, 0x65, 0x3e))
  
  ################
  # max length
  mns$mx_length <- mns %>% map_int(~if(is.list(.x)){map_int(.x, length) %>% max}else{length(.x)}) %>% max()
  
  mns
  
}

# save this magic_numbers for quicker access in tidycells_pkg_env
tidycells_pkg_env$magic_numbers <- this_domain_magic_numbers()
