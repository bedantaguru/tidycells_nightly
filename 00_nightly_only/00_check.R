

## LIB ##
source("00_nightly_only/00_check.lib.R")

check_dev_mark()
# ("tidycells" "datasets") are ok
# also these are ok ("pkg" ".x" ) but do check
this_pkg_deps()


TF_replace()
quick_spell_check()
pdf_ok()
