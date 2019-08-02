

# shinytest:::sd_startShiny
# 
# shinytest::ShinyDriver 
# 

require(covr)
require(shinytest)

testApp("00_nightly_only/shiny_test/exapp/", testnames = "crop")



covr::package_coverage()

