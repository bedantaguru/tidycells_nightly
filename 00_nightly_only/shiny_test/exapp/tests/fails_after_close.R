app <- ShinyDriver$new("../")
app$snapshotInit("fails_after_close")

# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$setInputs(`ui_crop-data_del` = "click")
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$setInputs(now_tab_main = "Parameters")
app$setInputs(`ui_plot_tune-adaptive_txt_size` = FALSE)
app$setInputs(`ui_plot_tune-txt_size` = 6.5)
app$setInputs(`ui_plot_tune-fill` = "type")
app$setInputs(now_tab_main = "Crop")
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$setInputs(`ui_crop-data_crop` = "click")
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$snapshot()
# it breaks here
app$setInputs(done = "click")
# Running fails_after_close.R Error in curl::curl_fetch_memory(url, handle = handle) : 
#   Failed to connect to 127.0.0.1 port 8695: Connection refused
app$snapshot()
