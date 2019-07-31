app <- ShinyDriver$new("../")
app$snapshotInit("crop")

app$setInputs(now_tab_main = "Parameters")
app$setInputs(`ui_plot_tune-adaptive_txt_size` = FALSE)
app$setInputs(`ui_plot_tune-txt_size` = 5.5)
app$setInputs(`ui_plot_tune-txt_alpha` = 0.55)
app$setInputs(`ui_plot_tune-fill` = "type")
app$snapshot()
app$setInputs(now_tab_main = "Crop")
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$setInputs(`ui_crop-data_del` = "click")
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$setInputs(`ui_crop-data_del` = "click")
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$setInputs(`ui_crop-data_crop` = "click")
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(`ui_crop-data_reset` = "click")
app$snapshot()
