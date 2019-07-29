app <- ShinyDriver$new("../")
app$snapshotInit("omod")

app$setInputs(now_tab_main = "Parameters")
app$setInputs(`ui_plot_tune-txt_alpha` = 0.5)
app$setInputs(`ui_plot_tune-txt_alpha` = 0.6)
app$setInputs(`ui_plot_tune-txt_size` = 5.5)
app$setInputs(now_tab_main = "Orientation")
# Input '`ui_orientation_modification-brush_omod`' was set, but doesn't have an input binding.
# Input '`ui_orientation_modification-click_omod`' was set, but doesn't have an input binding.
app$setInputs(`ui_orientation_modification-new_direction` = "ENE", wait_=FALSE, values_=FALSE)
app$setInputs(`ui_orientation_modification-apply_direction` = "click", wait_=FALSE, values_=FALSE)
Sys.sleep(0.5)
app$snapshot()
# Input '`ui_orientation_modification-click_omod`' was set, but doesn't have an input binding.
app$setInputs(`ui_orientation_modification-new_direction` = "NNW", wait_=FALSE, values_=FALSE)
app$setInputs(`ui_orientation_modification-apply_direction` = "click", wait_=FALSE, values_=FALSE)
Sys.sleep(0.5)
# Input '`ui_orientation_modification-click_omod`' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(`ui_orientation_modification-new_direction` = "ABOVE", wait_=FALSE, values_=FALSE)
app$setInputs(`ui_orientation_modification-apply_direction` = "click", wait_=FALSE, values_=FALSE)
Sys.sleep(0.5)
app$snapshot()
app$setInputs(now_tab_main = "Data Block")
app$setInputs(`ui_data_block-block_boundary` = TRUE)
app$snapshot()
app$setInputs(now_tab_main = "Orientation")
# Input '`ui_orientation_modification-click_omod`' was set, but doesn't have an input binding.
# Input '`ui_orientation_modification-brush_omod`' was set, but doesn't have an input binding.
app$snapshot()
