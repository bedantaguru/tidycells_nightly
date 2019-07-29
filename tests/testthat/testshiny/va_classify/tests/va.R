app <- ShinyDriver$new("../")
app$snapshotInit("va")

app$setInputs(now_tab_main = "Parameters")
app$setInputs(`ui_plot_tune-adaptive_txt_size` = FALSE)
app$setInputs(`ui_plot_tune-txt_size` = 4.5)
app$setInputs(`ui_plot_tune-txt_size` = 5.5)
app$setInputs(now_tab_main = "Crop")
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$setInputs(`ui_crop-data_del` = "click")
# Input '`ui_crop-brush_crop`' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(now_tab_main = "Classify")
# Input '`ui_va_classify-brush_va_classify`' was set, but doesn't have an input binding.
app$setInputs(`ui_va_classify-make_value_va_classify` = "click")
# Input '`ui_va_classify-brush_va_classify`' was set, but doesn't have an input binding.
app$snapshot()
# Input '`ui_va_classify-brush_va_classify`' was set, but doesn't have an input binding.
app$setInputs(`ui_va_classify-make_attr_va_classify` = "click")
# Input '`ui_va_classify-brush_va_classify`' was set, but doesn't have an input binding.
# Input '`ui_va_classify-brush_va_classify`' was set, but doesn't have an input binding.
app$setInputs(`ui_va_classify-make_attr_va_classify` = "click")
# Input '`ui_va_classify-brush_va_classify`' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(`ui_va_classify-reset_va_classify` = "click")
app$setInputs(`ui_va_classify-reset_va_classify` = "click")
app$snapshot()
