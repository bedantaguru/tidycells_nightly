
dir <- tempfile()
dir.create(dir)
htmlFile <- file.path(dir, "index.html")

writeLines("
<html>
<header><title>This is title</title></header>
<body>
Hello world
</body>
</html>
           ",htmlFile)


rstudioapi::viewer(htmlFile)

