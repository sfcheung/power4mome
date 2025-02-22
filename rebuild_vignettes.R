# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("power4mome.Rmd.originaL", output = "power4mome.Rmd")

setwd(base_dir)

# For articles

base_dir <- getwd()

setwd("vignettes/articles")

setwd(base_dir)
