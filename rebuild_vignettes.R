# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("power4mome.Rmd.original", output = "power4mome.Rmd")

setwd(base_dir)

# For articles

base_dir <- getwd()

setwd("vignettes/articles")
knitr::knit("x_from_power_for_n.Rmd.original", output = "x_from_power_for_n.Rmd")

setwd(base_dir)
