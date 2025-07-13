# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd(paste0(base_dir, "/vignettes/"))

knitr::knit("power4mome.Rmd.original", output = "power4mome.Rmd")
knitr::knit("power4test_latent_mediation.Rmd.original", output = "power4test_latent_mediation.Rmd")
pkgdown::build_articles("..")

setwd(base_dir)

# For articles

setwd(paste0(base_dir, "/vignettes/articles/"))

knitr::knit("x_from_power_for_n.Rmd.original", output = "x_from_power_for_n.Rmd")
knitr::knit("x_from_power_for_es.Rmd.original", output = "x_from_power_for_es.Rmd")
# knitr::knit("template_mediation_obs.Rmd.original", output = "template_mediation_obs.Rmd")
# knitr::knit("template_mediation_obs_serial.Rmd.original", output = "template_mediation_obs_serial.Rmd")
knitr::knit("template_n_from_power_mediation_obs.Rmd.original", output = "template_n_from_power_mediation_obs.Rmd")
knitr::knit("template_n_from_power_mediation_obs_serial.Rmd.original", output = "template_n_from_power_mediation_obs_serial.Rmd")
# knitr::knit("template_mediation_obs_ind.Rmd.original", output = "template_mediation_obs_ind.Rmd")
pkgdown::build_articles("../..")

setwd(base_dir)
