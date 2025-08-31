# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd(paste0(base_dir, "/vignettes/"))

knitr::knit("power4mome.Rmd.original", output = "power4mome.Rmd", envir = new.env())
knitr::knit("power4test_latent_mediation.Rmd.original", output = "power4test_latent_mediation.Rmd", envir = new.env())
pkgdown::build_articles("..")

setwd(base_dir)

# For articles

setwd(paste0(base_dir, "/vignettes/articles/"))

knitr::knit("x_from_power_for_n.Rmd.original", output = "x_from_power_for_n.Rmd", envir = new.env())
knitr::knit("x_from_power_for_es.Rmd.original", output = "x_from_power_for_es.Rmd", envir = new.env())

knitr::knit("template_n_from_power_mediation_obs_simple.Rmd.original", output = "template_n_from_power_mediation_obs_simple.Rmd", envir = new.env())
knitr::knit("template_n_from_power_mediation_obs_serial.Rmd.original", output = "template_n_from_power_mediation_obs_serial.Rmd", envir = new.env())
knitr::knit("template_n_from_power_mediation_obs_parallel.Rmd.original", output = "template_n_from_power_mediation_obs_parallel.Rmd", envir = new.env())

knitr::knit("template_n_from_power_mediation_lav_simple.Rmd.original", output = "template_n_from_power_mediation_lav_simple.Rmd", envir = new.env())
knitr::knit("template_n_from_power_mediation_lav_serial.Rmd.original", output = "template_n_from_power_mediation_lav_serial.Rmd", envir = new.env())
knitr::knit("template_n_from_power_mediation_lav_parallel.Rmd.original", output = "template_n_from_power_mediation_lav_parallel.Rmd", envir = new.env())

knitr::knit("template_n_from_power_moderation_obs_simple.Rmd.original", output = "template_n_from_power_moderation_obs_simple.Rmd", envir = new.env())
knitr::knit("template_n_from_power_moderation_obs_two_ws.Rmd.original", output = "template_n_from_power_moderation_obs_two_ws.Rmd", envir = new.env())

knitr::knit("template_n_from_power_mome_obs_a.Rmd.original", output = "template_n_from_power_mome_obs_a.Rmd", envir = new.env())
knitr::knit("template_n_from_power_mome_obs_b.Rmd.original", output = "template_n_from_power_mome_obs_b.Rmd", envir = new.env())


# knitr::knit("template_mediation_obs.Rmd.original", output = "template_mediation_obs.Rmd", envir = new.env())
# knitr::knit("template_mediation_obs_serial.Rmd.original", output = "template_mediation_obs_serial.Rmd", envir = new.env())
# knitr::knit("template_mediation_obs_ind.Rmd.original", output = "template_mediation_obs_ind.Rmd", envir = new.env())
pkgdown::build_articles("../..")

setwd(base_dir)
