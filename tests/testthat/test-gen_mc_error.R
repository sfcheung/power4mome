library(testthat)
suppressMessages(library(lavaan))

test_that("gen_mc: Error and warning catching", {

# Simple Mediation Model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = ".30",
                         "m ~ x" = ".40",
                         "y ~ x" = ".10")

data_i <- sim_data(nrep = 3,
                   model = model_simple_med,
                   pop_es = model_simple_med_es,
                   n = 50,
                   iseed = 1234,
                   parallel = FALSE,
                   progress = !is_testing())

# Add an error in fit
data_i[[2]]$model_final <- "y1 ~ x1"

fit_out <- fit_model(data_i,
                     parallel = FALSE,
                     progress = !is_testing())

mc_out <- gen_mc(fit_out,
                 R = 100,
                 seed = 1234)
expect_s3_class(mc_out[[2]],
                "error")

})
