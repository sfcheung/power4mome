library(testthat)
suppressMessages(library(lavaan))

test_that("fit_model: Update with new data", {

# Simple Mediation Model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

data_i1 <- sim_data(nrep = 2,
                    model = model_simple_med,
                    pop_es = model_simple_med_es,
                    n = 100,
                    iseed = 1234,
                    progress = !is_testing())
data_i2 <- sim_data(nrep = 2,
                    model = model_simple_med,
                    pop_es = model_simple_med_es,
                    n = 120,
                    iseed = 5678,
                    progress = !is_testing())

fit_i1 <- fit_model(data_i1)
fit_i2 <- fit_model(data_i2)
fit_i1_from_2 <- fit_model(data_i2,
                           fit_out = fit_i1)
expect_equal(coef(fit_i2[[2]]),
             coef(fit_i1_from_2[[2]]))

fit_i1_from_2_new_model <- fit_model(data_i2,
                                     model = c("m ~ x", "y ~ m"),
                                     fit_out = fit_i1,
                                     progress = !is_testing())
fit_i2_new_model <- fit_model(data_i2,
                              model = c("m ~ x", "y ~ m"),
                              progress = !is_testing())
expect_equal(coef(fit_i2_new_model[[2]]),
             coef(fit_i1_from_2_new_model[[2]]))

})
