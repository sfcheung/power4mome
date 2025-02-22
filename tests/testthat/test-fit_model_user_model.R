library(testthat)
suppressMessages(library(lavaan))

test_that("fit_model: User models", {

# Simple Mediation Model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

data_i <- sim_data_i(model = model_simple_med,
                     pop_es = model_simple_med_es,
                     n = 500,
                     seed = 1234)

fit_i <- fit_model_i(data_i,
                     model = c("m ~ x", "y ~ m"))
expect_equal(fitMeasures(fit_i, "df"),
             1,
             ignore_attr = TRUE)
})
