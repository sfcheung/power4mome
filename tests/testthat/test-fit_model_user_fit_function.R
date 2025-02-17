library(testthat)
suppressMessages(library(lavaan))
library(manymome)

skip_if_not_installed(lmhelprs)

test_that("fit_model: User fit function", {

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
                     model = c("m ~ x", "y ~ m"),
                     fit_function = lmhelprs::many_lm)

expect_s3_class(fit_i,
                "lm_list_lmhelprs")
lm_m <- lm(m ~ x,
           data_i$mm_lm_dat_out)
lm_y <- lm(y ~ m,
           data_i$mm_lm_dat_out)
expect_equal(coef(fit_i[[2]]),
             coef(lm_y))

ind <- indirect_effect(fit = fit_i,
                       x = "x",
                       m = "m",
                       y = "y")
expect_equal(unname(coef(ind)),
             unname(coef(lm_m)["x"] * coef(lm_y)["m"]))

})
