skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("sim_out and do_test", {

mod <-
"
y ~ x + w1 + x:w1
"


es <-
"
y ~ x:w1: m
y ~ x: s
y ~ w1: m
"

data_all <- sim_data(nrep = 2,
                     model = mod,
                     pop_es = es,
                     n = 100,
                     progress = !is_testing(),
                     iseed = 1234)
fit_all <- fit_model(data_all)
sim_all <- sim_out(data_all = data_all,
                   fit = fit_all)
tmp <- capture.output(print(sim_all))
expect_true(any(grepl("[w1]", tmp, fixed = TRUE)))
expect_true(any(grepl("[x]", tmp, fixed = TRUE)))

})
