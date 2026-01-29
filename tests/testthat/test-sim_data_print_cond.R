skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("sim_out and do_test", {

mod <-
"
m1 ~ x + w1 + x:w1
m2 ~ x
m3 ~ x + w4 + x:w4
y ~ m1 + x + w2 + w3 + m1:w2 + m2 + m1:w3 + m3 + w5 + x:w5
"


es <-
"
m3 ~ x:w4: s
m1 ~ x: m
m1 ~ w1: s
m1 ~ x:w1: l
y ~ m1:w2: m
y ~ m1:w3: s
y ~ m1: l
y ~ x: s
y ~ x:w5: m
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
expect_true(any(grepl("x, w2, w3", tmp, fixed = TRUE)))
expect_true(any(grepl("w1, w2, w3", tmp, fixed = TRUE)))
expect_true(any(grepl("[w4]", tmp, fixed = TRUE)))
expect_true(any(grepl("[w5]", tmp, fixed = TRUE)))

})
