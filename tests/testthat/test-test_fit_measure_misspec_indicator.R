skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("test_fit_measure: Misspec: Indictors", {

model_simple_med <-
"
m ~ x
y ~ m
"

# ---- RMSEA ----

model_simple_med_es <-
"
y ~ m: s
m ~ x: s
.fm.(rmsea): .12
"

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 500,
                       number_of_indicators = c(
                          y = 3,
                          m = 4
                        ),
                       reliability = c(
                          m = .80,
                          y = .70
                       ),
                       progress = !is_testing(),
                       iseed = 1234)

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    fit_measure = "rmsea"
  )
)

tmp1 <- rejection_rates(test_out1)

# No error, though misfit will be masked by the measurement part.

expect_lt(
  tmp1$est,
  .12
)

# ---- CFI ----

model_simple_med_es <-
"
y ~ m: s
m ~ x: s
.fm.(cfi): .90
"

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 5000,
                       number_of_indicators = c(
                          y = 3,
                          m = 4
                        ),
                       reliability = c(
                          m = .80,
                          y = .70
                       ),
                       progress = !is_testing(),
                       iseed = 1234)

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    fit_measure = "cfi",
    sig_if = "<.95"
  )
)

tmp1 <- rejection_rates(test_out1)

# No error, though misfit will be masked by the measurement part.

expect_gt(
  tmp1$est,
  .95
)

})
