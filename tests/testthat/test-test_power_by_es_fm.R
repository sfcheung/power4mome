skip_on_cran()

library(testthat)

skip_if_not_installed("lmhelprs")

test_that("Power by es: .fm.", {

model_simple_med <-
"
m ~ x
y ~ m
"

# .rmsea.

model_simple_med_es <- c("m ~ x" = "s",
                         "y ~ m" = "s",
                         ".fm.(rmsea)" = .08)

sim_only <- power4test(nrep = 2,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       do_the_test = FALSE,
                       parallel = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

if (!is_testing()) {
print(sim_only,
      data_long = TRUE)
}

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    fit_measure = "rmsea"
  )
)

out <- power4test_by_es(
  test_out1,
  pop_es_name = ".fm.(rmsea)",
  pop_es_values = c(.10, .12),
  by_seed = 1357,
  progress = !is_testing()
)
out_reject <- rejection_rates(out)

expect_equal(
  out_reject$es,
  c(.10, .12)
)

# .cfi

model_simple_med <-
"
m1 ~ x
m2 ~ m1
y ~ m2
"

model_simple_med_es <- c("m1 ~ x" = "s",
                         "m2 ~ m1" = "s",
                         "y ~ m2" = "s",
                         ".fm.(cfi)" = .90)

sim_only <- power4test(nrep = 2,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       do_the_test = FALSE,
                       parallel = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

if (!is_testing()) {
print(sim_only,
      data_long = TRUE)
}

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    fit_measure = "cfi"
  )
)

out <- power4test_by_es(
  test_out1,
  pop_es_name = ".fm.(cfi)",
  pop_es_values = c(.91, .92),
  by_seed = 1357,
  progress = !is_testing()
)
out_reject <- rejection_rates(out)

expect_equal(
  out_reject$es,
  c(.91, .92)
)

})
