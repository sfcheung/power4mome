skip_on_cran()

library(testthat)

test_that("probabilistic bisection: n", {

# ==== n ====

mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: s
y ~ m: m
y ~ x: s
"

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  parallel = FALSE,
                  progress = FALSE,
                  iseed = 1234)

by_x_1 <- power4test_by_n(out,
                          n = 90,
                          progress = FALSE)

## ==== Close enough ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(700, 1000),
                                  goal = "close_enough",
                                  final_nrep = 100,
                                  progress = FALSE,
                                  max_trials = 50,
                                  variants = list(initial_nrep = 50)
                                )

tmp_out <- power4test(
              out,
              n = a_out$x_out,
              nrep = 100,
              iseed = 147,
              progress = FALSE)
tmp <- rejection_rates(tmp_out)
expect_true((tmp$reject_ci_lo < .80) &&
            (tmp$reject_ci_hi > .80))

## ==== ub ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(500, 700),
                                  goal = "close_enough",
                                  what = "ub",
                                  final_nrep = 100,
                                  progress = FALSE,
                                  max_trials = 50,
                                  variants = list(initial_nrep = 50)
                                )
tmp_out <- power4test(
              out,
              n = a_out$x_out,
              nrep = 100,
              iseed = 2345,
              progress = FALSE)
tmp <- rejection_rates(tmp_out)
expect_true((tmp$reject_ci_lo < a_out$f_power) &&
            (tmp$reject_ci_hi > a_out$f_power))

## ==== lb ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(800, 1000),
                                  goal = "close_enough",
                                  what = "lb",
                                  final_nrep = 100,
                                  progress = FALSE,
                                  max_trials = 50,
                                  variants = list(initial_nrep = 50)
                                )
tmp_out <- power4test(
              out,
              n = a_out$x_out,
              nrep = 100,
              iseed = 2345,
              progress = FALSE)
tmp <- rejection_rates(tmp_out)
expect_true((tmp$reject_ci_lo < a_out$f_power) &&
            (tmp$reject_ci_hi > a_out$f_power))

})
