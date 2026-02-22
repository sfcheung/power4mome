skip("WIP")

skip("A long test with parallel processing. Test interactively.")

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
                  iseed = 1234)

by_x_1 <- power4test_by_n(out,
                          n = 90)

## ==== Close enough ====

set.seed(4321)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

tmp_out <- power4test(
              out,
              n = a_out$x_out,
              nrep = 2000,
              iseed = 147)
rejection_rates(tmp_out)

## ==== ub ====

set.seed(2345)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  what = "ub",
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

tmp_out <- power4test(
              out,
              n = a_out$x_out,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== lb ====

set.seed(271828)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  what = "lb",
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

tmp_out <- power4test(
              out,
              n = a_out$x_out,
              nrep = 2000,
              iseed = 12345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

# Solution already in interval

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(775, 800))
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(600, 775))
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)


# ==== es ====

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
                  iseed = 1234)

by_x_1 <- power4test_by_es(out,
                           pop_es_name = "m~x",
                           pop_es_values = c(.10))

## ==== Close enough ====

set.seed(345)
a_out <- power_algorithm_prob_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   goal = "close_enough",
                                   x_interval = c(.10, .40),
                                   final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

(tmp_es <- setNames(a_out$x_out, "m~x"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== ub ====

set.seed(3579)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "m~x",
                                  by_x_1 = by_x_1,
                                  what = "ub",
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

(tmp_es <- setNames(a_out$x_out, "m~x"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== lb ====

set.seed(123456)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "m~x",
                                  by_x_1 = by_x_1,
                                  x_interval = c(.10, .50),
                                  what = "lb",
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

(tmp_es <- setNames(a_out$x_out, "m~x"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

})
