skip("WIP")

skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("probabilistic bisection: n, indirect", {

options(power4mome.bz = TRUE)

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

# ==== n ====

out <- power4test(nrep = 50,
                  model = mod,
                  pop_es = mod_es,
                  n = 50,
                  R = 199,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE,
                                   test_method = "pvalue"),
                  iseed = 1234,
                  parallel = TRUE)
rejection_rates(out)

by_x_1 <- power4test_by_n(out,
                          n = 90)

## ==== Close enough ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  R = 199,
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

tmp_out <- power4test(
              out,
              n = ceiling(a_out$x_out),
              R = 1000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)

## ==== ub ====

set.seed(4321)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  R = 199,
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  goal = "close_enough",
                                  what = "ub",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

tmp_out <- power4test(
              out,
              n = ceiling(a_out$x_out),
              R = 1000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== lb ====

set.seed(4312)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  R = 199,
                                  by_x_1 = by_x_1,
                                  x_interval = c(100, 1000),
                                  goal = "close_enough",
                                  what = "lb",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

tmp_out <- power4test(
              out,
              n = ceiling(a_out$x_out),
              R = 1000,
              nrep = 2000,
              iseed = 3456)
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
m ~ x: l
y ~ m: m
y ~ x: s
"

out <- power4test(nrep = 50,
                  model = mod,
                  pop_es = mod_es,
                  n = 50,
                  R = 199,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE,
                                   test_method = "pvalue"),
                  iseed = 1234,
                  parallel = TRUE)
rejection_rates(out)

by_x_1 <- power4test_by_es(out,
                           pop_es_name = "y~m",
                           pop_es_values = c(.10))

## ==== Close enough ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 199,
                                  by_x_1 = by_x_1,
                                  x_interval = c(.00, .50),
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

(tmp_es <- setNames(a_out$x_out, "y~m"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 1000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== ub ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 199,
                                  by_x_1 = by_x_1,
                                  what = "ub",
                                  x_interval = c(.30, .60),
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

(tmp_es <- setNames(a_out$x_out, "y~m"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 1000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== lb ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 199,
                                  by_x_1 = by_x_1,
                                  what = "lb",
                                  x_interval = c(.30, .60),
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
pba_diag(a_out)

(tmp_es <- setNames(a_out$x_out, "y~m"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 2000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

})
