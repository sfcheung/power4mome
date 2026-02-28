skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("probabilistic bisection: n: reuse trials", {

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
pba_diag(a_out)
plot(a_out$time_passed_history, type = "l")

a_out_by_x_1 <- a_out$by_x_1
names(a_out_by_x_1)

## ==== All settings identical ====

set.seed(4321)
a_out2 <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = a_out_by_x_1,
                                  x_interval = c(721, 800),
                                  goal = "close_enough",
                                  final_nrep = 2000)
tryCatch(pba_diag(a_out2),
         error = function(e) e)

# The solution is in the bound, but
# only the trial with nrep == initial_nrep will be used.

set.seed(4321)
a_out2b <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = a_out_by_x_1,
                                  x_interval = c(795, 2000),
                                  goal = "close_enough",
                                  final_nrep = 2000)
tryCatch(pba_diag(a_out2b),
         error = function(e) e)

## ==== Different final_nrep ====

set.seed(4321)
a_out3 <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = a_out_by_x_1,
                                  x_interval = c(50, 2000),
                                  goal = "close_enough",
                                  final_nrep = 1000)
a_out3$x_out
tryCatch(pba_diag(a_out3),
         error = function(e) e)

## ==== Reuse outputs ====

set.seed(3)
a_out4 <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = a_out_by_x_1,
                                  x_interval = c(50, 2000),
                                  goal = "close_enough",
                                  final_nrep = 2000)
a_out4$x_out
tryCatch(pba_diag(a_out4),
         error = function(e) e)
plot(a_out4$time_passed_history, type = "l")

expect_equal(attr(a_out4$by_x_1[[a_out4$i2]], "args")$iseed,
             attr(a_out$by_x_1[[a_out$i2]], "args")$iseed)

})
