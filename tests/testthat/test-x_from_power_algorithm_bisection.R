skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("bisection: n", {

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

####### n

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  iseed = 1234)

by_x_1 <- power4test_by_n(out,
                          n = 90)

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1)
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

# Close enough

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(600, 700),
                                   goal = "close_enough",
                                   tol = .05)
a_out$solution_found
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(600, 700),
                                   extendInt = "yes",
                                   goal = "close_enough",
                                   tol = .05)
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)


# ub

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   what = "ub",
                                   goal = "close_enough")
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

# lb

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   what = "lb",
                                   goal = "close_enough")
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

# Solution already in interval

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(775, 800))
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(600, 775))
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)


####### es

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

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1)
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

# Close enough

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   goal = "close_enough",
                                   tol = .30)
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

# ub

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   what = "ub",
                                   goal = "close_enough")
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)


# lb

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   what = "lb",
                                   goal = "close_enough")
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

# Solution already in interval

})
