skip_on_cran()

library(testthat)

test_that("bisection", {

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
                  progress = !is_testing(),
                  iseed = 1234)

by_x_1 <- power4test_by_n(out,
                          n = 90,
                          progress = !is_testing())

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   final_nrep = 20,
                                   x_interval = c(200, 2000),
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_false(.80 %in% range(a_out$ci_out))

# Close enough

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(600, 700),
                                   goal = "close_enough",
                                   tol = .07,
                                   final_nrep = 20,
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_true(abs(a_out$power_out - .80) < .07)

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(890, 950),
                                   extendInt = "yes",
                                   goal = "close_enough",
                                   tol = .05,
                                   max_trials = 3,
                                   final_nrep = 20,
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_true(abs(a_out$power_out - .80) < .05)

# ub

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   what = "ub",
                                   goal = "close_enough",
                                   final_nrep = 20,
                                   x_interval = c(100, 1000),
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_true(abs(a_out$ci_out[2] - .80) < .02)

# lb

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   what = "lb",
                                   goal = "close_enough",
                                   extendInt = "yes",
                                   final_nrep = 20,
                                   tol = .2,
                                   x_interval = c(100, 1000),
                                   max_trials = 3,
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_true(abs(a_out$ci_out[1] - .80) < .2)

# Solution already in interval

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(775, 800),
                                   final_nrep = 20,
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_false(.80 %in% range(a_out$ci_out))

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(600, 775),
                                   final_nrep = 20,
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_false(.80 %in% range(a_out$ci_out))

####### es

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  progress = !is_testing(),
                  iseed = 1234)

by_x_1 <- power4test_by_es(out,
                           pop_es_name = "m~x",
                           pop_es_values = c(.10),
                           progress = !is_testing())

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   x_interval = c(0, .50),
                                   final_nrep = 20,
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_false(.80 %in% range(a_out$ci_out))

# Close enough

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   goal = "close_enough",
                                   tol = .10,
                                   x_interval = c(0, .50),
                                   final_nrep = 20,
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_true(abs(a_out$power_out - .80) < .10)

# ub

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   what = "ub",
                                   goal = "close_enough",
                                   x_interval = c(0, .50),
                                   tol = .05,
                                   final_nrep = 20,
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_true(abs(a_out$ci_out[2] - .80) < .02)

# lb

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   what = "lb",
                                   goal = "close_enough",
                                   x_interval = c(0, .50),
                                   tol = .10,
                                   final_nrep = 20,
                                   progress = !is_testing(),
                                   simulation_progress = !is_testing())
rejection_rates(a_out$by_x_1)
expect_true(abs(a_out$ci_out[1] - .80) < .10)

})
