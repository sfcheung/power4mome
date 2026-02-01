skip_on_cran()

library(testthat)

test_that("x_from_power: n: power_curve", {

# Case 1

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

out <- power4test(nrep = 50,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = "y~m"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = !is_testing())
out_power <- rejection_rates(out)
out_power

expect_no_error(tmp <- x_from_power(out,
                    x = "n",
                    target_power = .80,
                    tolerance = .10,
                    final_nrep = 50,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "lb",
                    goal = "close_enough",
                    algorithm = "power_curve",
                    internal_args = list(keep_algorithm = TRUE)))
expect_no_error(capture.output(print(summary(tmp))))
expect_true(abs(tmp$ci_final[1] - .80) < tmp$technical$tol)

expect_no_error(tmp <- x_from_power(out,
                    x = "n",
                    target_power = .80,
                    tolerance = .10,
                    final_nrep = 50,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub",
                    goal = "close_enough",
                    algorithm = "power_curve",
                    internal_args = list(keep_algorithm = TRUE)))
expect_no_error(capture.output(print(summary(tmp))))
expect_true(abs(tmp$ci_final[2] - .80) < tmp$technical$tol)

# Use x_from_power object as input

tmp2 <- x_from_power(tmp,
                    x = "n",
                    target_power = .80,
                    tolerance = .10,
                    final_nrep = 50,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub",
                    goal = "close_enough",
                    algorithm = "power_curve",
                    internal_args = list(keep_algorithm = TRUE))
expect_identical(tmp2,
                 tmp)

tmp2b <- x_from_power(tmp,
                    x = "n",
                    target_power = .65,
                    tolerance = .10,
                    final_nrep = 50,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub",
                    goal = "close_enough",
                    algorithm = "power_curve",
                    internal_args = list(keep_algorithm = TRUE))
expect_no_error(capture.output(print(summary(tmp2b))))
expect_true(abs(tmp2b$ci_final[2] - tmp2b$target_power) < tmp2b$technical$tol)

# Should not run if final_nrep or ci_level changed

expect_error(x_from_power(tmp,
                    x = "n",
                    target_power = .80,
                    tolerance = .10,
                    final_nrep = 70,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub",
                    goal = "close_enough",
                    algorithm = "power_curve",
                    internal_args = list(keep_algorithm = TRUE)))
expect_error(x_from_power(tmp,
                    x = "n",
                    target_power = .80,
                    tolerance = .10,
                    final_nrep = 60,
                    ci_level = .90,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub",
                    goal = "close_enough",
                    algorithm = "power_curve",
                    internal_args = list(keep_algorithm = TRUE)))


tmp3 <- x_from_power(tmp$power4test_trials,
                    x = "n",
                    target_power = .80,
                    tolerance = .10,
                    final_nrep = 50,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub",
                    goal = "close_enough",
                    algorithm = "power_curve",
                    internal_args = list(keep_algorithm = TRUE))
expect_identical(tmp3$x_tried,
                 tmp$x_tried)
expect_identical(tmp3$x_final,
                 tmp$x_final)

})
