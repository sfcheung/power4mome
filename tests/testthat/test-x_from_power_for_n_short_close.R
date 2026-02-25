skip_on_cran()

library(testthat)

test_that("x_from_power: n", {

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
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub"))
expect_no_error(capture.output(print(summary(tmp))))
expect_true(abs(tmp$ci_final - .80)[2] < set_tolerance(tmp$algorithm))

# Use x_from_power object as input

tmp2 <- x_from_power(tmp,
                    x = "n",
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub")
expect_identical(tmp2,
                 tmp)

tmp2b <- x_from_power(tmp,
                    x = "n",
                    target_power = .65,
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub")
expect_no_error(capture.output(print(summary(tmp))))
expect_true(abs(tmp$ci_final - .80)[2] < set_tolerance(tmp$algorithm))

# Should not run if final_nrep or ci_level changed

expect_error(x_from_power(tmp,
                    x = "n",
                    target_power = .80,
                    final_nrep = 70,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub"))
expect_error(x_from_power(tmp,
                    x = "n",
                    target_power = .80,
                    final_nrep = 60,
                    ci_level = .90,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub"))


tmp3 <- x_from_power(tmp$power4test_trials,
                    x = "n",
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub")
expect_identical(tmp3$x_tried,
                 tmp$x_tried)
expect_identical(tmp3$x_final,
                 tmp$x_final)

expect_no_error(tmp <- x_from_power(out,
                    x = "n",
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "lb"))
expect_no_error(capture.output(print(summary(tmp))))
expect_true(is.na(tmp$ci_final))

out <- power4test(nrep = 50,
                  model = mod,
                  pop_es = mod_es,
                  n = 150,
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
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "lb"))
expect_no_error(capture.output(print(summary(tmp))))
expect_true(abs(tmp$ci_final - .80)[1] < set_tolerance(tmp$algorithm))

})
