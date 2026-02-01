skip_on_cran()

library(testthat)

test_that("x_from_power: es: power_curve", {

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
                  n = 50,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = "y~m"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = !is_testing())
out_power <- rejection_rates(out)
out_power

expect_no_error(tmp <- x_from_power(out,
                    x = "es",
                    pop_es_name = "m ~ x",
                    target_power = .70,
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
expect_true(abs(tmp$ci_final[2] - tmp$target_power) < tmp$technical$tol)

tmp2 <- x_from_power(tmp,
                    x = "es",
                    pop_es_name = "m ~ x",
                    target_power = .70,
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

tmp3 <- x_from_power(tmp$power4test_trials,
                    x = "es",
                    pop_es_name = "m ~ x",
                    target_power = .70,
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
expect_true(all.equal(tmp3$x_tried,
                      tmp$x_tried))
expect_true(all.equal(tmp3$x_final,
                      tmp$x_final))

expect_no_error(tmp <- x_from_power(out,
                    x = "es",
                    pop_es_name = "m ~ x",
                    target_power = .60,
                    tolerance = .10,
                    final_nrep = 100,
                    max_trials = 2,
                    seed = 23456,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "ub",
                    goal = "close_enough",
                    algorithm = "power_curve",
                    internal_args = list(keep_algorithm = TRUE)))
expect_no_error(capture.output(print(summary(tmp))))
expect_true(abs(tmp$ci_final[2] - tmp$target_power) < tmp$technical$tol)

expect_no_error(tmp <- x_from_power(out,
                    x = "es",
                    pop_es_name = "m ~ x",
                    target_power = .50,
                    tolerance = .10,
                    final_nrep = 100,
                    max_trials = 2,
                    seed = 23456,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    what = "lb",
                    goal = "close_enough",
                    algorithm = "power_curve",
                    internal_args = list(keep_algorithm = TRUE)))
expect_no_error(capture.output(print(summary(tmp))))
expect_true(abs(tmp$ci_final[1] - tmp$target_power) < tmp$technical$tol)


})
