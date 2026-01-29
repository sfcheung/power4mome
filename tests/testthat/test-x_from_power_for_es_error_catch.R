skip_on_cran()

library(testthat)

test_that("x_from_power: es", {

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
                  n = 30,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = "y~m"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = !is_testing())
out_power <- rejection_rates(out)
out_power

# Failed in the search

tmp <- x_from_power(out,
                    x = "es",
                    pop_es_name = "y ~ m",
                    target_power = .90,
                    final_nrep = 50,
                    x_interval = c(.0, .4),
                    extendInt = "no",
                    max_trials = 2,
                    seed = 1234,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    algorithm = "power_curve")
expect_no_error(capture.output(print(summary(tmp))))
expect_true(is.na(tmp$power_final))
expect_true(is.na(tmp$x_final))

# Can extend the search

tmp <- x_from_power(out,
                    x = "es",
                    pop_es_name = "y ~ m",
                    target_power = .90,
                    final_nrep = 50,
                    x_interval = c(.0, .4),
                    extendInt = "yes",
                    max_trials = 3,
                    seed = 12,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    algorithm = "power_curve")
expect_no_error(capture.output(print(summary(tmp))))
expect_true(!is.na(tmp$power_final))
expect_true(!is.na(tmp$x_final))

# Can extend the search

mod_es <-
"
m ~ x: s
y ~ m: l
y ~ x: s
"

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 110,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = "y~m"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = !is_testing())
out_power <- rejection_rates(out)
out_power

tmp <- x_from_power(out,
                    x = "es",
                    pop_es_name = "y ~ m",
                    target_power = .20,
                    final_nrep = 50,
                    x_interval = c(.4, .6),
                    extendInt = "yes",
                    max_trials = 3,
                    seed = 12,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    algorithm = "power_curve")
expect_no_error(capture.output(print(summary(tmp))))
expect_true(!is.na(tmp$power_final))
expect_true(!is.na(tmp$x_final))
})
