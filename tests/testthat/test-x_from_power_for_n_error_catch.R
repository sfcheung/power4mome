skip_on_cran()

library(testthat)

test_that("x_from_power: n", {

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
                  progress = FALSE)
out_power <- rejection_rates(out)
out_power

# Failed in the search

tmp <- x_from_power(out,
                    x = "n",
                    target_power = .90,
                    final_nrep = 50,
                    x_interval = c(10, 35),
                    extendInt = "no",
                    max_trials = 2,
                    seed = 1234,
                    progress = FALSE,
                    simulation_progress = FALSE,
                    algorithm = "power_curve")
expect_no_error(print(summary(tmp)))
expect_true(is.na(tmp$power_final))
expect_true(is.na(tmp$x_final))

# Can extend the search

tmp <- x_from_power(out,
                    x = "n",
                    target_power = .90,
                    final_nrep = 50,
                    x_interval = c(10, 60),
                    extendInt = "yes",
                    max_trials = 3,
                    seed = 12,
                    progress = FALSE,
                    simulation_progress = FALSE,
                    algorithm = "power_curve")
expect_no_error(print(summary(tmp)))
expect_true(!is.na(tmp$power_final))
expect_true(!is.na(tmp$x_final))

# Can extend the search

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 110,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = "y~m"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = FALSE)
out_power <- rejection_rates(out)
out_power

tmp <- x_from_power(out,
                    x = "n",
                    target_power = .50,
                    final_nrep = 20,
                    x_interval = c(100, 120),
                    extendInt = "yes",
                    max_trials = 2,
                    seed = 12345,
                    progress = FALSE,
                    simulation_progress = FALSE,
                    algorithm = "power_curve")
expect_no_error(print(summary(tmp)))
expect_true(!is.na(tmp$power_final))
expect_true(!is.na(tmp$x_final))
})
