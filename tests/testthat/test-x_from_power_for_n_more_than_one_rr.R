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

out <- power4test(nrep = 5,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = c("y~m", "y~x")),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = !is_testing())
out_power <- rejection_rates(out)
out_power

expect_error(tmp <- x_from_power(out,
                    x = "n",
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    algorithm = "power_curve"))

# No error if collapsed to one test

out <- power4test(nrep = 5,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = c("y~m", "y~x"),
                                   omnibus = "all_sig"),
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
                    algorithm = "power_curve"))

})
