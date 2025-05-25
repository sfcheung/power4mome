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
                  progress = FALSE)
out_power <- rejection_rates(out)
out_power

expect_no_error(tmp <- x_from_power(out,
                    x = "n",
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 1234,
                    progress = TRUE,
                    simulation_progress = FALSE))
expect_no_error(print(summary(tmp)))
expect_true((tmp$power_final > .70) &&
            (tmp$power_final < .90))

# Should invoke OLS

out <- power4test(nrep = 10,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = "y~m"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = FALSE)
out_power <- rejection_rates(out)
out_power

expect_no_error(tmp <- x_from_power(out,
                    x = "n",
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 1234,
                    progress = TRUE,
                    simulation_progress = FALSE))
expect_no_error(print(summary(tmp)))
expect_true((tmp$power_final > .60) &&
            (tmp$power_final < .90))
})
