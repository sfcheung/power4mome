skip_on_cran()

library(testthat)

test_that("x_from_power: n region", {

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

out <- power4test(out,
                  test_fun = test_parameters,
                  test_args = list(pars = "y~x"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = !is_testing())
out_power <- rejection_rates(out)
out_power

expect_no_error(tmp <- n_region_from_power(out,
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    algorithm = "bisection"))
expect_true(all(grepl("all_sig", tmp$below$rejection_rates$test_label)))
expect_true(all(grepl("all_sig", tmp$above$rejection_rates$test_label)))

expect_no_error(tmp <- n_region_from_power(out,
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 2,
                    seed = 2345,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    algorithm = "bisection",
                    rejection_rates_args = list(collapse = "at_least_one_sig")))
expect_true(all(grepl("one_sig", tmp$below$rejection_rates$test_label)))
expect_true(all(grepl("one_sig", tmp$above$rejection_rates$test_label)))

})
