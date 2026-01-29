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

out <- power4test_by_n(out,
                       n = c(80, 85),
                       parallel = FALSE,
                       progress = !is_testing(),
                       by_seed = 2345)
rejection_rates(out)

expect_no_error(tmp <- x_from_power(out,
                    x = "n",
                    target_power = .80,
                    final_nrep = 55,
                    max_trials = 1,
                    seed = 1234,
                    progress = !is_testing(),
                    simulation_progress = !is_testing(),
                    algorithm = "power_curve"))
expect_true(all(c(80, 85) %in% tmp$x_tried))
})
