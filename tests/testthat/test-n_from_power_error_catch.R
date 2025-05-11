skip_on_cran()

library(testthat)

test_that("n_from_power", {

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
                  progress = FALSE)
out_power <- get_rejection_rates(out)
out_power

# Failed in the search

tmp <- n_from_power(out,
                    target_power = .90,
                    final_nrep = 20,
                    n_interval = c(10, 60),
                    extendInt = "no",
                    max_trials = 2,
                    seed = 1234,
                    progress = FALSE,
                    simulation_progress = FALSE)
expect_true(is.na(tmp$power_final))

})
