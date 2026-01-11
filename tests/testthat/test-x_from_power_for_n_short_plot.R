skip("Graphs: Test interactively")

library(testthat)

test_that("x_from_power: n region: plot", {

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

expect_no_error(tmp2 <- n_region_from_power(out,
                    target_power = .75,
                    final_nrep = 100,
                    seed = 234,
                    progress = FALSE,
                    simulation_progress = FALSE))

expect_no_error(plot(tmp2))

})
