skip_on_cran()

library(testthat)

test_that("x_from_power: use by_es", {

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
                    x = "es",
                    pop_es_name = "y ~ m",
                    target_power = .80,
                    final_nrep = 50,
                    max_trials = 1,
                    seed = 1234,
                    progress = TRUE,
                    simulation_progress = FALSE))

expect_no_error(tmp2 <- x_from_power(tmp,
                    x = "es",
                    pop_es_name = "y ~ m",
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 1,
                    seed = 1234,
                    progress = TRUE,
                    simulation_progress = FALSE))

expect_true(all(tmp$x_tried %in% tmp2$x_tried))

expect_error(tmp2 <- x_from_power(tmp,
                    x = "es",
                    pop_es_name = "x ~ m",
                    target_power = .80,
                    final_nrep = 60,
                    max_trials = 1,
                    seed = 1234,
                    progress = TRUE,
                    simulation_progress = FALSE),
            "but requested pop_es_name")

})
