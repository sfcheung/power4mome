skip_on_cran()

library(testthat)

test_that("iseed in parallel", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es1 <- c("y ~ m" = "l",
                          "m ~ x" = "m",
                          "y ~ x" = "n")

sim_only1 <- power4test(nrep = 2,
                        model = model_simple_med,
                        pop_es = model_simple_med_es1,
                        n = 100,
                        R = 50,
                        ci_type = "mc",
                        fit_model_args = list(fit_function = "lavaan"),
                        do_the_test = FALSE,
                        progress = FALSE,
                        parallel = TRUE,
                        ncores = 2,
                        iseed = 1234)

sim_only2 <- power4test(nrep = 2,
                        model = model_simple_med,
                        pop_es = model_simple_med_es1,
                        n = 100,
                        R = 50,
                        ci_type = "mc",
                        fit_model_args = list(fit_function = "lavaan"),
                        do_the_test = FALSE,
                        progress = FALSE,
                        parallel = TRUE,
                        ncores = 2,
                        iseed = 1234)

expect_equal(sim_only1$sim_all[[1]]$extra$mc_out[[1]]$est,
             sim_only2$sim_all[[1]]$extra$mc_out[[1]]$est)

sim_only1 <- power4test(nrep = 2,
                        model = model_simple_med,
                        pop_es = model_simple_med_es1,
                        n = 100,
                        R = 50,
                        ci_type = "boot",
                        fit_model_args = list(fit_function = "lm"),
                        do_the_test = FALSE,
                        progress = FALSE,
                        parallel = TRUE,
                        ncores = 2,
                        iseed = 1234)

sim_only2 <- power4test(nrep = 2,
                        model = model_simple_med,
                        pop_es = model_simple_med_es1,
                        n = 100,
                        R = 50,
                        ci_type = "boot",
                        fit_model_args = list(fit_function = "lm"),
                        do_the_test = FALSE,
                        progress = FALSE,
                        parallel = TRUE,
                        ncores = 2,
                        iseed = 1234)

expect_equal(sim_only1$sim_all[[1]]$extra$boot_out[[1]]$est,
             sim_only2$sim_all[[1]]$extra$boot_out[[1]]$est)

})
