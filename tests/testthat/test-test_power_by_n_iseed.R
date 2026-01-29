library(testthat)

skip_if_not_installed("lmhelprs")

test_that("Power by n: by_seed", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 50,
                       R = 50,
                       ci_type = "boot",
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       parallel = FALSE,
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE,
                                        mc_ci = FALSE))

out1 <- power4test_by_n(test_out,
                        n = c(100, 110),
                        by_seed = 1234,
                        progress = !is_testing())
out1_reject <- rejection_rates(out1)
out1_reject

out2 <- power4test_by_n(test_out,
                        n = c(100, 110),
                        by_seed = 1234,
                        progress = !is_testing())
out2_reject <- rejection_rates(out2)
out2_reject

expect_identical(out1_reject,
                 out2_reject)

})
