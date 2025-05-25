library(testthat)

skip_if_not_installed("lmhelprs")

test_that("Power by n", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 3,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 50,
                       R = 50,
                       ci_type = "boot",
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234)

out1 <- power4test_by_n(sim_only,
                        n = c(100, 300),
                        test_fun = test_indirect_effect,
                        test_args = list(x = "x",
                                          m = "m",
                                          y = "y",
                                          boot_ci = TRUE,
                                          mc_ci = FALSE),
                        by_seed = 1234)

out2 <- power4test_by_n(out1,
                        n = c(50, 150),
                        by_seed = 2345)

out_reject1 <- rejection_rates_by_n(out1)
out_reject1
out_reject2 <- rejection_rates_by_n(out2)
out_reject2

expect_equal(out_reject1$n,
             c(100, 300))
expect_equal(out_reject2$n,
             c(50, 150))

out <- c(out1, out2)
out_reject <- rejection_rates_by_n(out)
expect_equal(out_reject$n,
             c(50, 100, 150, 300))

})
