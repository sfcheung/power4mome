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
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE,
                                        mc_ci = FALSE))

out <- power4test_by_n(test_out,
                       n = c(100, 110),
                       by_seed = 1234,
                       progress = !is_testing())
out_reject <- rejection_rates(out)
out_reject

out2 <- power4test_by_n(object = sim_only,
                        n = c(100, 110),
                        test_fun = test_indirect_effect,
                        test_args = list(x = "x",
                                         m = "m",
                                         y = "y",
                                         boot_ci = TRUE,
                                         mc_ci = FALSE),
                        by_seed = 1234,
                        progress = !is_testing())
out_reject2 <- rejection_rates(out2)
out_reject2

expect_equal(out[[2]]$sim_all[[1]]$mm_lm_dat_out[1, ],
             out2[[2]]$sim_all[[1]]$mm_lm_dat_out[1, ])

})
