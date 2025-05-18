library(testthat)

skip_if_not_installed("lmhelprs")

test_that("Power by es", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 2,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       R = 50,
                       ci_type = "boot",
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE))

power_all_test_only_new_es <- power4test(object = test_out,
                                         pop_es = c("y ~ m" = ".10"))

out <- power4test_by_es(test_out,
                            pop_es_name = "y ~ m",
                            pop_es_values = c(.10, .20),
                            by_seed = 1357)
out_reject <- get_rejection_rates_by_pop_es(out)

out2 <- power4test_by_es(object = sim_only,
                             pop_es_name = "y ~ m",
                             pop_es_values = c(.10, .20),
                             test_fun = test_indirect_effect,
                             test_args = list(x = "x",
                                              m = "m",
                                              y = "y",
                                              boot_ci = TRUE,
                                              mc_ci = FALSE),
                             by_seed = 1357)

out_reject2 <- get_rejection_rates_by_pop_es(out2)
out_reject2

expect_equal(out[[2]]$sim_all[[1]]$mm_lm_dat_out[1, ],
             out2[[2]]$sim_all[[1]]$mm_lm_dat_out[1, ])

})
