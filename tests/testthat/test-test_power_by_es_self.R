library(testthat)

skip_if_not_installed("lmhelprs")

test_that("Power by pop es", {

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

out1 <- power4test_by_pop_es(sim_only,
                             pop_es_name = "y ~ m",
                             pop_es_values = c(.10, .20),
                             test_fun = test_indirect_effect,
                             test_args = list(x = "x",
                                              m = "m",
                                              y = "y",
                                              boot_ci = TRUE,
                                              mc_ci = FALSE),
                             by_seed = 1234)

out2 <- power4test_by_pop_es(out1,
                             pop_es_name = "y ~ m",
                             pop_es_values = c(.05, .15),
                             by_seed = 2345)

out_reject1 <- get_rejection_rates_by_pop_es(out1)
out_reject1
out_reject2 <- get_rejection_rates_by_pop_es(out2)
out_reject2

expect_equal(out_reject1$es,
             c(.1, .2))
expect_equal(out_reject2$es,
             c(.05, .15))

out <- c(out1, out2)
out_reject <- get_rejection_rates_by_pop_es(out)
expect_equal(out_reject$es,
             c(.05, .10, .15, .20))

})
