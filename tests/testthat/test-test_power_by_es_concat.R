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

out1 <- power4test_by_pop_es(test_out,
                             pop_es_name = "y ~ m",
                             pop_es_values = c(.10, .20))
out2 <- power4test_by_pop_es(test_out,
                             pop_es_name = "y ~ m",
                             pop_es_values = c(.40, .30))
out <- c(out1, out2)

out_reject <- get_rejection_rates_by_pop_es(out)

out_chk <- power4test_by_pop_es(test_out,
                                pop_es_name = "y ~ m",
                                pop_es_values = c(.10, .20, .30, .40))
out_reject_chk <- get_rejection_rates_by_pop_es(out_chk)

expect_identical(out_reject,
                 out_reject_chk)

# Test sort = FALSE

out <- c(out1, out2, sort = FALSE)
out_reject <- get_rejection_rates_by_pop_es(out)

out_chk <- power4test_by_pop_es(test_out,
                                pop_es_name = "y ~ m",
                                pop_es_values = c(.10, .20, .40, .30))
out_reject_chk <- get_rejection_rates_by_pop_es(out_chk)

expect_identical(out_reject,
                 out_reject_chk)


model_simple_med2 <-
"
m ~ x
y ~ m
"

model_simple_med_es2 <- c("y ~ m" = "l",
                          "m ~ x" = "m")

sim_only2 <- power4test(nrep = 2,
                        model = model_simple_med2,
                        pop_es = model_simple_med_es2,
                        n = 100,
                        R = 50,
                        ci_type = "boot",
                        fit_model_args = list(fit_function = "lm"),
                        do_the_test = FALSE,
                        iseed = 1234)

test_out2 <- power4test(object = sim_only2,
                        test_fun = test_indirect_effect,
                        test_args = list(x = "x",
                                         m = "m",
                                         y = "y",
                                         boot_ci = TRUE))

out3 <- power4test_by_pop_es(test_out2,
                             pop_es_name = "y ~ m",
                             pop_es_values = c(.10, .20))

expect_error(c(out3, out1))

})
