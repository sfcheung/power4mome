library(testthat)

skip_if_not_installed("lmhelprs")

test_that("Power by n: Concatenate", {

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

test_out <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE,
                                        mc_ci = FALSE))

out1 <- power4test_by_n(test_out,
                        n = c(100, 110))
out2 <- power4test_by_n(test_out,
                        n = c(130, 120))
out <- c(out1, out2)
out_reject <- get_rejection_rates_by_n(out)

out_chk <- power4test_by_n(test_out,
                           n = c(100, 110, 120, 130))
out_reject_chk <- get_rejection_rates_by_n(out_chk)

expect_identical(out_reject,
                 out_reject_chk)

# Test sort = FALSE

out <- c(out1, out2, sort = FALSE)
out_reject <- get_rejection_rates_by_n(out)

out_chk <- power4test_by_n(test_out,
                           n = c(100, 110, 130, 120))
out_reject_chk <- get_rejection_rates_by_n(out_chk)

expect_identical(out_reject,
                 out_reject_chk)

model_simple_med2 <-
"
m ~ x
y ~ m
"
model_simple_med_es2 <- c("y ~ m" = "l",
                          "m ~ x" = "m")
sim_only2 <- power4test(nrep = 3,
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
                                         boot_ci = TRUE,
                                         mc_ci = FALSE))
out3 <- power4test_by_n(test_out2,
                        n = c(105, 115))
expect_error(c(out3, out1))

})
