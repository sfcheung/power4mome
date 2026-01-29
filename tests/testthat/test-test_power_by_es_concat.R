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
                       progress = !is_testing(),
                       parallel = FALSE,
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE))

tmp <- as.power4test_by_es(sim_only,
                           pop_es_name = "y ~ m")
expect_true(is.list(tmp))
expect_true(length(tmp) == 1)
expect_true(inherits(tmp, "power4test_by_es"))
expect_equal(attr(tmp[[1]], "pop_es_name"), "y ~ m")
expect_equal(attr(tmp[[1]], "pop_es_value"), 0.5)
expect_equal(names(tmp), "y ~ m = 0.5")

power_all_test_only_new_es <- power4test(object = test_out,
                                         pop_es = c("y ~ m" = ".10"))

out1 <- power4test_by_es(test_out,
                             pop_es_name = "y ~ m",
                             pop_es_values = c(.10, .20),
                             by_seed = 1234,
                             progress = !is_testing())
out2 <- power4test_by_es(test_out,
                             pop_es_name = "y ~ m",
                             pop_es_values = c(.40, .30),
                             by_seed = 5678,
                             progress = !is_testing())
out <- c(out1, out2)

out_reject <- rejection_rates(out)
tmp <- sapply(out,
              \(x) {attr(x, "args")$iseed})
out_chk <- power4test_by_es(test_out,
                                pop_es_name = "y ~ m",
                                pop_es_values = c(.10, .20, .30, .40),
                                by_seed = tmp,
                                progress = !is_testing())
out_reject_chk <- rejection_rates(out_chk)

expect_identical(out_reject,
                 out_reject_chk)

# Test different parameters

out4 <- power4test_by_es(test_out,
                             pop_es_name = "y ~ x",
                             pop_es_values = c(.40, .30),
                             progress = !is_testing())
expect_error(c(out1, out4))

# Test sort = FALSE

out <- c(out1, out2, sort = FALSE)
out_reject <- rejection_rates(out)
tmp <- sapply(out,
              \(x) {attr(x, "args")$iseed})
out_chk <- power4test_by_es(test_out,
                                pop_es_name = "y ~ m",
                                pop_es_values = c(.10, .20, .40, .30),
                                by_seed = tmp,
                                progress = !is_testing())
out_reject_chk <- rejection_rates(out_chk)

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
                        progress = !is_testing(),
                        iseed = 1234)

test_out2 <- power4test(object = sim_only2,
                        test_fun = test_indirect_effect,
                        test_args = list(x = "x",
                                         m = "m",
                                         y = "y",
                                         boot_ci = TRUE))

out3 <- power4test_by_es(test_out2,
                             pop_es_name = "y ~ m",
                             pop_es_values = c(.10, .20),
                             progress = !is_testing())

expect_error(c(out3, out1))

})
