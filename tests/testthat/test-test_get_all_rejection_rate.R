library(testthat)

test_that("Get all rejection rates", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 4,
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
                                        boot_ci = TRUE,
                                        mc_ci = FALSE))
test_out <- power4test(object = test_out,
                       test_fun = test_parameters)
test_out <- power4test(object = test_out,
                       test_fun = test_parameters,
                       test_args = list(op = "~"))
test_out <- power4test(object = test_out,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        y = "y",
                                        boot_ci = TRUE,
                                        mc_ci = FALSE))

expect_no_error(rejection_rates(test_out))
expect_no_error(rejection_rates(test_out,
                                    all_columns = TRUE))

tmp <- rejection_rates(test_out,
                           ci = FALSE)
expect_false("reject_ci_lo" %in% colnames(tmp))

tmp <- rejection_rates(test_out,
                           se = TRUE)
expect_true("reject_se" %in% colnames(tmp))

tmp <- rejection_rates(test_out,
                           ci = FALSE,
                           se = TRUE)
expect_true("reject_se" %in% colnames(tmp))

tmp <- rejection_rates(test_out,
                           all_columns = TRUE,
                           ci = FALSE)
expect_false("reject_ci_lo" %in% colnames(tmp))

tmp <- rejection_rates(test_out,
                           all_columns = TRUE,
                           se = TRUE)
expect_true("reject_se" %in% colnames(tmp))

tmp <- rejection_rates(test_out,
                           all_columns = TRUE,
                           ci = FALSE,
                           se = TRUE)
expect_true("reject_se" %in% colnames(tmp))

})
