library(testthat)

test_that("All parameters: lm", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "s")

sim_only <- power4test(nrep = 10,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 50,
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234,
                       progress = !is_testing())

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters)

chk0 <- rejection_rates(test_out, collapse = "all_sig")
chk1 <- rejection_rates(test_out, collapse = "at_least_one_sig")
chk2 <- rejection_rates(test_out, collapse = "at_least_k_sig", at_least_k = 1)
chk3 <- rejection_rates(test_out, collapse = "at_least_k_sig", at_least_k = 2)
chk4 <- rejection_rates(test_out, collapse = "at_least_k_sig", at_least_k = 3)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(omnibus = "all_sig"),
                       progress = FALSE)
(chk <- test_summary(test_out))
expect_equal(chk0$reject,
             chk[[1]]$sig)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(omnibus = "at_least_one_sig"),
                       progress = FALSE)
(chk <- test_summary(test_out))
expect_equal(chk1$reject,
             chk[[1]]$sig)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(omnibus = "at_least_k_sig",
                                        at_least_k = 1),
                       progress = FALSE)
(chk <- test_summary(test_out))
expect_equal(chk1$reject,
             chk[[1]]$sig)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(omnibus = "at_least_k_sig",
                                        at_least_k = 2),
                       progress = FALSE)
(chk <- test_summary(test_out))
expect_equal(chk3$reject,
             chk[[1]]$sig)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(omnibus = "at_least_k_sig",
                                        at_least_k = 3),
                       progress = FALSE)
(chk <- test_summary(test_out))
expect_equal(chk4$reject,
             chk[[1]]$sig)
expect_equal(chk0$reject,
             chk[[1]]$sig)


})
