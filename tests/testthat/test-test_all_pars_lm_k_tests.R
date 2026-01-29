library(testthat)

test_that("All parameters: lm", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "s",
                         "y ~ x" = "m")

sim_only <- power4test(nrep = 10,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 200,
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters)

chk0 <- summarize_tests(test_out)
chk1 <- summarize_tests(test_out, collapse = "all_sig")
chk2 <- summarize_tests(test_out, collapse = "at_least_one_sig")
chk3 <- summarize_tests(test_out, collapse = "at_least_k_sig", at_least_k = 1)
chk4 <- summarize_tests(test_out, collapse = "at_least_k_sig", at_least_k = 2)
chk5 <- summarize_tests(test_out, collapse = "at_least_k_sig", at_least_k = 3)

expect_equal(chk1[[1]]$mean$sig,
             chk5[[1]]$mean$sig)
expect_equal(chk2[[1]]$mean$sig,
             chk3[[1]]$mean$sig)
expect_true(chk3[[1]]$mean$sig >= chk4[[1]]$mean$sig)
expect_true(chk4[[1]]$mean$sig >= chk5[[1]]$mean$sig)

chk0 <- rejection_rates(test_out)
chk1 <- rejection_rates(test_out, collapse = "all_sig")
chk2 <- rejection_rates(test_out, collapse = "at_least_one_sig")
chk3 <- rejection_rates(test_out, collapse = "at_least_k_sig", at_least_k = 1)
chk4 <- rejection_rates(test_out, collapse = "at_least_k_sig", at_least_k = 2)
chk5 <- rejection_rates(test_out, collapse = "at_least_k_sig", at_least_k = 3)

expect_equal(chk1$reject,
             chk5$reject)
expect_equal(chk2$reject,
             chk3$reject)
expect_true(chk3$reject >= chk4$reject)
expect_true(chk4$reject >= chk5$reject)

})
