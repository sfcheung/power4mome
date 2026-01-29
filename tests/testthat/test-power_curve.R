skip_on_cran()

library(testthat)

test_that("pwoer_curve", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = ".2")

sim_only <- power4test(nrep = 50,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 50,
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234,
                       parallel = FALSE,
                       progress = !is_testing())

# By n

out1 <- power4test_by_n(sim_only,
                        nrep = 50,
                        test_fun = test_parameters,
                        test_args = list(par = "y~x"),
                        n = c(25, 100, 200, 250, 500),
                        by_seed = 1234,
                        parallel = FALSE,
                        progress = !is_testing())

rejection_rates(out1)

expect_no_error(pout1 <- power_curve(out1,
                                     verbose = FALSE))
expect_no_error(capture.output(print(pout1)))

expect_no_error(capture.output(print(pout1, data_used = TRUE)))

# expect_no_error(plot(pout1))

# By es

out2 <- power4test_by_es(sim_only,
                             nrep = 50,
                             test_fun = test_parameters,
                             test_args = list(par = "y~x"),
                             pop_es_name = "y ~ x",
                             pop_es_values = seq(0, .7, .15),
                             by_seed = 1234,
                             parallel = FALSE,
                             progress = !is_testing())

rejection_rates(out2,
                all_columns = TRUE)

expect_no_error(pout2 <- power_curve(out2,
                                     verbose = FALSE))
expect_no_error(capture.output(print(out2)))

expect_no_error(capture.output(print(pout2, data_used = TRUE)))

# expect_no_error(plot(pout2))

})
