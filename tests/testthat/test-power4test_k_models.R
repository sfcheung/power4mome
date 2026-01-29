library(testthat)
suppressMessages(library(lavaan))

# All-In-One

test_that("power4test: k models", {

model_simple_med <-
"
m ~ a*x
y ~ b*m + x
ab := a * b
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

model_simple_med_complete <-
"
m ~ a*x
y ~ b*m
ab := a * b
"

model_simple_med_test <-
"
m ~ a*x
y ~ 0*m
"


# Generate the data

power_all_sim_only_k1 <- power4test(nrep = 3,
                                    model = model_simple_med,
                                    pop_es = model_simple_med_es,
                                    n = 100,
                                    do_the_test = FALSE,
                                    progress = !is_testing(),
                                    parallel = FALSE,
                                    iseed = 1234)

power_all_sim_only_k2 <- power4test(nrep = 3,
                                    model = model_simple_med,
                                    pop_es = model_simple_med_es,
                                    n = 100,
                                    fit_model_args = list(fit = list(),
                                                          fit2 = list(model = model_simple_med_complete),
                                                          fit3 = list(model = model_simple_med_test) ),
                                    do_the_test = FALSE,
                                    progress = !is_testing(),
                                    parallel = FALSE,
                                    iseed = 1234)

power_all_test_only <- power4test(object = power_all_sim_only_k2,
                                  test_fun = test_parameters,
                                  test_args = list(pars = "b"),
                                  map_names = c(fit = "fit"),
                                  test_name = "Model 1")
power_all_test_only <- power4test(object = power_all_test_only,
                                  test_fun = test_parameters,
                                  test_args = list(pars = "b",
                                                   fit_name = "fit2"),
                                  map_names = c(fit = "fit2"),
                                  test_name = "Model 2")
power_all_test_only <- power4test(object = power_all_test_only,
                                  test_fun = test_parameters,
                                  test_args = list(pars = "a",
                                                   fit_name = "fit3"),
                                  map_names = c(fit = "fit3"),
                                  test_name = "Model 3")

if (!is_testing()) {
print(power_all_test_only$test_all,
      test_long = TRUE)
}

chk1 <- power_all_test_only$test_all$`Model 1`[[1]]$test_results$est
chk2 <- coef(power_all_test_only$sim_all[[1]]$extra$fit)["b"]
expect_equal(chk1,
             chk2,
             ignore_attr = TRUE)

chk1 <- power_all_test_only$test_all$`Model 2`[[1]]$test_results$est
chk2 <- coef(power_all_test_only$sim_all[[1]]$extra$fit2)["b"]
expect_equal(chk1,
             chk2,
             ignore_attr = TRUE)

chk1 <- power_all_test_only$test_all$`Model 3`[[1]]$test_results$est
chk2 <- coef(power_all_test_only$sim_all[[1]]$extra$fit3)["a"]
expect_equal(chk1,
             chk2,
             ignore_attr = TRUE)

})

