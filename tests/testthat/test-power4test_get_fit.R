library(testthat)
suppressMessages(library(lavaan))

# All-In-One

test_that("power4test: Get fit", {

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

fit0 <- get_sim_fit(power_all_sim_only_k2)

expect_identical(
  fit0,
  power_all_sim_only_k2$sim_all[[1]]$extra$fit
)

fit0 <- get_sim_fit(power_all_sim_only_k2, "fit2")

expect_identical(
  fit0,
  power_all_sim_only_k2$sim_all[[1]]$extra$fit2
)

expect_error(get_sim_fit(power_all_sim_only_k2, "fit0"))
expect_error(get_sim_fit(power_all_sim_only_k2, fit_class = "lm"))

power_all_sim_only_k2 <- power4test(nrep = 3,
                                    model = model_simple_med,
                                    pop_es = model_simple_med_es,
                                    n = 100,
                                    fit_model_args = list(fit = list(fit_function = "lm"),
                                                          fit2 = list(model = model_simple_med_complete),
                                                          fit3 = list(model = model_simple_med_test) ),
                                    do_the_test = FALSE,
                                    progress = !is_testing(),
                                    parallel = FALSE,
                                    iseed = 1234)

expect_error(get_sim_fit(power_all_sim_only_k2, "fit0"))
expect_error(get_sim_fit(power_all_sim_only_k2, fit_class = "lm"))

fit0 <- get_sim_fit(power_all_sim_only_k2, "fit2")
expect_identical(
  fit0,
  power_all_sim_only_k2$sim_all[[1]]$extra$fit2
)

expect_equal(
  get_sim_fit(power_all_sim_only_k2, NULL),
  c("fit", "fit2", "fit3")
)
expect_equal(
  get_sim_fit(power_all_sim_only_k2, NULL, fit_class = "lavaan"),
  c("fit2", "fit3")
)

fit0 <- get_sim_fit(power_all_sim_only_k1)
expect_identical(
  fit0,
  power_all_sim_only_k1$sim_all[[1]]$extra$fit
)

})

