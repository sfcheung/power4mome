skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("power4test: User models", {

model_simple_med <-
"
m ~ a*x
y ~ b*m + x
ab := a * b
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "l")
k <- c(y = 3,
       m = 3,
       x = 3)
rel <- c(y = .70,
         m = .70,
         x = .70)

model_complete_med <-
"
m ~ a*x
y ~ b*m
ab := a * b
"

# Generate the data

# Fit one user model

power_all_sim_only_1 <- power4test(
  nrep = 10,
  model = model_simple_med,
  pop_es = model_simple_med_es,
  n = 100,
  fit_model_args = list(model = model_complete_med),
  R = 50,
  do_the_test = FALSE,
  progress = !is_testing(),
  iseed = 1234
)

fit <- power_all_sim_only_1$sim_all[[1]]$extra$fit
expect_equal(
  lavaan::fitMeasures(fit, "df"),
  1,
  ignore_attr = TRUE
)

# Fit a user model

power_all_sim_only_2 <- power4test(
  nrep = 10,
  model = model_simple_med,
  pop_es = model_simple_med_es,
  n = 100,
  fit_model_args = list(
    fit = list(model = model_complete_med),
    fit2 = list(model = model_simple_med)
  ),
  R = 50,
  do_the_test = FALSE,
  progress = !is_testing(),
  iseed = 1234
)

fit <- power_all_sim_only_2$sim_all[[1]]$extra$fit
expect_equal(
  lavaan::fitMeasures(fit, "df"),
  1,
  ignore_attr = TRUE
)
fit <- power_all_sim_only_2$sim_all[[1]]$extra$fit2
expect_equal(
  lavaan::fitMeasures(fit, "df"),
  0,
  ignore_attr = TRUE
)

# Indirect effect

# Do the test
# - Need only the arguments for the test.

power_all_test_only_1 <- power4test(
  object = power_all_sim_only_1,
  test_fun = test_indirect_effect,
  test_args = list(
    x = "x",
    m = "m",
    y = "y",
    mc_ci = TRUE)
)
summary_all1 <- test_summary(power_all_test_only_1)
summary_all1

power_all_test_only_2_fit <- power4test(
  object = power_all_sim_only_2,
  test_fun = test_indirect_effect,
  test_args = list(
    x = "x",
    m = "m",
    y = "y",
    mc_ci = TRUE)
)
summary_all_fit2_1 <- test_summary(power_all_test_only_2_fit)
summary_all_fit2_1

power_all_test_only_2_fit2 <- power4test(
  object = power_all_sim_only_2,
  test_fun = test_indirect_effect,
  test_args = list(
    x = "x",
    m = "m",
    y = "y",
    fit_name = "fit",
    mc_ci = TRUE),
  test_name = "ind_fit"
)
power_all_test_only_2_fit2 <- power4test(
  object = power_all_test_only_2_fit2,
  test_fun = test_indirect_effect,
  test_args = list(
    x = "x",
    m = "m",
    y = "y",
    fit_name = "fit2",
    mc_ci = TRUE),
  test_name = "ind_fit2"
)
summary_all_fit2_2 <- test_summary(power_all_test_only_2_fit2)
summary_all_fit2_2

expect_equal(
  summary_all_fit2_1[[1]][c("cilo", "cihi")],
  summary_all_fit2_2[[1]][c("cilo", "cihi")],
  tolerance = 1e-4
)

})