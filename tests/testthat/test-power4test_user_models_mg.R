skip("WIP")

library(testthat)
suppressMessages(library(lavaan))

test_that("power4test: User models: Multigroup", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <-
"
y ~ m: l
m ~ x:
  - nil
  - s
y ~ x: nil
"

model_complete_med <-
"
m ~ x
y ~ m
"

# Generate the data

# Fit one user model

power_all_sim_only_1 <- power4test(
  nrep = 10,
  model = model_simple_med,
  pop_es = model_simple_med_es,
  n = 100,
  fit_model_args = list(
    model = model_complete_med,
    arg_group_name = NULL
  ),
  R = 50,
  do_the_test = FALSE,
  progress = !is_testing(),
  iseed = 1234
)

power_all_sim_only_1

# Fit a user model

power_all_sim_only_2 <- power4test(
  nrep = 10,
  model = model_simple_med,
  pop_es = model_simple_med_es,
  n = 100,
  fit_model_args = list(
    fit = list(model = model_complete_med,
               arg_group_name = NULL),
    fit2 = list(model = model_complete_med)
  ),
  R = 50,
  do_the_test = FALSE,
  progress = !is_testing(),
  iseed = 1234
)

power_all_sim_only_2


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
summary_all <- test_summary(power_all_test_only_1)
summary_all

power_all_test_only_2_fit <- power4test(
  object = power_all_sim_only_2,
  test_fun = test_indirect_effect,
  test_args = list(
    x = "x",
    m = "m",
    y = "y",
    mc_ci = TRUE)
)
summary_all_fit <- test_summary(power_all_test_only_2_fit)
summary_all_fit

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
summary_all_fit2 <- test_summary(power_all_test_only_2_fit2)
summary_all_fit2
rejection_rates(power_all_test_only_2_fit2)

})