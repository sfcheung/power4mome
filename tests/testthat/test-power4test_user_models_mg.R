skip_on_cran()

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

fit <- power_all_sim_only_1$sim_all[[1]]$extra$fit
expect_equal(
  lavInspect(fit, "ngroups"),
  1
)


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

fit <- power_all_sim_only_2$sim_all[[1]]$extra$fit
expect_equal(
  lavInspect(fit, "ngroups"),
  1
)
fit <- power_all_sim_only_2$sim_all[[1]]$extra$fit2
expect_equal(
  lavInspect(fit, "ngroups"),
  2
)

})