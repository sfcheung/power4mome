skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("power4test: User models: model_append", {

model_simple_med <-
"
m ~ a*x
y ~ b*m + x
ab := a * b
"

model_simple_med_es <- c("y ~ m" = "m",
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
  number_of_indicators = k,
  reliability = rel,
  n = 100,
  fit_model_args = list(model_append = "y ~ 0*x"),
  R = 50,
  do_the_test = FALSE,
  progress = !is_testing(),
  iseed = 1234
)

# Check the parameter `y ~ x`
est <- parameterTable(power_all_sim_only_1$sim_all[[1]]$extra$fit)
est$lav_label <- lav_partable_labels(est)
i <- match("y~x", est$lav_label)
expect_equal(
  est$free[i],
  0
)
expect_equal(
  est$est[i],
  0
)

# Fit a user model

power_all_sim_only_2 <- power4test(
  nrep = 10,
  model = model_simple_med,
  pop_es = model_simple_med_es,
  number_of_indicators = k,
  reliability = rel,
  n = 100,
  fit_model_args = list(
    fit = list(),
    fit2 = list(model_append = "y ~ 0*x")
  ),
  R = 50,
  do_the_test = FALSE,
  progress = !is_testing(),
  iseed = 1234
)

# Check the parameter `y ~ x`
est <- parameterTable(power_all_sim_only_2$sim_all[[1]]$extra$fit)
est$lav_label <- lav_partable_labels(est)
i <- match("y~x", est$lav_label)
expect_true(
  est$free[i] > 0
)
est <- parameterTable(power_all_sim_only_2$sim_all[[1]]$extra$fit2)
est$lav_label <- lav_partable_labels(est)
i <- match("y~x", est$lav_label)
expect_equal(
  est$free[i],
  0
)
expect_equal(
  est$est[i],
  0
)

})
