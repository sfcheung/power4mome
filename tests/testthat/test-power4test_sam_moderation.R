skip_on_cran()

# A slow test with bootstrapping

library(testthat)
suppressMessages(library(lavaan))

# All-In-One

test_that("power4test: sam", {

skip_if(utils::packageVersion("manymome") < "0.3.4.25")

mod <-
"
m ~ x + w + x:w
y ~ m + x
"

es <- c(
"m ~ x:w" = "m",
"y ~ m" = "l",
"m ~ x" = "nil",
"y ~ x" = "nil"
)

k <- c(y = 3,
       m = 3,
       x = 3,
       w = 3)
rel <- c(y = .70,
         m = .70,
         x = .70,
         w = .70)

# Generate the data

power_all_sim_only1 <- power4test(
  nrep = 10,
  model = mod,
  pop_es = es,
  n = 100,
  number_of_indicators = k,
  reliability = rel,
  fit_model_args = list(fit_function = lavaan::sam),
  R = 5,
  ci_type = "boot",
  do_the_test = FALSE,
  progress = !is_testing(),
  iseed = 1234
)

# Do the test

power_all_test_only1 <- power4test(
  object = power_all_sim_only1,
  test_fun = test_parameters,
  test_args = list(pars = "m~x:w")
)

power_all_test_only2 <- power4test(
  object = power_all_sim_only1,
  test_fun = test_moderation
)

summary_all1 <- test_summary(power_all_test_only1)
summary_all2 <- test_summary(power_all_test_only2)

expect_equal(summary_all1[[1]],
             summary_all2[[1]])

# The warnings are expected due to small R
suppressWarnings(
power_all_test_only3 <- power4test(
  object = power_all_sim_only1,
  test_fun = test_cond_indirect,
  test_args = list(x = "x",
                   y = "m",
                   wvalues = c(w = 1),
                   boot_ci = TRUE))
)

suppressWarnings(
power_all_test_only4 <- power4test(
  object = power_all_sim_only1,
  test_fun = test_cond_indirect_effects,
  test_args = list(x = "x",
                   y = "m",
                   wlevels = c("w"),
                   boot_ci = TRUE))
)

expect_no_error(summary_all3 <- test_summary(power_all_test_only3))
expect_no_error(summary_all4 <- test_summary(power_all_test_only4))

# The warnings are expected due to small R
suppressWarnings(
power_all_test_only5 <- power4test(
  object = power_all_sim_only1,
  test_fun = test_cond_indirect,
  test_args = list(x = "x",
                   y = "y",
                   m = "m",
                   wvalues = c(w = 1),
                   boot_ci = TRUE))
)

suppressWarnings(
power_all_test_only6 <- power4test(
  object = power_all_sim_only1,
  test_fun = test_cond_indirect_effects,
  test_args = list(x = "x",
                   y = "y",
                   m = "m",
                   wlevels = c("w"),
                   boot_ci = TRUE))
)

expect_no_error(summary_all5 <- test_summary(power_all_test_only5))
expect_no_error(summary_all6 <- test_summary(power_all_test_only6))


})
