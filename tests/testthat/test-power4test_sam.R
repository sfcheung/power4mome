library(testthat)
suppressMessages(library(lavaan))

# All-In-One

test_that("power4test: sam", {

skip_if(utils::packageVersion("manymome") < "0.3.4.16")

model_simple_med <-
"
m ~ a*x
y ~ b*m + x
ab := a * b
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")
k <- c(y = 3,
       m = 3,
       x = 3)
rel <- c(y = .70,
         m = .70,
         x = .70)

# Generate the data

power_all_sim_only1 <- power4test(
  nrep = 10,
  model = model_simple_med,
  pop_es = model_simple_med_es,
  n = 100,
  number_of_indicators = k,
  reliability = rel,
  fit_model_args = list(fit_function = lavaan::sam),
  R = 50,
  do_the_test = FALSE,
  progress = !is_testing(),
  iseed = 1234
)

power_all_sim_only2 <- power4test(
  nrep = 10,
  model = model_simple_med,
  pop_es = model_simple_med_es,
  n = 100,
  number_of_indicators = k,
  reliability = rel,
  fit_model_args = list(fit_function = "sam"),
  R = 50,
  do_the_test = FALSE,
  progress = !is_testing(),
  iseed = 1234
)


# Do the test

power_all_test_only1 <- power4test(
  object = power_all_sim_only1,
  test_fun = test_indirect_effect,
  test_args = list(x = "x",
                    m = "m",
                    y = "y")
)

power_all_test_only2 <- power4test(
  object = power_all_sim_only2,
  test_fun = test_indirect_effect,
  test_args = list(x = "x",
                    m = "m",
                    y = "y")
)

summary_all1 <- test_summary(power_all_test_only1)
summary_all2 <- test_summary(power_all_test_only2)

expect_equal(summary_all1[[1]],
             summary_all2[[1]])

})
