library(testthat)

test_that("ordinal_variables", {

expect_true(is.list(cut_patterns()))
expect_true(length(cut_patterns("s3")) == 2)
expect_setequal(cut_patterns("ma4"),
                -cut_patterns("-ma4"))

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

sim_only1 <- power4test(
                  nrep = 2,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 100,
                  number_of_indicators = k,
                  reliability = rel,
                  process_data = list(fun = "ordinal_variables",
                                      args = list(cut_patterns = c(x = "s3"),
                                                  cuts = list(m = c(-2, 0, 2)))),
                  fit_model_args = list(estimator = "ML"),
                  R = 100,
                  do_the_test = FALSE,
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)

expect_output(print(sim_only1, data_long = TRUE),
              " Response Proportions ")

tmp <- pool_sim_data(sim_only1)

expect_equal(length(table(tmp$m1)), 4)
expect_equal(length(table(tmp$x1)), 3)


sim_only1 <- power4test(
                  nrep = 2,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 100,
                  number_of_indicators = k,
                  reliability = rel,
                  process_data = list(fun = "ordinal_variables",
                                      args = list(cut_patterns = c(x = "-ma3"))),
                  fit_model_args = list(estimator = "ML"),
                  R = 100,
                  do_the_test = FALSE,
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)

tmp <- pool_sim_data(sim_only1)

expect_equal(length(table(tmp$x1)), 3)

sim_only1 <- power4test(
                  nrep = 2,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 100,
                  number_of_indicators = k,
                  reliability = rel,
                  process_data = list(fun = "ordinal_variables",
                                      args = list(cuts = list(x = c(-2, 1, 2)))),
                  fit_model_args = list(estimator = "ML"),
                  R = 100,
                  do_the_test = FALSE,
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)

tmp <- pool_sim_data(sim_only1)

expect_equal(length(table(tmp$x1)), 4)

sim_only1 <- power4test(
                  nrep = 2,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 100,
                  number_of_indicators = k,
                  reliability = rel,
                  process_data = list(fun = "ordinal_variables"),
                  fit_model_args = list(estimator = "ML"),
                  R = 100,
                  do_the_test = FALSE,
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)

tmp <- pool_sim_data(sim_only1)

expect_true(length(table(tmp$x1)) > 10)


expect_error(power4test(
                  nrep = 2,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 100,
                  number_of_indicators = k,
                  reliability = rel,
                  process_data = list(fun = "ordinal_variables",
                                      args = list(cut_patterns = c(x = "test"))),
                  fit_model_args = list(estimator = "ML"),
                  R = 100,
                  do_the_test = FALSE,
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)
)

})
