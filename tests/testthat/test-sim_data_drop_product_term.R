library(testthat)
suppressMessages(library(lavaan))

test_that("sim_data: drop w:x", {

skip_on_cran()

model <-
"
m ~ x + w + w:x
y ~ m + x
"

model_es <-
"
m ~ x: m
m ~ w: nil
m ~ w:x: m
y ~ x: nil
y ~ m: m
"

# Check the Data Generated

out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  number_of_indicators = c(x = 3,
                                           m = 4,
                                           y = 3,
                                           w = 4),
                  reliability = c(x = .80,
                                  m = .70,
                                  y = .80,
                                  w = .80),
                  process_data = list(fun = common_processes,
                                      args = list(method = "mean")),
                  progress = !is_testing(),
                  iseed = 1234)

dat <- pool_sim_data(out)

expect_length(
  intersect(
    c("x:w", "w:x"),
    colnames(dat)
  ),
  0
)

out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  number_of_indicators = c(x = 3,
                                           m = 4,
                                           y = 3,
                                           w = 4),
                  reliability = c(x = .80,
                                  m = .70,
                                  y = .80,
                                  w = .80),
                  iseed = 1234,
                  progress = !is_testing())

dat <- pool_sim_data(out)

expect_length(
  intersect(
    c("x:w", "w:x"),
    colnames(dat)
  ),
  0
)

})
