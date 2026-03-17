library(testthat)
suppressMessages(library(lavaan))

test_that("sim_out: descriptive for multigroup model", {

mod <-
"
m ~ x
y ~ m + x
"

es <-
"
y ~ m: l
m ~ x:
  - nil
  - s
y ~ x: nil
"

data_all <- sim_data(nrep = 2,
                     model = mod,
                     pop_es = es,
                     n = 100,
                     progress = !is_testing(),
                     iseed = 1234)

expect_output(print(data_all),
              "Descriptive statistics by group",
              fixed = TRUE)


})
