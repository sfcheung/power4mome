library(testthat)
suppressMessages(library(lavaan))

test_that("sim_out and do_test", {

mod <-
"m ~ x
 y ~ m + x"
es <-
c("y ~ m" = "m",
  "m ~ x" = "m",
  "y ~ x" = "n")
data_all <- sim_data(nrep = 3,
                     model = mod,
                     pop_es = es,
                     n = 100,
                     iseed = 1234)

expect_output(print(data_all), "Variances and error variances")
expect_output(print(data_all, variances = TRUE), "Variances:")
expect_output(print(data_all, est_type = "unstandardized"), "Unstandardized")
expect_output(print(data_all, est_type = c("standardized", "unstandardized")), "Unstandardized")


})
