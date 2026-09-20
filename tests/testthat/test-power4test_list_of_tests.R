library(testthat)

test_that("power4test: List of tests", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters)

test_out2 <- power4test(object = test_out,
                        test_fun = test_fit_measure,
                        test_args = list(
                          fit_measure = "cfi",
                          sig_if = "<.90"
                        ))

test_outb <- power4test(object = test_out2,
                        n = 200)

names(test_outb$test_all)

# A list of tests

test_out_list <- power4test(
  object = sim_only,
  test_fun = list(
    "CFI" = test_fit_measure,
    "Estimates" = test_parameters
  ),
  test_args = list(
    "CFI" = list(
      fit_measure = "cfi",
      sig_if = "<.90"
    )
  )
)

expect_equal(
  names(test_out_list$test_all),
  c("CFI", "Estimates")
)

expect_identical(
  test_out$test_all[[1]][[1]]$test_results,
  test_out_list$test_all$Estimates[[1]]$test_results
)
expect_identical(
  test_out$test_all[[1]][[2]]$test_results,
  test_out_list$test_all$Estimates[[2]]$test_results
)
expect_identical(
  test_out$test_all[[1]][[1]]$test_results,
  test_out_list$test_all$Estimates[[1]]$test_results
)
expect_identical(
  test_out$test_all[[1]][[2]]$test_results,
  test_out_list$test_all$Estimates[[2]]$test_results
)
expect_identical(
  test_out2$test_all[[2]][[1]]$test_results,
  test_out_list$test_all$CFI[[1]]$test_results
)
expect_identical(
  test_out2$test_all[[2]][[2]]$test_results,
  test_out_list$test_all$CFI[[2]]$test_results
)

})
