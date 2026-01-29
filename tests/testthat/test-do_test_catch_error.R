skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("do_test: Error and warning catching", {

# Simple Mediation Model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")
k <- c(y = 8,
       m = 8,
       x = 8)
rel <- c(y = .30,
         m = .30,

         x = .30)

suppressWarnings(
  out_i <- power4test(
            nrep = 2,
            model = model_simple_med,
            pop_es = model_simple_med_es,
            n = 50,
            number_of_indicators = k,
            R = 100,
            # fit_model_args = list(control = list(iter.max = 100)),
            ci_type = "mc",
            reliability = rel,
            test_fun = test_indirect_effect,
            test_args = list(x = "x",
                             m = "m",
                             y = "y"),
            iseed = 1234,
            parallel = FALSE,
            progress = !is_testing()
          )
)
expect_true(rejection_rates(out_i, all_columns = TRUE)$nvalid < 2)

suppressWarnings(
  out_i <- power4test(
            nrep = 2,
            model = model_simple_med,
            pop_es = model_simple_med_es,
            n = 50,
            number_of_indicators = k,
            R = 100,
            fit_model_args = list(control = list(iter.max = 100)),
            ci_type = "mc",
            reliability = rel,
            test_fun = test_indirect_effect,
            test_args = list(x = "x",
                             m = "m",
                             y = "y"),
            iseed = 1234,
            parallel = FALSE,
            progress = !is_testing()
          )
)
expect_true(rejection_rates(out_i, all_columns = TRUE)$nvalid == 0)

})
