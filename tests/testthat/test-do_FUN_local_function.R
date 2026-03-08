skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("do_FUN with global environment functions", {

mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
y ~ m: l
m ~ x: m
y ~ x: n
"

test_process <- function(data) {
  data$x <- data$x * 10
  data
}

# Simulate the data
out <- power4test(
         nrep = 2,
         model = mod,
         pop_es = mod_es,
         n = 5000,
         process_data = list(fun = "test_process"),
         test_fun = test_parameters,
         test_args = list(op = "~"),
         parallel = TRUE,
         iseed = 1234)

expect_equal(sd((out$sim_all[[1]]$mm_lm_dat_out)$x),
             10,
             tolerance = 1e-1)

})
