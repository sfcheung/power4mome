skip("Internal")

library(testthat)

test_that("PBA: diagnostic function", {

# Case 1

mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: s
y ~ m: m
y ~ x: s
"

out <- power4test(nrep = 51,
                  model = mod,
                  pop_es = mod_es,
                  n = 600,
                  R = 99,
                  test_fun = test_parameters,
                  test_args = list(par = "y~x"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = !testthat::is_testing())

system.time(
a_out <- x_from_power(out,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 100,
                     save_sim_all = FALSE,
                     x_interval = c(300, 1000),
                     algorithm = "probabilistic_bisection",
                     what = "point",
                     goal = "close_enough")
)

pba_diagnosis(a_out)

})