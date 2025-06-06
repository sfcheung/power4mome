skip("WIP")

library(testthat)

test_that("x_from_power: n", {

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

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  R = 500,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 1234,
                  parallel = TRUE)

})
