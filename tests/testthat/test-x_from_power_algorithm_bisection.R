skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("bisection: n", {

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

# n

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  iseed = 1234)

by_x_1 <- power4test_by_n(out,
                          n = 90)

a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1)
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)

# es

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  iseed = 1234)

by_x_1 <- power4test_by_es(out,
                           pop_es_name = "m~x",
                           pop_es_values = c(.10))

a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1)
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)

})
