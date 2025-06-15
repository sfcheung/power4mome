skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("x_from_power: bisection", {

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

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 50,
                  R = 500,
                  ci_type = "mc",
                  test_fun = test_parameters,
                  test_args = list(par = "y~m"),
                  iseed = 1234,
                  parallel = TRUE)
out_power <- rejection_rates(out)
out_power

system.time(
tmp2 <- x_from_power(out,
                     x = "es",
                     pop_es_name = "y ~ m",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234,
                     save_sim_all = TRUE,
                     algorithm = "bisection",
                     what = "ub",
                     goal = "close_enough")
)

tmp2

summary(tmp2)

plot(tmp2)


system.time(
tmp3 <- x_from_power(out,
                     x = "es",
                     pop_es_name = "y ~ m",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234,
                     save_sim_all = TRUE,
                     algorithm = "bisection",
                     what = "lb",
                     goal = "close_enough")
)

tmp3

summary(tmp3)

plot(tmp3)


})