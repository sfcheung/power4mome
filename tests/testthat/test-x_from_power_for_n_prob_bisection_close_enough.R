skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("x_from_power: probabilistic bisection", {

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
                  n = 100,
                  R = 199,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 1234,
                  parallel = TRUE)
out_power <- rejection_rates(out)
out_power

system.time(
tmp2 <- x_from_power(out,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 1000,
                     final_R = 199,
                     seed = 1234,
                     save_sim_all = FALSE,
                     algorithm = "probabilistic_bisection",
                     what = "point",
                     goal = "close_enough")
)

tmp2

summary(tmp2)

plot(tmp2)


system.time(
tmp3 <- x_from_power(out,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 1000,
                     seed = 1234,
                     save_sim_all = FALSE,
                     algorithm = "probabilistic_bisection",
                     what = "lb",
                     goal = "close_enough")
)

tmp3

summary(tmp3)

plot(tmp3)


# Case 2

out2 <- power4test(nrep = 50,
                   model = mod,
                   pop_es = mod_es,
                   n = 1000,
                   R = 199,
                   ci_type = "mc",
                   test_fun = test_indirect_effect,
                   test_args = list(x = "x",
                                    m = "m",
                                    y = "y",
                                    mc_ci = TRUE,
                                    test_method = "pvalue"),
                   iseed = 1234,
                   parallel = TRUE)

system.time(
tmp2 <- x_from_power(out2,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 1000,
                     final_R = 1000,
                     seed = 1234,
                     algorithm = "probabilistic_bisection",
                     what = "ub",
                     goal = "close_enough",
                     x_interval = c(100, 2000))
)

tmp2

summary(tmp2)

plot(tmp2)

# Case 3

mod <-
"
m1 ~ x
m2 ~ m1 + x
m3 ~ m2 + m1 + x
y ~ m3 + m2 + m1 + x
"

mod_es <-
"
m1 ~ x: s
m2 ~ m1: s
m3 ~ m2: s
y ~ m3: s
y ~ x: l
"

out3 <- power4test(nrep = 50,
                   model = mod,
                   pop_es = mod_es,
                   n = 200,
                   R = 199,
                   ci_type = "mc",
                   test_fun = test_indirect_effect,
                   test_args = list(x = "x",
                                    m = c("m1", "m2", "m3"),
                                    y = "y",
                                    mc_ci = TRUE,
                                    test_method = "pvalue"),
                   iseed = 1234,
                   parallel = TRUE)

rejection_rates(out3)

system.time(
tmp2 <- x_from_power(out3,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 1000,
                     final_R = 1000,
                     seed = 1234,
                     algorithm = "probabilistic_bisection",
                     what = "ub",
                     goal = "close_enough")
)

tmp2

summary(tmp2)

plot(tmp2)


})