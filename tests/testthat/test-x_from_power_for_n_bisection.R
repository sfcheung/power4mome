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
out_power <- rejection_rates(out)
out_power

system.time(
tmp1 <- x_from_power(out,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234,
                     save_sim_all = TRUE,
                     algorithm = "power_curve")
)
system.time(
tmp2 <- x_from_power(out,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234,
                     save_sim_all = TRUE,
                     algorithm = "bisection")
)

tmp1
tmp2

summary(tmp1)
summary(tmp2)

plot(tmp1)
plot(tmp2)

plot(tmp2,
     lwd = 4,
     col = "green",
     what = c("ci",
              "final_x",
              "final_power"))
plot(tmp2,
     lwd = 4,
     col = "darkgreen",
     what = c("ci",
              "final_x",
              "final_power"),
     pars_ci_final_x = list(lwd = 10),
     pars_final_x = list(lwd = 2, col = "red"))

plot(tmp2,
     pars_power_curve = list(col = "blue", lwd = 4, type = "o"))

# Case 2

out2 <- power4test(nrep = 50,
                   model = mod,
                   pop_es = mod_es,
                   n = 1000,
                   R = 1000,
                   ci_type = "mc",
                   test_fun = test_indirect_effect,
                   test_args = list(x = "x",
                                    m = "m",
                                    y = "y",
                                    mc_ci = TRUE),
                   iseed = 1234,
                   parallel = TRUE)

system.time(
tmp1 <- x_from_power(out2,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234,
                     algorithm = "power_curve")
)
system.time(
tmp2 <- x_from_power(out2,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234,
                     algorithm = "bisection")
)

tmp1
tmp2
summary(tmp1)
summary(tmp2)

plot(tmp1)
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
                   R = 1000,
                   ci_type = "mc",
                   test_fun = test_indirect_effect,
                   test_args = list(x = "x",
                                    m = c("m1", "m2", "m3"),
                                    y = "y",
                                    mc_ci = TRUE),
                   iseed = 1234,
                   parallel = TRUE)

rejection_rates(out3)

system.time(
tmp1 <- x_from_power(out3,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234,
                     algorithm = "power_curve")
)
system.time(
tmp2 <- x_from_power(out3,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234,
                     algorithm = "bisection")
)

tmp1
tmp2

summary(tmp1)
summary(tmp2)

plot(tmp1)
plot(tmp2)


})