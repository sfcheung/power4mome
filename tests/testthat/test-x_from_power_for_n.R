skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("x_from_power: n", {

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
out_power <- get_rejection_rates(out)
out_power

tmp <- x_from_power(out,
                    x = "n",
                    progress = TRUE,
                    final_nrep = 100,
                    seed = 1234)
tmp
summary(tmp)
plot(tmp)
plot(tmp,
     lwd = 4,
     col = "green",
     what = c("ci",
              "final_x",
              "final_power"))
plot(tmp,
     lwd = 4,
     col = "darkgreen",
     what = c("ci",
              "final_x",
              "final_power"),
     pars_ci_final_x = list(lwd = 10),
     pars_final_x = list(lwd = 2, col = "red"))

plot(tmp,
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

tmp2 <- x_from_power(out2,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234)
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
                   R = 1000,
                   ci_type = "mc",
                   test_fun = test_indirect_effect,
                   test_args = list(x = "x",
                                    m = c("m1", "m2", "m3"),
                                    y = "y",
                                    mc_ci = TRUE),
                   iseed = 1234,
                   parallel = TRUE)

get_rejection_rates(out3)

tmp3 <- x_from_power(out3,
                     x = "n",
                     progress = TRUE,
                     final_nrep = 100,
                     seed = 1234)

tmp3
summary(tmp3)
plot(tmp3)



})