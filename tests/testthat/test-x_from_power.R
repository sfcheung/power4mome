skip("A long test with parallel processing. Test interactively.")

# This test can be removed when
# x_from_power is ready and
# n_from_power is changed to a wrapper of x_from_power

library(testthat)

test_that("x_from_power", {

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

tmp1a <- n_from_power(out,
                      progress = TRUE,
                      final_nrep = 100,
                      seed = 1234)
tmp1a

tmp1b <- x_from_power(out,
                      x = "n",
                      progress = TRUE,
                      final_nrep = 100,
                      seed = 1234)
tmp1b

expect_equal(tmp1a$n_final,
             tmp1b$x_final)
expect_equal(tmp1a$n_tried,
             tmp1b$x_tried)

summary(tmp1a)
summary(tmp1b)

plot(tmp1a)
plot(tmp1b)

plot(tmp1a,
     lwd = 4,
     col = "green",
     what = c("ci",
              "final_n",
              "final_power"))
plot(tmp1b,
     lwd = 4,
     col = "green",
     what = c("ci",
              "final_x",
              "final_power"))
plot(tmp1a,
     lwd = 4,
     col = "darkgreen",
     what = c("ci",
              "final_n",
              "final_power"),
     pars_ci_final_sample_size = list(lwd = 10),
     pars_final_sample_size = list(lwd = 2, col = "red"))
plot(tmp1b,
     lwd = 4,
     col = "darkgreen",
     what = c("ci",
              "final_x",
              "final_power"),
     pars_ci_final_x = list(lwd = 10),
     pars_final_x = list(lwd = 2, col = "red"))

plot(tmp1a,
     pars_power_curve = list(col = "blue", lwd = 4, type = "o"))
plot(tmp1b,
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

tmp2a <- n_from_power(out2,
                      progress = TRUE,
                      final_nrep = 100,
                      seed = 1234)
tmp2a

tmp2b <- x_from_power(out2,
                      x = "n",
                      progress = TRUE,
                      final_nrep = 100,
                      seed = 1234)
tmp2a

expect_equal(tmp2a$n_final,
             tmp2b$x_final)
expect_equal(tmp2a$n_tried,
             tmp2b$x_tried)

summary(tmp2a)
summary(tmp2b)

plot(tmp2a)
plot(tmp2b)

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

tmp3b <- n_from_power(out3,
                      progress = TRUE,
                      final_nrep = 100,
                      seed = 1234)
tmp3a

tmp3b <- x_from_power(out3,
                      x = "n",
                      progress = TRUE,
                      final_nrep = 100,
                      seed = 1234)
tmp3b

expect_equal(tmp2a$n_final,
             tmp2b$x_final)
expect_equal(tmp2a$n_tried,
             tmp2b$x_tried)

summary(tmp3a)
summary(tmp3b)

plot(tmp3a)
plot(tmp3b)

})