skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("x_from_power: es", {

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

out <- power4test(nrep = 50,
                  model = mod,
                  pop_es = mod_es,
                  n = 60,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "y~m"),
                  iseed = 1234,
                  parallel = FALSE)
out_power <- rejection_rates(out)
out_power

tmp <- x_from_power(out,
                    x = "es",
                    pop_es_name = "y ~ m",
                    progress = TRUE,
                    final_nrep = 80,
                    max_trials = 4,
                    seed = 1234,
                    algorithm = "power_curve")
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

mod_es <-
"
m ~ x: s
y ~ m: .05
y ~ x: s
"
out2 <- power4test(nrep = 50,
                   model = mod,
                   pop_es = mod_es,
                   n = 60,
                   fit_model_args = list(fit_function = "lm"),
                   test_fun = test_parameters,
                   test_args = list(par = "y~m"),
                   iseed = 1234,
                   parallel = FALSE)

tmp2 <- x_from_power(out2,
                     x = "es",
                     pop_es_name = "y ~ m",
                     progress = TRUE,
                     final_nrep = 80,
                     max_trials = 4,
                     seed = 1234,
                     algorithm = "power_curve")
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
m1 ~ x: m
m2 ~ m1: m
m3 ~ m2: m
y ~ m3: m
y ~ x: l
"

out3 <- power4test(nrep = 50,
                   model = mod,
                   pop_es = mod_es,
                   n = 150,
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

tmp3 <- x_from_power(out3,
                     x = "es",
                     pop_es_name = "m1 ~ x",
                     progress = TRUE,
                     final_nrep = 80,
                     max_trials = 4,
                     x_interval = c(0, .90),
                     seed = 1234,
                     algorithm = "power_curve")

tmp3
summary(tmp3)
plot(tmp3)


})