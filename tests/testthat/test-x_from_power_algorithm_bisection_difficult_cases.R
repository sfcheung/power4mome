skip("Internal")

library(testthat)

test_that("bisection: difficult cases", {

mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: m
y ~ m: l
y ~ x: nil
"

out <- power4test(nrep = 2,
                  model = mod,
                  pop_es = mod_es,
                  n = 50000,
                  iseed = 1234)

out <- power4test(nrep = 100,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  R = 1000,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 1234,
                  parallel = TRUE)

out_es_a <- x_from_power(out,
                         x = "es",
                         pop_es_name = "m~x",
                         target_power = .80,
                         final_nrep = 400,
                         seed = 4567,
                         algorithm = "power_curve",
                         goal = "ci_hit")
summary(out_es_a)
plot(out_es_a)

out_es_b <- x_from_power(out,
                         x = "es",
                         pop_es_name = "y~m",
                         target_power = .80,
                         final_nrep = 400,
                         seed = 4567,
                         algorithm = "power_curve",
                         goal = "ci_hit")

summary(out_es_b)

out_es_b <- x_from_power(out,
                         x = "es",
                         pop_es_name = "y~m",
                         target_power = .80,
                         final_nrep = 400,
                         seed = 4567,
                         algorithm = "power_curve",
                         goal = "ci_hit")

summary(out_es_b)


out_es_ab <- x_from_power(out,
                         x = "es",
                         pop_es_name = ".ind.(x->m->y)",
                         target_power = .80,
                         final_nrep = 400,
                         algorithm = "power_curve",
                         goal = "ci_hit",
                         seed = 4567)

summary(out_es_ab)

out_by_es_ab <- power4test_by_es(out,
                                 pop_es_name = ".ind.(x->m->y)",
                                 pop_es_values = seq(.05, .15, .01))
rejection_rates(out_by_es_ab)

# Close enough


mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: m
y ~ m: l
y ~ x: nil
"

out <- power4test(nrep = 2,
                  model = mod,
                  pop_es = mod_es,
                  n = 50000,
                  iseed = 1234)

out <- power4test(nrep = 100,
                  model = mod,
                  pop_es = mod_es,
                  n = 50,
                  R = 1000,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 1234,
                  parallel = TRUE)


out_es_a <- x_from_power(out,
                         x = "es",
                         pop_es_name = "m~x",
                         target_power = .80,
                         final_nrep = 400,
                         seed = 4567,
                         what = "ub",
                         goal = "close_enough")

summary(out_es_a)
plot(out_es_a)

# Try Muller

out_es_a <- x_from_power(out,
                         x = "es",
                         pop_es_name = "m~x",
                         target_power = .75,
                         final_nrep = 400,
                         seed = 2345,
                         what = "ub",
                         goal = "close_enough",
                         control = list(variants = list(muller = TRUE)))

summary(out_es_a)
plot(out_es_a)

out_es_b <- x_from_power(out,
                         x = "es",
                         pop_es_name = "m~x",
                         target_power = .80,
                         final_nrep = 400,
                         seed = 4567,
                         what = "lb",
                         goal = "close_enough")

summary(out_es_b)
plot(out_es_b)

# Try Muller

out_es_b <- x_from_power(out,
                         x = "es",
                         pop_es_name = "m~x",
                         target_power = .80,
                         final_nrep = 400,
                         seed = 4567,
                         what = "lb",
                         goal = "close_enough",
                         control = list(variants = list(muller = TRUE)))


summary(out_es_b)
plot(out_es_b)


mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: s
y ~ m: s
y ~ x: nil
"

out <- power4test(nrep = 2,
                  model = mod,
                  pop_es = mod_es,
                  n = 50000,
                  iseed = 1234)

out <- power4test(nrep = 100,
                  model = mod,
                  pop_es = mod_es,
                  n = 50,
                  R = 1000,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 1234,
                  parallel = TRUE)

out_es_b <- x_from_power(out,
                         x = "n",
                         target_power = .80,
                         final_nrep = 400,
                         seed = 4567,
                         what = "lb",
                         goal = "close_enough",
                         control = list(variants = list(muller = TRUE)))

out_es_b <- x_from_power(out,
                         x = "n",
                         target_power = .80,
                         final_nrep = 400,
                         seed = 4567,
                         what = "lb",
                         goal = "close_enough")


summary(out_es_b)
plot(out_es_b)

})
