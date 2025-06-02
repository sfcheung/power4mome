skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("x_from_power: es", {

# Case 1

mod <-
"
m1 ~ x
m2 ~ m1 + x
m3 ~ m2 + m1 + x
y ~ m3 + m2 + m1 + x
"

mod_es <-
"
.ind.(x->m1->m2->m3->y): .05
y ~ x: nil
"

out3 <- power4test(nrep = 50,
                   model = mod,
                   pop_es = mod_es,
                   n = 50,
                   R = 1000,
                   ci_type = "mc",
                   test_fun = test_indirect_effect,
                   test_args = list(x = "x",
                                    m = c("m1", "m2", "m3"),
                                    y = "y",
                                    mc_ci = TRUE),
                   iseed = 1234,
                   parallel = TRUE)

print(out3,
      data_long = TRUE)
rejection_rates(out3, all_columns = TRUE)

tmp3 <- x_from_power(out3,
                     x = "es",
                     pop_es_name = ".ind.(x->m1->m2->m3->y)",
                     progress = TRUE,
                     final_nrep = 80,
                     max_trials = 4,
                     x_interval = c(0, .90),
                     seed = 1234)

tmp3
summary(tmp3)
plot(tmp3)
tmp3$power_curve
power_curve(tmp3$power4test_trials)

})