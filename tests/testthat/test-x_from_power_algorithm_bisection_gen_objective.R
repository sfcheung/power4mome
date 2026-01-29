library(testthat)

test_that("gen_objective", {

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
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  progress = !is_testing(),
                  iseed = 1234)

f0 <- gen_objective(out,
                    x = "n",
                    pop_es_name = NULL,
                    target_power = .80,
                    ci_level = .95,
                    progress = !is_testing(),
                    digits = 3,
                    nrep = 20,
                    R = NULL,
                    what = "point",
                    simulation_progress = !is_testing(),
                    save_sim_all = TRUE,
                    store_output = TRUE)
set.seed(1234)
f_i <- f0(20)
as.numeric(f_i)
output_i <- attr(f_i, "output")
expect_identical(as.numeric(f_i),
                 rejection_rates(output_i)$reject - .80)

set.seed(1234)
f_i <- f0(20,
          what = "lb")
as.numeric(f_i)
output_i <- attr(f_i, "output")
rejection_rates(output_i)$reject_ci_lo
expect_identical(as.numeric(f_i),
                 rejection_rates(output_i)$reject_ci_lo - .80)

set.seed(1234)
f_i <- f0(20,
          what = "ub")
as.numeric(f_i)
output_i <- attr(f_i, "output")
rejection_rates(output_i)$reject_ci_hi
expect_identical(as.numeric(f_i),
                 rejection_rates(output_i)$reject_ci_hi - .80)

# With power_i or output_i

expect_equal(f0(20, out_i = output_i, what = "ub", progress = !is_testing()),
             as.numeric(f_i))
expect_equal(f0(20, power_i = rejection_rates(output_i)$reject, what = "ub", progress = !is_testing()),
             as.numeric(f_i))


})
