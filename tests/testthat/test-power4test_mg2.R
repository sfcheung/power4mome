skip_on_cran()

library(testthat)

test_that("power4test: mg", {

model_simple_med <-
"
m ~ c(a1, a2) * x
y ~ m + x
a := a1 - a2
"

model_simple_med_es <-
"
y ~ m: l
m ~ x:
  - l
  - s
y ~ x: nil
"

# Generate the data

out1 <- power4test(
          nrep = 50,
          model = model_simple_med,
          pop_es = model_simple_med_es,
          n = 200,
          test_fun = test_parameters,
          test_args = list(par = c("a")),
          progress = !is_testing(),
          iseed = 1234,
          n_ratio = c(1, .5)
        )

rejection_rates(out1)

expect_no_error(tmp2 <- n_region_from_power(out1,
                    target_power = .75,
                    final_nrep = 100,
                    seed = 234,
                    progress = !is_testing(),
                    simulation_progress = !is_testing()))

})

