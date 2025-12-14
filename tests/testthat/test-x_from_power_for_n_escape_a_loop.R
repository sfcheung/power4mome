skip("Internal: Only valid under a specific configuration")

library(testthat)

test_that("x_from_power: escape a loop", {

options(power4mome.bz = TRUE)

mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: l
y ~ m: m
y ~ x: s
"

out <- power4test(nrep = 400,
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
                  iseed = 2345,
                  parallel = TRUE)

n_power_region <- n_region_from_power(out,
                                      seed = 1357)
n_power_region
plot(n_power_region)

tmp <- n_power_region$above$power4test_trials
class(tmp)
tmp2 <- tmp[1:7]
class(tmp2) <- class(tmp)

tmp3 <- power_curve(
          tmp2,
          models = "nls",
          verbose = TRUE
        )
plot(tmp3)

})