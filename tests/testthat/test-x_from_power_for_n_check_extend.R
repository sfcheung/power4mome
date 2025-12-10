skip("Internal")

library(testthat)

test_that("n_region_from_power: check_extend: short", {

mod <-
"
m ~ x
y ~ m + x
"

pop_es <-
"
m ~ x: m
y ~ m: m
y ~ x: m
"

options(power4mome.bz = TRUE)
out <- power4test(
          nrep = 400,
          model = mod,
          pop_es = pop_es,
          n = 110,
          reliability = .70,
          number_of_indicators = 6,
          R = 199,
          ci_type = "mc",
          test_fun = test_indirect_effect,
          test_args = list(x = "x",
                            m = "m",
                            y = "y",
                            mc_ci = TRUE
                          ),
          iseed = 1234,
          parallel = TRUE,
          ncores = max(1, parallel::detectCores(logical = FALSE) - 1)
        )

out1 <- n_from_power(out,
                     x_interval = c(110, 300),
                     what = "ub",
                     seed = 1234)
tmp <- out1
tmp$what <- "lb"
tmp$goal <- "close_enough"
tmp$solution_found <- FALSE
tmp$call$what <- "lb"
tmp$call$goal <- "close_enough"
out2 <- n_from_power(tmp,
                     what = "lb",
                     seed = 4567)

})

test_that("n_region_from_power: check_extend", {

mod <-
"
m ~ x
y ~ m + x
"

pop_es <-
"
m ~ x: m
y ~ m: m
y ~ x: m
"

options(power4mome.bz = TRUE)
out <- power4test(
          nrep = 400,
          model = mod,
          pop_es = pop_es,
          n = 50,
          reliability = .70,
          number_of_indicators = 6,
          R = 199,
          ci_type = "mc",
          test_fun = test_indirect_effect,
          test_args = list(x = "x",
                            m = "m",
                            y = "y",
                            mc_ci = TRUE
                          ),
          iseed = 1234,
          parallel = TRUE,
          ncores = max(1, parallel::detectCores(logical = FALSE) - 1)
        )

out2 <- n_region_from_power(
          out,
          target_power = .80,
          progress = TRUE,
          simulation_progress = TRUE,
          max_trials = 10,
          seed = 1234
        )
summary(out2)
plot(out2)
})