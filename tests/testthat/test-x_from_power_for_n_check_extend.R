skip("Internal")

library(testthat)

test_that("check_extend: Start with very small n", {

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
          n = 51,
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
                     what = "ub",
                     seed = 1234)
plot(out1)

out2 <- n_region_from_power(
          out,
          seed = 1234
        )
plot(out2)

})

test_that("check_extend: Start with very large n", {

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
outb <- power4test(
          nrep = 400,
          model = mod,
          pop_es = pop_es,
          n = 1999,
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

outb1 <- n_from_power(outb,
                     what = "ub",
                     seed = 1234)
plot(outb1)

outb2 <- n_region_from_power(
          outb,
          seed = 1234
        )
plot(outb2)

})
