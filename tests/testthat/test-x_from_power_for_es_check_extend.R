skip("Internal")

library(testthat)

test_that("es_region_from_power: check_extend: short", {

# A case the failed in previous versions due to invalid
# ranges.

mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: m
y ~ m: m
y ~ x: m
"

options(power4mome.bz = TRUE)

out <- power4test(nrep = 50,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = "y~m"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = FALSE)

out1 <- x_from_power(
            out,
            x = "es",
            pop_es_name = "y ~ m",
            what = "ub",
            target_power = .80,
            seed = 1234,
            progress = TRUE,
            simulation_progress = TRUE)
# plot(out1)

out1b <- x_from_power(
            out1,
            seed = 1234,
            progress = TRUE,
            simulation_progress = TRUE)
# plot(out1b)

out2 <- x_from_power(
            out1,
            what = "lb",
            seed = 4567)
# plot(out2)

})