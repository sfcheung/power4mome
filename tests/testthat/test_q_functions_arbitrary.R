skip_on_cran()

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Arbitrary model =====

options(power4mome.bz = TRUE)
model <-
"
m1 ~ x
m21 ~ m1
m22 ~ m1
y ~ m21 + m22
"
pop_es <-
"
m1 ~ x: m
m21 ~ m1: m
m22 ~ m1: m
y ~ m21: m
y ~ m22: m
"

system.time(
outa <- q_power_mediation(
    model = model,
    pop_es = pop_es,
    n = 100,
    R = 199,
    test_fun = test_indirect_effect,
    test_more_args = list(x = "x",
                          y = "y",
                          m = c("m1", "m21")),
    seed = 1234,
    mode = "region",
    nrep = 10,
    max_trials = 2,
    parallel = FALSE,
    progress = progress0,
    simulation_progress = progress0,
    tolerance = .20
  )
)
expect_no_error(capture.output(print(outa)))
expect_no_error(summary(outa))
expect_equal(outa$n_region_from_power$above$args$final_nrep,
             10)

})
