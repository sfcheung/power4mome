skip_on_cran()

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Simple mediation =====

options(power4mome.bz = TRUE)
system.time(
out <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    nrep = 10,
    R = 199,
    seed = 1234,
    mode = "region",
    max_trials = 1,
    progress = progress0,
    parallel = FALSE,
    simulation_progress = progress0,
    tolerance = .20
  )
)
expect_no_error(capture.output(print(out)))
expect_no_error(summary(out))
expect_equal(out$n_region_from_power$below$args$final_nrep,
             10)

})
