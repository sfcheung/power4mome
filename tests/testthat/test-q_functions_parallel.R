skip_on_cran()

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Parallel mediation =====

options(power4mome.bz = TRUE)
system.time(
outp <- q_power_mediation_parallel(
    as = c("s", "m"),
    bs = c("m", "s"),
    cp = "n",
    n = 50,
    nrep = 10,
    R = 199,
    seed = 1234,
    mode = "region",
    max_trials = 2,
    progress = progress0,
    parallel = FALSE,
    simulation_progress = progress0,
    tolerance = .20
  )
)
# plot(outp)
expect_no_error(capture.output(print(outp)))
expect_no_error(summary(outp))
expect_equal(outp$n_region_from_power$above$args$final_nrep,
             10)

})
