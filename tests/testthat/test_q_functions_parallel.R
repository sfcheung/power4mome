skip_on_cran()

test_that("q_power_mediation", {

# ===== Parallel mediation =====

options(power4mome.bz = TRUE)
system.time(
outp <- q_power_mediation_parallel(
    as = c("s", "m"),
    bs = c("m", "s"),
    cp = "n",
    n = 50,
    nrep = 3,
    R = 101,
    seed = 1234,
    mode = "region",
    max_trials = 2,
    progress = FALSE,
    parallel = FALSE,
    simulation_progress = FALSE
  )
)
expect_no_error(capture.output(print(outp)))
expect_no_error(summary(outp))
expect_equal(outp$n_region_from_power$above$args$final_nrep,
             3)

})
