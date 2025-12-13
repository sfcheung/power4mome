skip_on_cran()

test_that("q_power_mediation", {

# ===== Serial mediation =====

options(power4mome.bz = TRUE)
system.time(
outs <- q_power_mediation_serial(
    ab = c("s", "m", "l"),
    ab_others = "n",
    cp = "s",
    n = 50,
    nrep = 4,
    R = 101,
    seed = 1234,
    mode = "region",
    max_trials = 1,
    progress = FALSE,
    parallel = FALSE,
    simulation_progress = FALSE
  )
)
expect_no_error(capture.output(print(outs)))
expect_no_error(summary(outs))
expect_equal(outs$n_region_from_power$below$args$final_nrep,
             4)

})
