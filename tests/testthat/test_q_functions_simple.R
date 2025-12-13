skip_on_cran()

test_that("q_power_mediation", {

# ===== Simple mediation =====

options(power4mome.bz = TRUE)
system.time(
out <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    nrep = 5,
    R = 101,
    seed = 1234,
    mode = "region",
    max_trials = 1,
    progress = FALSE,
    parallel = FALSE,
    simulation_progress = FALSE
  )
)
expect_no_error(capture.output(print(out)))
expect_no_error(summary(out))
expect_equal(out$n_region_from_power$below$args$final_nrep,
             5)

})
