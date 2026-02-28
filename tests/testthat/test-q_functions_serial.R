skip_on_cran()

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Serial mediation =====

options(power4mome.bz = TRUE)
system.time(
outs <- tryCatch(q_power_mediation_serial(
    ab = c("s", "m", "l"),
    ab_others = "n",
    cp = "s",
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
  ),
  error = function(e) e)
)
if (!inherits(outs, "error")) {
expect_no_error(capture.output(print(outs)))
expect_no_error(summary(outs))
expect_equal(outs$n_region_from_power$below$args$final_nrep,
             10)
}

})
