skip_on_cran()

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Parallel mediation =====

system.time(
outp <- q_power_mediation_parallel(
    as = c("m", "m"),
    bs = c("m", "m"),
    cp = "n",
    n = 125,
    final_nrep = 60,
    R = 99,
    seed = 2345,
    mode = "n",
    x_interval = c(125, 155),
    progress = progress0,
    parallel = FALSE
  )
)
if (!inherits(outp, "error")) {
expect_no_error(capture.output(print(outp)))
expect_no_error(summary(outp))
}

})
