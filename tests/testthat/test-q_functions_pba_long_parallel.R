skip("A long test with parallel processing. Test interactively")

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Parallel mediation =====

system.time(
outp <- q_power_mediation_parallel(
    as = c("s", "m"),
    bs = c("m", "s"),
    cp = "n",
    n = 50,
    final_nrep = 2000,
    R = 199,
    seed = 2345,
    mode = "n",
    progress = progress0,
    parallel = TRUE,
    algorithm = "probabilistic_bisection"
  )
)
# plot(outp)
expect_no_error(capture.output(print(outp)))
expect_no_error(summary(outp))


})
