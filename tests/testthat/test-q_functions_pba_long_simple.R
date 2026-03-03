skip("A long test with parallel processing. Test interactively")

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Simple mediation =====

system.time(
out <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    final_nrep = 2000,
    R = 199,
    seed = 1234,
    mode = "n",
    progress = progress0,
    parallel = TRUE
  )
)
expect_no_error(capture.output(print(out)))
expect_no_error(summary(out))

})
