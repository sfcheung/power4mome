skip("A long test with parallel processing. Test interactively")

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Serial mediation =====

system.time(
outs <- tryCatch(q_power_mediation_serial(
    ab = c("s", "m", "l"),
    ab_others = "n",
    cp = "s",
    n = 50,
    final_nrep = 2000,
    R = 199,
    seed = 1234,
    mode = "n",
    progress = progress0,
    parallel = TRUE,
    algorithm = "probabilistic_bisection"
  ),
  error = function(e) e)
)
if (!inherits(outs, "error")) {
expect_no_error(capture.output(print(outs)))
expect_no_error(summary(outs))

}

})
