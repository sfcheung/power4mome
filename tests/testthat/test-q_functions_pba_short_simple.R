skip_on_cran()

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Simple mediation =====

system.time(
out <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 100,
    final_nrep = 60,
    R = 99,
    seed = 1234,
    mode = "n",
    x_interval = c(100, 120),
    progress = progress0,
    parallel = FALSE
  )
)
if (!inherits(out, "error")) {
expect_no_error(capture.output(print(out)))
expect_no_error(summary(out))
}
})
