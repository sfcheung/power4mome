skip_on_cran()

test_that("q_power_mediation", {

progress0 <- !testthat::is_testing()

# ===== Serial mediation =====

system.time(
outs <- tryCatch(q_power_mediation_serial(
    ab = c("m", "m", "l"),
    ab_others = "nil",
    cp = "nil",
    n = 80,
    final_nrep = 60,
    R = 99,
    seed = 1234,
    mode = "n",
    x_interval = c(80, 130),
    progress = progress0,
    parallel = FALSE
  ),
  error = function(e) e)
)
if (!inherits(outs, "error")) {
expect_no_error(capture.output(print(outs)))
expect_no_error(summary(outs))
}

})
