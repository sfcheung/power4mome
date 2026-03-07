skip_on_cran()

test_that("q_power_mediation: R_for_bz", {

progress0 <- !testthat::is_testing()

# ===== Simple mediation =====

system.time(
out1 <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    nrep = 10,
    R = R_for_bz(100),
    seed = 1234,
    mode = "region",
    max_trials = 1,
    progress = progress0,
    parallel = FALSE,
    simulation_progress = progress0,
    tolerance = .20
  )
)

system.time(
out2 <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    nrep = 10,
    R = R_for_bz(200),
    seed = 1234,
    mode = "region",
    max_trials = 1,
    progress = progress0,
    parallel = FALSE,
    simulation_progress = progress0,
    tolerance = .20
  )
)

expect_equal(attr(out1$power4test, "args")$R,
             79)
expect_equal(attr(out2$power4test, "args")$R,
             199)

})
