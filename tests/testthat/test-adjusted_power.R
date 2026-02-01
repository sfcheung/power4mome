library(testthat)

test_that("Adjusted target power", {
out <- target_power_adjusted(
          .70,
          goal = "close_enough",
          what = "ub",
          nrep = 1000,
          tolerance = .03)
out
chk <- reject_ci_wilson(
          nreject = ceiling(out * 1000),
          nvalid = 1000,
          level = .95)
expect_equal(chk[2],
             .70,
             tolerance = 1e-2)

out <- target_power_adjusted(
          .70,
          goal = "close_enough",
          what = "lb",
          nrep = 1000,
          tolerance = .03)
out
chk <- reject_ci_wilson(
          nreject = ceiling(out * 1000),
          nvalid = 1000,
          level = .95)
expect_equal(chk[1],
             .70,
             tolerance = 1e-2)

out <- target_power_adjusted(
          .70,
          goal = "close_enough",
          what = "point",
          nrep = 1000,
          tolerance = .03)
out
expect_equal(out,
             .70,
             tolerance = 1e-2)

out <- target_power_adjusted(
          .70,
          goal = "ci_hit",
          what = "point",
          nrep = 1000,
          tolerance = .03)
out
expect_equal(out,
             .70,
             tolerance = 1e-2)
})
