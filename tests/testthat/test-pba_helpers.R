test_that("p_c", {

tmp1 <- p_c0(
    target_power = .80,
    power_i = .70,
    trial_nrep = 50
  )
tmp1 <- 1 - (1 - tmp1) * 2

tmp2 <- reject_ci_wilson(
          nreject = .70 * 50,
          nvalid = 50,
          level = tmp1
        )
expect_all_equal(tmp2[, "cihi"],
                 .80)

tmp1 <- p_c0(
    target_power = .80,
    power_i = .85,
    trial_nrep = 100
  )
tmp1 <- 1 - (1 - tmp1) * 2

tmp2 <- reject_ci_wilson(
          nreject = .85 * 100,
          nvalid = 100,
          level = tmp1
        )
expect_all_equal(tmp2[, "cilo"],
                 .80)

tmp1 <- p_c(
    target_power = .80,
    power_i = .85,
    trial_nrep = 100,
    goal = "close_enough",
    what = "point"
  )
tmp1 <- 1 - (1 - tmp1) * 2

tmp2 <- reject_ci_wilson(
          nreject = .85 * 100,
          nvalid = 100,
          level = tmp1
        )
expect_all_equal(tmp2[, "cilo"],
                 .80)

# ==== ub =====

tmp1 <- p_c(
    target_power = .80,
    power_i = .75,
    trial_nrep = 100,
    goal = "close_enough",
    what = "ub",
    final_nrep = 2000
  )
tmp1 <- 1 - (1 - tmp1) * 2

tmp3 <- target_power_adjusted(
    target_power = .80,
    goal = "close_enough",
    what = "ub",
    nrep = 2000
  )

tmp2 <- reject_ci_wilson(
          nreject = .75 * 100,
          nvalid = 100,
          level = tmp1
        )
expect_equal(
          unname(tmp2[, "cihi"]),
          tmp3,
          tolerance = 1e-2)


tmp1 <- p_c(
    target_power = .80,
    power_i = .79,
    trial_nrep = 100,
    goal = "close_enough",
    what = "ub",
    final_nrep = 2000
  )
tmp1 <- 1 - (1 - tmp1) * 2

tmp3 <- target_power_adjusted(
    target_power = .80,
    goal = "close_enough",
    what = "ub",
    nrep = 2000
  )

tmp2 <- reject_ci_wilson(
          nreject = .79 * 100,
          nvalid = 100,
          level = tmp1
        )
expect_equal(
          unname(tmp2[, "cilo"]),
          tmp3,
          tolerance = 1e-2)

# ==== lb =====

tmp1 <- p_c(
    target_power = .80,
    power_i = .85,
    trial_nrep = 100,
    goal = "close_enough",
    what = "lb",
    final_nrep = 2000
  )
tmp1 <- 1 - (1 - tmp1) * 2

tmp3 <- target_power_adjusted(
    target_power = .80,
    goal = "close_enough",
    what = "lb",
    nrep = 2000
  )

tmp2 <- reject_ci_wilson(
          nreject = .85 * 100,
          nvalid = 100,
          level = tmp1
        )
expect_equal(
          unname(tmp2[, "cilo"]),
          tmp3,
          tolerance = 1e-2)


tmp1 <- p_c(
    target_power = .80,
    power_i = .70,
    trial_nrep = 100,
    goal = "close_enough",
    what = "lb",
    final_nrep = 2000
  )
tmp1 <- 1 - (1 - tmp1) * 2

tmp3 <- target_power_adjusted(
    target_power = .80,
    goal = "close_enough",
    what = "lb",
    nrep = 2000
  )

tmp2 <- reject_ci_wilson(
          nreject = .70 * 100,
          nvalid = 100,
          level = tmp1
        )
expect_equal(
          unname(tmp2[, "cihi"]),
          tmp3,
          tolerance = 1e-2)


})
