library(testthat)

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

# ==== HDR ====

test_that("HDR", {

# set.seed(1234)
# x <- rnorm(10000, mean = 500, sd = 10)
# x <- round(x)
# x <- table(x)
# plot(x)

x <- 10:2000
d1 <- dnorm(x, mean = 500, sd = 100)
d2 <- dnorm(x, mean = 1000, sd = 20)
range(d1)
range(d2)
d <- d1 + d2
range(d)
d <- d / sum(d)
range(d)
dfun1 <- cbind(x = x,
               prob = d)
# plot(dfun1)

x <- 10:2000
d1 <- dnorm(x, mean = 500, sd = 100)
range(d1)
d <- d1 / sum(d1)
range(d)
dfun2 <- cbind(x = x,
               prob = d)
# plot(dfun2)

out1 <- hdr(dfun1, prob = .8)
out2 <- hdr(dfun1, prob = .7)

expect_true(length(out1) == 2)
expect_true(length(out2) == 2)

expect_true(diff(out1[[1]]) > diff(out2[[1]]))
expect_true(diff(out1[[2]]) > diff(out2[[2]]))

out1 <- hdr(dfun2, prob = .8)
out2 <- hdr(dfun2, prob = .7)

expect_true(length(out1) == 1)
expect_true(length(out2) == 1)

expect_true(diff(out1[[1]]) > diff(out2[[1]]))


})
