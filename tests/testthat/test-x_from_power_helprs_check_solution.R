library(testthat)

test_that("Check solution in objects", {

p_ci <- function(x,
                 nrep,
                 ci_level) {
  a <- abs(stats::qnorm((1 - ci_level) / 2))
  se <- sqrt(x * (1 - x) / nrep)
  cilb <- x - a * se
  ciub <- x + a * se
  c(cilb, ciub)
}


reject_out <- data.frame(nrep = c(60, 60, 50),
                         reject = c(.60, .70, .80))
reject_out[, c("cilo", "cihi")] <- p_ci(reject_out$reject,
                                        reject_out$nrep,
                                        ci_level = .95)
class(reject_out) <- "rejection_rates_df"

chk1 <- check_solution_in_by_x(
  reject_out,
  target_power = .80,
  final_nrep = 60,
  ci_level = .95,
  what = "point",
  tol = 1e-2,
  goal = "ci_hit"
)

expect_equal(chk1,
             c(FALSE, TRUE, FALSE))

chk2 <- check_solution_in_by_x(
  reject_out,
  target_power = .60,
  ci_level = .95,
  final_nrep = 60,
  what = "point",
  tol = 1e-2,
  goal = "ci_hit"
)

expect_equal(chk2,
             c(TRUE, TRUE, FALSE))

chk3 <- check_solution_in_by_x(
  reject_out,
  target_power = .890,
  final_nrep = 50,
  ci_level = .95,
  what = "ub",
  tol = 1e-2,
  goal = "close_enough"
)

expect_equal(chk3,
             c(FALSE, FALSE, FALSE))

chk4 <- check_solution_in_by_x(
  reject_out,
  target_power = .58,
  ci_level = .95,
  final_nrep = 60,
  what = "lb",
  tol = 1e-2,
  goal = "close_enough"
)

expect_equal(chk4,
             c(FALSE, TRUE, FALSE))

})
