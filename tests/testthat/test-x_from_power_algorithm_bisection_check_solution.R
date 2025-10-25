library(testthat)

test_that("bisection: n", {

ci_from_f <- function(f_i,
                      nrep,
                      ci_level = .95) {
  a <- abs(stats::qnorm((1 - ci_level) / 2))
  se_i <- sqrt(f_i * (1 - f_i) / nrep)
  cilb <- f_i - a * se_i
  ciub <- f_i + a * se_i
  c(cilb, ciub)
}

# Use Wilson CI
ci_from_f <- function(f_i,
                      nrep,
                      ci_level = .95) {
  reject_ci_wilson(round(f_i * nrep),
                   nrep,
                   ci_level)
}

ci_from_f(.70, 100)
expect_false(check_solution(
               .70,
               target_power = .80,
               nrep = 100,
               final_nrep = 100,
               what = "point",
               tol = 1e-2,
               goal = "ci_hit"
             ))

ci_from_f(.70, 10)
expect_true(check_solution(
              .70,
              target_power = .80,
              nrep = 10,
              final_nrep = 10,
              what = "point",
              tol = 1e-2,
              goal = "ci_hit"
            ))

expect_false(check_solution(
               .70,
               target_power = .80,
               nrep = 10,
               final_nrep = 10,
               what = "point",
               tol = 1e-2,
               goal = "close_enough"
             ))

expect_true(check_solution(
              .79,
              target_power = .80,
              nrep = 10,
              final_nrep = 10,
              what = "point",
              tol = .02,
              goal = "close_enough"
            ))

ci_from_f(.65, 25)
expect_true(check_solution(
              .65,
              target_power = .80,
              nrep = 25,
              final_nrep = 25,
              what = "ub",
              tol = .02,
              goal = "close_enough"
            ))
expect_false(check_solution(
               .60,
               target_power = .80,
               nrep = 10,
               final_nrep = 10,
               what = "ub",
               tol = .02,
               goal = "close_enough"
             ))

ci_from_f(.90, 50)
expect_true(check_solution(
              .90,
              target_power = .80,
              nrep = 50,
              final_nrep = 50,
              what = "lb",
              tol = .02,
              goal = "close_enough"
            ))
expect_false(check_solution(
               .90,
               target_power = .80,
               nrep = 100,
               final_nrep = 100,
               what = "lb",
               tol = .02,
               goal = "close_enough"
            ))

})
