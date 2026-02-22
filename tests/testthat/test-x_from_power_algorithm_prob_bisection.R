skip("WIP")

skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("probabilistic bisection: n", {

# ==== Diagnostic function ====

diag <- function(a_out) {
  (x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
  (x_lo <- q_dfun(a_out$dfun_out, .05))
  (x_hi <- q_dfun(a_out$dfun_out, .95))
  parold <- par(no.readonly = TRUE)
  layout(matrix(1:6, nrow = 3, byrow = TRUE))
  plot(a_out$fit_1)
  abline(h = .80, col = "blue", lwd = 1)
  abline(h = a_out$f_power, col = "red", lwd = 1)
  abline(v = x_tmp, col = "red", lwd = 1)
  abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  plot(a_out$x_history, type = "l")
  abline(h = x_tmp, col = "blue", lwd = 1)
  abline(h = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  plot(a_out$f_history, type = "l")
  abline(h = 0, col = "blue", lwd = 1)
  tmp <- c(q_dfun(a_out$dfun_out, .01),
           q_dfun(a_out$dfun_out, .99))
  plot(a_out$dfun_out, type = "l", xlim = tmp)
  abline(v = x_tmp, col = "red", lwd = 1)
  abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  plot(a_out$fit_1,
      xlim = c(x_lo * .9, x_hi * 1.1))
  abline(h = .80, col = "blue", lwd = 1)
  abline(h = a_out$f_power, col = "red", lwd = 1)
  abline(v = x_tmp, col = "red", lwd = 1)
  abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  hdr_power_history <- a_out$hdr_power_history
  tmp <- sapply(unlist(hdr_power_history, recursive = FALSE),
                range)
  plot(seq_along(hdr_power_history),
       y = rep(a_out$f_power, length(hdr_power_history)),
       ylim = range(tmp),
       type = "l")
  # points(a_out$f_power + a_out$f_history)
  points(a_out$reject_by_power_curve_history)
  for (i in seq_along(hdr_power_history)) {
    for (y in hdr_power_history[[i]]) {
      arrows(
          x0 = i,
          y0 = y[1],
          x1 = i,
          y1 = y[2],
          lty = "dotted",
          code = 3,
          angle = 90,
          length = .05
        )
    }
  }
  par(parold)
  hdr_h <- a_out$hdr_power_history
  tmp <- sapply(hdr_h,
            \(x) ifelse(length(x) == 1,
                        diff(x[[1]]),
                        NA)
            )
  tmp2 <- sapply(hdr_h,
            \(x) ifelse(length(x) == 1,
                        (x[[1]][1] <= a_out$f_power) &
                        (x[[1]][1] >= a_out$f_power),
                        NA)
            )
  print(tmp <= a_out$hdr_power_tol)
}


mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: s
y ~ m: m
y ~ x: s
"

# ==== n ====

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  parallel = FALSE,
                  iseed = 1234)

by_x_1 <- power4test_by_n(out,
                          n = 90)

## ==== Close enough ====

set.seed(4321)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

tmp_out <- power4test(
              out,
              n = a_out$x_out,
              nrep = 2000,
              iseed = 147)
rejection_rates(tmp_out)

## ==== ub ====

set.seed(2345)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  what = "ub",
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

tmp_out <- power4test(
              out,
              n = a_out$x_out,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== lb ====

set.seed(271828)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  what = "lb",
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

tmp_out <- power4test(
              out,
              n = a_out$x_out,
              nrep = 2000,
              iseed = 12345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

# Solution already in interval

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(775, 800))
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(600, 775))
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)


# ==== es ====

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  iseed = 1234)

by_x_1 <- power4test_by_es(out,
                           pop_es_name = "m~x",
                           pop_es_values = c(.10))

## ==== Close enough ====

set.seed(345)
a_out <- power_algorithm_prob_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   goal = "close_enough",
                                   x_interval = c(.10, .40),
                                   final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

(tmp_es <- setNames(a_out$x_out, "m~x"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== ub ====

set.seed(3579)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "m~x",
                                  by_x_1 = by_x_1,
                                  what = "ub",
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

(tmp_es <- setNames(a_out$x_out, "m~x"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== lb ====

set.seed(123456)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "m~x",
                                  by_x_1 = by_x_1,
                                  x_interval = c(.10, .50),
                                  what = "lb",
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

(tmp_es <- setNames(a_out$x_out, "m~x"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

})
