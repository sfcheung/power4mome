skip("WIP")

skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("probabilistic bisection: n, indirect", {

options(power4mome.bz = TRUE)

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

out <- power4test(nrep = 50,
                  model = mod,
                  pop_es = mod_es,
                  n = 50,
                  R = 199,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE,
                                   test_method = "pvalue"),
                  iseed = 1234,
                  parallel = TRUE)

rejection_rates(out)

out_tmp <- power4test(out,
                      n = 2000)
rejection_rates(out_tmp)

by_x_1 <- power4test_by_n(out,
                          n = 90)
rejection_rates(by_x_1)

out <- power4test(nrep = 20,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(par = "m~x"),
                  parallel = FALSE,
                  iseed = 1234)

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
  plot(a_out$dfun_out, type = "l")
  abline(v = x_tmp, col = "red", lwd = 1)
  abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  plot(a_out$fit_1,
      xlim = c(x_lo * .9, x_hi * 1.1))
  abline(h = .80, col = "blue", lwd = 1)
  abline(h = a_out$f_power, col = "red", lwd = 1)
  abline(v = x_tmp, col = "red", lwd = 1)
  abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  par(parold)
  hdr_h <- a_out$hdr_power_history
  tmp <- sapply(hdr_h,
            \(x) ifelse(length(x) == 1,
                        diff(x[[1]]),
                        NA)
            )
  print(tmp <= .04)
}

## ==== Close enough ====

set.seed(1234)
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
              n = ceiling(q_dfun(a_out$dfun_out)),
              R = 1000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)

## ==== ub ====

set.seed(1234)
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
              n = ceiling(q_dfun(a_out$dfun_out)),
              R = 1000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== lb ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  what = "lb",
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

tmp_out <- power4test(
              out,
              n = ceiling(q_dfun(a_out$dfun_out)),
              R = 1000,
              nrep = 2000,
              iseed = 2345)
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

mod <-
"
m ~ x
y ~ m + x
"

mod_es <-
"
m ~ x: l
y ~ m: m
y ~ x: s
"

out <- power4test(nrep = 50,
                  model = mod,
                  pop_es = mod_es,
                  n = 50,
                  R = 199,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE,
                                   test_method = "pvalue"),
                  iseed = 1234,
                  parallel = TRUE)
rejection_rates(out)

by_x_1 <- power4test_by_es(out,
                           pop_es_name = "y~m",
                           pop_es_values = c(.10))

## ==== Close enough ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 199,
                                  by_x_1 = by_x_1,
                                  x_interval = c(.00, .50),
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

(tmp_es <- setNames(q_dfun(a_out$dfun_out), "y~m"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 1000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== ub ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 199,
                                  by_x_1 = by_x_1,
                                  what = "ub",
                                  x_interval = c(.30, .60),
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

(tmp_es <- setNames(q_dfun(a_out$dfun_out), "y~m"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 1000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

## ==== lb ====

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 199,
                                  by_x_1 = by_x_1,
                                  what = "lb",
                                  x_interval = c(.30, .60),
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
diag(a_out)

(tmp_es <- setNames(q_dfun(a_out$dfun_out), "y~m"))
tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 2000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)
a_out$f_power
a_out$f_what
a_out$f_goal

})
