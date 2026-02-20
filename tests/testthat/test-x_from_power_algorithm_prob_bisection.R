skip("WIP")

skip("A long test with parallel processing. Test interactively.")

library(testthat)

test_that("probabilistic bisection: n", {

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

####### n

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

# Close enough

set.seed(258)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
(x_lo <- q_dfun(a_out$dfun_out, .05))
(x_hi <- q_dfun(a_out$dfun_out, .95))
plot(a_out$fit_1)
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$x_history, type = "l")
abline(h = x_tmp, col = "blue", lwd = 4)
abline(h = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$f_history, type = "l")
abline(h = 0, col = "blue", lwd = 4)
plot(a_out$dfun_out, type = "l")
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$fit_1,
     xlim = c(x_lo * .9, x_hi * 1.1))
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
hdi_h <- a_out$hdi_power_history
tmp <- sapply(hdi_h,
           \(x) ifelse(length(x) == 1,
                       diff(x[[1]]),
                       NA)
          )
tmp <= .04

tmp_out <- power4test(
              out,
              n = x_tmp,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)

# Close enough

set.seed(1432)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  goal = "close_enough",
                                  final_nrep = 2000)
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
(x_lo <- q_dfun(a_out$dfun_out, .05))
(x_hi <- q_dfun(a_out$dfun_out, .95))
plot(a_out$fit_1)
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$x_history, type = "l")
abline(h = x_tmp, col = "blue", lwd = 4)
abline(h = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$f_history, type = "l")
abline(h = 0, col = "blue", lwd = 4)
plot(a_out$dfun_out, type = "l")
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$fit_1,
     xlim = c(x_lo * .9, x_hi * 1.1))
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")

tmp_out <- power4test(
              out,
              n = x_tmp,
              nrep = 2000,
              iseed = 1479)
rejection_rates(tmp_out)

# ub

set.seed(369)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(50, 2000),
                                  what = "ub",
                                  goal = "close_enough")
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
(x_lo <- q_dfun(a_out$dfun_out, .05))
(x_hi <- q_dfun(a_out$dfun_out, .95))
plot(a_out$fit_1)
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$x_history, type = "l")
abline(h = x_tmp, col = "blue", lwd = 4)
abline(h = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$f_history, type = "l")
abline(h = 0, col = "blue", lwd = 4)
plot(a_out$dfun_out, type = "l")
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$fit_1,
     xlim = c(x_lo * .9, x_hi * 1.1))
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
hdi_h <- a_out$hdi_power_history
tmp <- sapply(hdi_h,
           \(x) ifelse(length(x) == 1,
                       diff(x[[1]]),
                       NA)
          )
tmp <= .04

tmp_out <- power4test(
              out,
              n = x_tmp,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)

# lb

set.seed(271828)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  what = "lb",
                                  goal = "close_enough")
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
(x_lo <- q_dfun(a_out$dfun_out, .05))
(x_hi <- q_dfun(a_out$dfun_out, .95))
plot(a_out$fit_1)
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$x_history, type = "l")
abline(h = x_tmp, col = "blue", lwd = 4)
abline(h = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$f_history, type = "l")
abline(h = 0, col = "blue", lwd = 4)
plot(a_out$dfun_out, type = "l")
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$fit_1,
     xlim = c(x_lo * .9, x_hi * 1.1))
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
hdi_h <- a_out$hdi_power_history
tmp <- sapply(hdi_h,
           \(x) ifelse(length(x) == 1,
                       diff(x[[1]]),
                       NA)
          )
tmp <= .04

tmp_out <- power4test(
              out,
              n = x_tmp,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)


# Solution already in interval

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(775, 800))
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "n",
                                   by_x_1 = by_x_1,
                                   x_interval = c(600, 775))
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)


####### es

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

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "m~x",
                                  by_x_1 = by_x_1,
                                  variants = list(nrep_step = 0))
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
(x_lo <- q_dfun(a_out$dfun_out, .05))
(x_hi <- q_dfun(a_out$dfun_out, .95))
plot(a_out$fit_1)
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$x_history, type = "l")
abline(h = x_tmp, col = "blue", lwd = 4)
abline(h = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$f_history, type = "l")
abline(h = 0, col = "blue", lwd = 4)
plot(a_out$dfun_out, type = "l")
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$fit_1,
     xlim = c(x_lo * .9, x_hi * 1.1))
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")

tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)

# Close enough

set.seed(1234)
a_out <- power_algorithm_bisection(object = out,
                                   x = "es",
                                   pop_es_name = "m~x",
                                   by_x_1 = by_x_1,
                                   goal = "close_enough",
                                   tol = .30)
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80)

# ub

set.seed(12345)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "m~x",
                                  by_x_1 = by_x_1,
                                  what = "ub",
                                  goal = "close_enough",
                                  max_trials = 50,
                                  trial_nrep = 100,
                                  final_nrep = 2000,
                                  tol = .005,
                                  variants = list(npoints = 200))
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
(x_lo <- q_dfun(a_out$dfun_out, .05))
(x_hi <- q_dfun(a_out$dfun_out, .95))
plot(a_out$fit_1)
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$x_history, type = "l")
abline(h = x_tmp, col = "blue", lwd = 4)
abline(h = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$f_history, type = "l")
abline(h = 0, col = "blue", lwd = 4)
plot(a_out$dfun_out, type = "l")
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$fit_1,
     xlim = c(x_lo * .9, x_hi * 1.1))
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")

tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)


# lb

set.seed(123456)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "m~x",
                                  by_x_1 = by_x_1,
                                  what = "lb",
                                  goal = "close_enough",
                                  max_trials = 50,
                                  trial_nrep = 100,
                                  final_nrep = 2000,
                                  tol = .005,
                                  variants = list(npoints = 200))
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
(x_lo <- q_dfun(a_out$dfun_out, .05))
(x_hi <- q_dfun(a_out$dfun_out, .95))
plot(a_out$fit_1)
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$x_history, type = "l")
abline(h = x_tmp, col = "blue", lwd = 4)
abline(h = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$f_history, type = "l")
abline(h = 0, col = "blue", lwd = 4)
plot(a_out$dfun_out, type = "l")
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
plot(a_out$fit_1,
     xlim = c(x_lo * .9, x_hi * 1.1))
abline(h = .80, col = "blue", lwd = 4)
abline(v = x_tmp, col = "red", lwd = 4)
abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")

tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)

# Solution already in interval

})
