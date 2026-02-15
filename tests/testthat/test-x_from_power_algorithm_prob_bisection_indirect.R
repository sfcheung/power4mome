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

####### n

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


set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  R = 199,
                                  max_trials = 100,
                                  final_nrep = 2000,
                                  last_k = 5,
                                  delta_tol = 5,
                                  variants = list(nrep_step = 0))
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
rejection_rates(a_out$by_x_1)
plot(a_out$fit_1)
abline(h = .80, lwd = 5)
abline(v = x_tmp, lwd = 5, col = "blue")
plot(a_out$dfun_out, type = "l")

tmp_out <- power4test(
              out,
              n = x_tmp,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)

# Close enough

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  x_interval = c(200, 2000),
                                  goal = "close_enough",
                                  max_trials = 100,
                                  final_nrep = 2000,
                                  last_k = 5,
                                  delta_tol = 5,
                                  variants = list(nrep_step = 0))
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
plot(a_out$fit_1)
abline(h = .80)
abline(v = x_tmp)
plot(a_out$x_history, type = "l")
abline(h = q_dfun(a_out$dfun_out))
plot(a_out$dfun_out, type = "l")
q_dfun(a_out$dfun_out, .10)
q_dfun(a_out$dfun_out, .90)


tmp_out <- power4test(
              out,
              n = x_tmp,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)

# ub

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  what = "ub",
                                  goal = "close_enough",
                                  R = 79,
                                  max_trials = 100,
                                  final_nrep = 2000,
                                  last_k = 5,
                                  delta_tol = 5,
                                  variants = list(nrep_step = 0))
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
plot(a_out$fit_1)
abline(h = .80)
abline(v = x_tmp)
plot(a_out$x_history, type = "l")
abline(h = q_dfun(a_out$dfun_out))
plot(a_out$dfun_out, type = "l")
q_dfun(a_out$dfun_out, .10)
q_dfun(a_out$dfun_out, .90)

tmp_out <- power4test(
              out,
              n = x_tmp,
              R = 1000,
              nrep = 2000,
              iseed = 2345)
rejection_rates(tmp_out)

# lb

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  what = "lb",
                                  goal = "close_enough",
                                  R = 79,
                                  max_trials = 100,
                                  final_nrep = 2000,
                                  last_k = 5,
                                  delta_tol = 5,
                                  variants = list(nrep_step = 0))
rejection_rates(a_out$by_x_1)
(x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
plot(a_out$fit_1)
abline(h = .80)
abline(v = x_tmp)
plot(a_out$x_history, type = "l")
points(a_out$x_history)
abline(h = q_dfun(a_out$dfun_out))
plot(a_out$dfun_out, type = "l")
q_dfun(a_out$dfun_out, .10)
q_dfun(a_out$dfun_out, .90)

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
                  R = 79,
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

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 79,
                                  by_x_1 = by_x_1,
                                  x_interval = c(.00, .90),
                                  max_trials = 100,
                                  final_nrep = 2000,
                                  last_k = 5,
                                  delta_tol = .005,
                                  variants = list(nrep_step = 0))
rejection_rates(a_out$by_x_1)
(x_tmp <- q_dfun(a_out$dfun_out, prob = .50))
plot(a_out$fit_1)
abline(h = .80)
abline(v = x_tmp)
plot(a_out$x_history, type = "l")
points(a_out$x_history)
abline(h = q_dfun(a_out$dfun_out))
plot(a_out$dfun_out, type = "l")
q_dfun(a_out$dfun_out, .10)
q_dfun(a_out$dfun_out, .90)
(tmp_es <- setNames(x_tmp, "y~m"))

tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 1000,
              nrep = 2000,
              iseed = 2345,
              parallel = TRUE)
rejection_rates(tmp_out)

# Close enough

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 79,
                                  by_x_1 = by_x_1,
                                  goal = "close_enough",
                                  tol = .005,
                                  x_interval = c(.00, .90),
                                  max_trials = 100,
                                  final_nrep = 2000,
                                  last_k = 5,
                                  delta_tol = .005,
                                  variants = list(nrep_step = 0))
rejection_rates(a_out$by_x_1)
(x_tmp <- q_dfun(a_out$dfun_out, prob = .50))
plot(a_out$fit_1)
abline(h = .80)
abline(v = x_tmp)
plot(a_out$x_history, type = "l")
points(a_out$x_history)
abline(h = q_dfun(a_out$dfun_out))
plot(a_out$dfun_out, type = "l")
q_dfun(a_out$dfun_out, .10)
q_dfun(a_out$dfun_out, .90)
(tmp_es <- setNames(x_tmp, "y~m"))

tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 1000,
              nrep = 2000,
              iseed = 2345,
              parallel = TRUE)
rejection_rates(tmp_out)



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

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 79,
                                  by_x_1 = by_x_1,
                                  what = "ub",
                                  goal = "close_enough",
                                  tol = .005,
                                  x_interval = c(.00, .90),
                                  max_trials = 100,
                                  final_nrep = 2000,
                                  last_k = 5,
                                  delta_tol = .005,
                                  variants = list(nrep_step = 0))
rejection_rates(a_out$by_x_1)
(x_tmp <- q_dfun(a_out$dfun_out, prob = .50))
plot(a_out$fit_1)
abline(h = .80)
abline(v = x_tmp)
plot(a_out$x_history, type = "l")
points(a_out$x_history)
abline(h = q_dfun(a_out$dfun_out))
plot(a_out$dfun_out, type = "l")
q_dfun(a_out$dfun_out, .10)
q_dfun(a_out$dfun_out, .90)
(tmp_es <- setNames(x_tmp, "y~m"))

tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 1000,
              nrep = 2000,
              iseed = 2345,
              parallel = TRUE)
rejection_rates(tmp_out)



# lb

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "es",
                                  pop_es_name = "y~m",
                                  R = 79,
                                  by_x_1 = by_x_1,
                                  what = "lb",
                                  goal = "close_enough",
                                  tol = .005,
                                  x_interval = c(.00, .90),
                                  max_trials = 100,
                                  final_nrep = 2000,
                                  last_k = 5,
                                  delta_tol = .005,
                                  variants = list(nrep_step = 0))
rejection_rates(a_out$by_x_1)
(x_tmp <- q_dfun(a_out$dfun_out, prob = .50))
plot(a_out$fit_1)
abline(h = .80)
abline(v = x_tmp)
plot(a_out$x_history, type = "l")
points(a_out$x_history)
abline(h = q_dfun(a_out$dfun_out))
plot(a_out$dfun_out, type = "l")
q_dfun(a_out$dfun_out, .10)
q_dfun(a_out$dfun_out, .90)
(tmp_es <- setNames(x_tmp, "y~m"))

tmp_out <- power4test(
              out,
              pop_es = tmp_es,
              R = 1000,
              nrep = 2000,
              iseed = 2345,
              parallel = TRUE)
rejection_rates(tmp_out)




# Solution already in interval

})
