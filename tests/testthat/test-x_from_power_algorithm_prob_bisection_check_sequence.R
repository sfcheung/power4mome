skip("Internal")

# Examine the effect of the sequence

library(testthat)

test_that("probabilistic bisection: Sequence", {

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
                  parallel = TRUE,
                  iseed = 1234)

by_x_1 <- power4test_by_n(out,
                          n = 90)

# Close enough

set.seed(1234)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  final_nrep = 2000,
                                  max_trials = 10,
                                  variants = list(total_nrep = 10 * 50))
x_history <- a_out$x_history
f_history <- a_out$f_history
dfun_history <- a_out$dfun_history

i <- seq_along(x_history)
i <- sample.int(length(x_history))
x_tmp <- x_history[i]
f_tmp <- f_history[i]
p <- .60
dfun_i <- gen_dfun(
              interval = c(50, 2000),
              npoints = (2000 - 50 + 1)
            )
for (i in seq_along(f_tmp)) {
  if (sign(f_tmp[i]) == -1) {
    z_i <- 1
  } else {
    z_i <- -1
  }
  dfun_i <- update_dfun(
              dfun = dfun_i,
              x_i = x_tmp[i],
              p = p,
              z_i = z_i
            )
  plot(dfun_i, type = "l", col = "blue")
  plot(dfun_history[[i]], type = "l", col = "red")
  all.equal(dfun_i, dfun_history[[i]])
}
all.equal(dfun_i,
          dfun_history[[i]])
ylim <- range(c(dfun_i[, "prob"],
                dfun_history[[i]][, "prob"]))
plot(dfun_i,
     type = "l",
     col = "blue",
     ylim = ylim)
points(dfun_history[[i]],
       type = "l",
       col = "red")
plot(dfun_i[, "prob"],
     dfun_history[[i]][, "prob"])

dfun_history_last <- dfun_history[[length(dfun_history)]]
all.equal(dfun_out,
          dfun_history_last)
ylim <- range(c(dfun_out[, "prob"],
                dfun_history_last[, "prob"]))
plot(dfun_out,
     type = "l",
     col = "blue",
     ylim = ylim)
points(dfun_history_last,
       type = "l",
       col = "red")


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

})
