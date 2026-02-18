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

set.seed(12345)
a_out <- power_algorithm_prob_bisection(
                                  object = out,
                                  x = "n",
                                  by_x_1 = by_x_1,
                                  final_nrep = 2000,
                                  max_trials = 100,
                                  variants = list(total_nrep = 100 * 50))
x_history <- a_out$x_history
f_history <- a_out$f_history
dfun_history <- a_out$dfun_history[seq_along(f_history)]

i <- seq_along(x_history)
i <- sample.int(length(x_history))
x_tmp <- x_history[i]
f_tmp <- f_history[i]
p <- .60
all_equal_history <- vector("logical", length(i))
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
  all_equal_history[i] <- isTRUE(all.equal(
                              dfun_i,
                              dfun_history[[i]])
                            )
}
all_equal_history
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

# Confirmed that the final posterior distribution
# does not depend on the order of the iterations.

dfun_out <- a_out$dfun_out

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


})
