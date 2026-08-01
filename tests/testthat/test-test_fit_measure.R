skip("WIP")

library(testthat)
suppressMessages(library(lavaan))

test_that("test_fit_measure", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

# model_simple_med_es <-
# "
# y ~ m: l
# m ~ x:
#   - nil
#   - s
# y ~ x: nil
# "

model_simple_med_es <-
"
y ~ m: l
m ~ x: s
y ~ x: m
"

model_simple_med_0 <-
"
m ~ x
y ~ m
"

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       fit_model_args = list(
                        estimator = "MLR",
                        model_append = "y ~ 0*x"
                       ),
                       n = 100,
                       progress = !is_testing(),
                       iseed = 1234)

# Default

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure
)

tmp1 <- rejection_rates(test_out1)

test_out2 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    model_to_fit = model_simple_med_0
  )
)

tmp2 <- rejection_rates(test_out2)

expect_equal(
  tmp1$est,
  tmp2$est,
  tolerance = 1e-4
)

# RMSEA

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    fit_measure = "rmsea"
  )
)

tmp1 <- rejection_rates(test_out1)

test_out2 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    model_to_fit = model_simple_med_0,
    fit_measure = "rmsea"
  )
)

tmp2 <- rejection_rates(test_out2)

expect_equal(
  tmp1$est,
  tmp2$est,
  tolerance = 1e-4
)

# CFI

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    fit_measure = "cfi",
    sig_if = "<.95"
  )
)

tmp1 <- rejection_rates(test_out1)

test_out2 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    model_to_fit = model_simple_med_0,
    fit_measure = "cfi",
    sig_if = "<.95"
  )
)

tmp2 <- rejection_rates(test_out2)

expect_equal(
  tmp1$est,
  tmp2$est,
  tolerance = 1e-4
)

# TO PROCESS

fit0 <- test_out$sim_all[[2]]$extra$fit
mod0 <- attr(test_out$sim_all[[2]]$ptable, "model")
fit1 <- update(fit0,
          group.equal = "regressions",
          group.partial = "y~m")
lrt_out <- lavTestLRT(fit0, fit1)
test0 <- test_out$test_all[[1]][[2]]$test_results

expect_equal(test0[, "pvalue"],
             lrt_out[2, "Pr(>Chisq)"])

est <- parameterEstimates(fit1, se = FALSE, ci = FALSE)

expect_equal(est[est$lhs == "m" & est$rhs == "x" & est$group == 1, "est"],
             est[est$lhs == "m" & est$rhs == "x" & est$group == 2, "est"])
expect_false(isTRUE(all.equal(
             est[est$lhs == "y" & est$rhs == "m" & est$group == 1, "est"],
             est[est$lhs == "y" & est$rhs == "m" & est$group == 2, "est"]
             )))

})
