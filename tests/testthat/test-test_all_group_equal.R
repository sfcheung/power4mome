library(testthat)
suppressMessages(library(lavaan))

test_that("test_group_equal", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <-
"
y ~ m: l
m ~ x:
  - nil
  - s
y ~ x: nil
"

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_group_equal,
                       test_args = list(group.equal = c("regressions"),
                                        group.partial = c("y~m")))

tmp <- rejection_rates(test_out)

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
