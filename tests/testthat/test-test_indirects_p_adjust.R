library(testthat)

test_that("indirect effects: p.adjust", {

mod <-
"
m1 ~ a1*x
m2 ~ a2*x
m3 ~ a3*x
m4 ~ a4*x
y ~ b1*m1 + b2*m2 + b3*m3 + b4*m4 + x
a1b1 := a1 * b1
a2b2 := a2 * b2
"

mod_es <- c("y ~ m1" = "l",
            "m1 ~ x" = "m",
            "y ~ m2" = "s",
            "m2 ~ x" = "l",
            "y ~ m3" = "n",
            "m3 ~ x" = "n",
            "y ~ m4" = "n",
            "m4 ~ x" = "n",
            "y ~ x" = "n")

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       fit_model_args = list(estimator = "ML"),
                       R = 100,
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

test_ind <- power4test(object = sim_only,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        y = "y",
                                        mc_ci = TRUE,
                                        p_adjust_method = "BH"),
                       iseed = 2345)
(chk <- test_summary(test_ind))

p_org <- test_ind$test_all[[1]][[1]]$test_results$pvalue_org
p_adj <- test_ind$test_all[[1]][[1]]$test_results$pvalue

expect_equal(p.adjust(p_org, method = "BH"),
             p_adj)

})
