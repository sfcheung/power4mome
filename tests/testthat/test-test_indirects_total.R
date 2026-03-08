library(testthat)

test_that("indirect effects", {

mod <-
"
m1 ~ a1*x
m2 ~ a2*x
y ~ b1*m1 + b2*m2 + x
a1b1 := a1 * b1
a2b2 := a2 * b2
"

mod_es <- c("y ~ m1" = "l",
            "m1 ~ x" = "m",
            "y ~ m2" = "s",
            "m2 ~ x" = "l",
            "y ~ x" = "n")
k <- c(y = 3,
       m1 = 3,
       m2 = 3,
       x = 3)
rel <- c(y = .70,
         m1 = .70,
         m2 = .70,
         x = .70)

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       number_of_indicators = k,
                       reliability = rel,
                       fit_model_args = list(estimator = "ML"),
                       R = 100,
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

test_tot <- power4test(object = sim_only,
                       progress = !is_testing(),
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        y = "y",
                                        mc_ci = TRUE,
                                        omnibus = "total"))

(chk <- test_summary(test_tot))
expect_true(length(chk) == 1)

test_ind <- power4test(object = sim_only,
                       progress = !is_testing(),
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        y = "y",
                                        mc_ci = TRUE))

chk1 <- test_tot$test_all[[1]][[1]]$test_results$est
chk2 <- test_ind$test_all[[1]][[1]]$test_results$est

expect_equal(chk1,
             sum(chk2))

})
