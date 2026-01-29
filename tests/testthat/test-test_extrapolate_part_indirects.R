skip_on_cran()

library(testthat)

test_that("Boos-Zhang: Set of Rs", {

opt_old <- options(power4mome.bz = TRUE)

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
                       n = 200,
                       number_of_indicators = k,
                       reliability = rel,
                       fit_model_args = list(estimator = "ML"),
                       R = 237,
                       do_the_test = FALSE,
                       iseed = 1234,
                       parallel = FALSE,
                       progress = !is_testing())

test_ind <- power4test(object = sim_only,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        y = "y",
                                        mc_ci = TRUE,
                                        test_method = "pvalue"),
                       parallel = FALSE,
                       progress = !is_testing())
(rr <- rejection_rates(test_ind))
expect_output(print(rr),
              "Boos and Zhang")
(chk <- test_summary(test_ind))
expect_true(any(grepl("bz_", colnames(chk[[1]]))))

# Alpha/level not supported

test_ind <- power4test(object = sim_only,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        y = "y",
                                        mc_ci = TRUE,
                                        level = .90,
                                        test_method = "pvalue"),
                       parallel = FALSE,
                       progress = !is_testing())
(chk <- test_summary(test_ind))
expect_false(any(grepl("bz_", colnames(chk[[1]]))))

options(opt_old)
})
