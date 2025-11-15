skip_on_cran()

library(testthat)

test_that("Boos-Zhang: Set of Rs", {

model_simple_med <-
"
m ~ a*x
y ~ b*m + x
ab := a * b
"

model_simple_med_es <- c("y ~ m" = "s",
                         "m ~ x" = "s",
                         "y ~ x" = "n")
k <- c(y = 3,
       m = 3,
       x = 3)
rel <- c(y = .70,
         m = .70,
         x = .70)

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       number_of_indicators = k,
                       reliability = rel,
                       fit_model_args = list(estimator = "ML"),
                       R = 237,
                       do_the_test = FALSE,
                       iseed = 1234,
                       parallel = FALSE,
                       progress = FALSE)

test_ind <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        mc_ci = TRUE,
                                        test_method = "pvalue"),
                       parallel = FALSE,
                       progress = FALSE)

expect_output(print(rejection_rates(test_ind)),
              "Boos and Zhang")
(chk <- test_summary(test_ind))
expect_true(any(grepl("bz_", names(chk[[1]]))))

# Alpha/level not supported

test_ind <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        mc_ci = TRUE,
                                        level = .90,
                                        test_method = "pvalue"),
                       parallel = FALSE,
                       progress = FALSE)
(chk <- test_summary(test_ind))
expect_false(any(grepl("bz_", names(chk[[1]]))))

})
