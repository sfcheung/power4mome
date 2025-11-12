skip("WIP")

library(testthat)

test_that("indirect effect", {

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
                       iseed = 1234)

test_ind <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        mc_ci = TRUE))

rejection_rates(test_ind)
(chk <- test_summary(test_ind))
expect_true(length(chk) == 1)

test_indb <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        mc_ci = TRUE,
                                        test_method = "pvalue"))
rejection_rates(test_indb)
(chkb <- test_summary(test_indb))


expect_equal(test_summary(test_ind)[[1]]["sig"],
             test_summary(test_indb)[[1]]["sig"])

test_ind2 <- power4test(object = test_ind,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        y = "m",
                                        mc_ci = TRUE))

(chk <- test_summary(test_ind2))
expect_true(length(chk) == 2)

test_ind2b <- power4test(object = test_ind,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        y = "m",
                                        mc_ci = TRUE,
                                        test_method = "pvalue"))
expect_equal(test_summary(test_ind2)[[1]]["sig"],
             test_summary(test_ind2b)[[1]]["sig"])


test_ind3 <- power4test(object = test_ind2,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "m",
                                        y = "y",
                                        mc_ci = TRUE))

(chk <- test_summary(test_ind3))
expect_true(length(chk) == 3)

test_ind3b <- power4test(object = test_ind2,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "m",
                                        y = "y",
                                        mc_ci = TRUE,
                                        test_method = "pvalue"))
expect_equal(test_summary(test_ind3)[[1]]["sig"],
             test_summary(test_ind3b)[[1]]["sig"])

# test_ind4 <- suppressWarnings(power4test(object = test_ind3,
#                                          test_fun = test_indirect_effect,
#                                          test_args = list(x = "x",
#                                                            m = "m",
#                                                            y = "y",
#                                                            mc_ci = TRUE,
#                                                            standardized_x = TRUE,
#                                                            standardized_y = TRUE)))

# (chk <- test_summary(test_ind4))
# expect_true(length(chk) == 4)

# test_ind4_summary <- summarize_tests(test_ind4$test_all)

})
