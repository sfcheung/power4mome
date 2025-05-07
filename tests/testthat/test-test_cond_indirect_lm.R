library(testthat)

skip_if_not_installed("lmhelprs")

test_that("cond indirect effect", {

mod <-
"
m ~ x + w1 + x:w1
y ~ m + w2 + m:w2 + x
"

mod_es <- c("m ~ x" = "n",
            "y ~ x" = "m",
            "m ~ w1" = "n",
            "m ~ x:w1" = "l",
            "y ~ m:w2" = "-s")

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       R = 50,
                       ci_type = "boot",
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234)

test_ind <- power4test(object = sim_only,
                       test_fun = test_cond_indirect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        wvalues = c(w2 = 1, w1 = 0),
                                        boot_ci = TRUE))

(chk <- test_summary(test_ind))
expect_true(length(chk) == 1)

test_ind2 <- power4test(object = test_ind,
                        test_fun = test_cond_indirect,
                        test_args = list(x = "x",
                                         y = "m",
                                         wvalues = c(w1 = 0),
                                         boot_ci = TRUE))

(chk <- test_summary(test_ind2))
expect_true(length(chk) == 2)

test_ind3 <- power4test(object = test_ind2,
                        test_fun = test_cond_indirect,
                        test_args = list(x = "m",
                                         y = "y",
                                         wvalues = c(w2 = 0),
                                         boot_ci = TRUE))

(chk <- test_summary(test_ind3))
expect_true(length(chk) == 3)

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
