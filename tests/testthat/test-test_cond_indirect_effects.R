library(testthat)

test_that("cond indirect effects", {

mod <-
"
m ~ x + w1 + x:w1
y ~ m + w2 + m:w2 + x
"

mod_es <- c("m ~ x" = "s",
            "y ~ x" = "m",
            "m ~ w1" = "l",
            "m ~ x:w1" = "l",
            "y ~ m:w2" = "-s")

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       R = 79,
                       do_the_test = FALSE,
                       iseed = 12345)

test_ind <- power4test(object = sim_only,
                       test_fun = test_cond_indirect_effects,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        wlevels = c("w2", "w1"),
                                        mc_ci = TRUE))

(chk <- test_summary(test_ind))
expect_true(length(chk) == 1)

test_indb <- power4test(object = sim_only,
                       test_fun = test_cond_indirect_effects,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        wlevels = c("w2", "w1"),
                                        mc_ci = TRUE,
                                        test_method = "pvalue"))

(chkb <- test_summary(test_indb))
expect_equal(chk[[1]]$sig,
             chkb[[1]]$sig)

test_ind2 <- power4test(object = test_ind,
                        test_fun = test_cond_indirect_effects,
                        test_args = list(x = "x",
                                         y = "m",
                                         wlevels = c("w1"),
                                         mc_ci = TRUE))

(chk <- test_summary(test_ind2))
expect_true(length(chk) == 2)

test_ind2b <- power4test(object = test_indb,
                        test_fun = test_cond_indirect_effects,
                        test_args = list(x = "x",
                                         y = "m",
                                         wlevels = c("w1"),
                                         mc_ci = TRUE,
                                         test_method = "pvalue"))

(chkb <- test_summary(test_ind2b))
expect_equal(chk[[2]]$sig,
             chkb[[2]]$sig)


test_ind3 <- power4test(object = test_ind2,
                        test_fun = test_cond_indirect_effects,
                        test_args = list(x = "m",
                                         y = "y",
                                         wlevels = "w2",
                                         mc_ci = TRUE))

(chk <- test_summary(test_ind3))
expect_true(length(chk) == 3)

test_ind3b <- power4test(object = test_ind2b,
                        test_fun = test_cond_indirect_effects,
                        test_args = list(x = "m",
                                         y = "y",
                                         wlevels = "w2",
                                         mc_ci = TRUE,
                                         test_method = "pvalue"))

(chkb <- test_summary(test_ind3b))
expect_equal(chk[[3]]$sig,
             chkb[[3]]$sig)


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
