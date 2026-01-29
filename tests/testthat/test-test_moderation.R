library(testthat)

test_that("Test moderation", {

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
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_moderation)

(chk <- test_summary(test_out))

test_out <- power4test(object = test_out,
                       test_fun = test_parameters,
                       test_args = list(pars = c("m~x:w1",
                                                 "y~m:w2")))
(chk <- test_summary(test_out))

expect_equal(chk[[1]]$sig,
             chk[[2]]$sig)

# Standardized

test_out <- power4test(object = sim_only,
                       test_fun = test_moderation,
                       test_args = list(standardized = TRUE))

(chk <- test_summary(test_out))

test_out <- power4test(object = test_out,
                       test_fun = test_parameters,
                       test_args = list(pars = c("m~x:w1",
                                                 "y~m:w2"),
                                        standardized = TRUE))
(chk <- test_summary(test_out))

expect_equal(chk[[1]]$sig,
             chk[[2]]$sig)

})
