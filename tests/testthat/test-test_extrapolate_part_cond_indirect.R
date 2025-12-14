skip_on_cran()

library(testthat)

test_that("Boos-Zhang: Set of Rs", {

opt_old <- options(power4mome.bz = TRUE)

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
                       R = 237,
                       do_the_test = FALSE,
                       iseed = 1234,
                       parallel = FALSE,
                       progress = FALSE)

test_ind <- power4test(object = sim_only,
                       test_fun = test_cond_indirect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        wvalues = c(w2 = 1, w1 = 0),
                                        mc_ci = TRUE,
                                        test_method = "pvalue"),
                       parallel = FALSE,
                       progress = FALSE)

(rr <- rejection_rates(test_ind))
(chk <- test_summary(test_ind))
expect_output(print(rr),
              "Boos and Zhang")
expect_true(any(grepl("bz_", names(chk[[1]]))))

# Alpha/level not supported

test_ind <- power4test(object = sim_only,
                       test_fun = test_cond_indirect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        wvalues = c(w2 = 1, w1 = 0),
                                        mc_ci = TRUE,
                                        level = .90,
                                        test_method = "pvalue"),
                       parallel = FALSE,
                       progress = FALSE)

(chk <- test_summary(test_ind))
expect_false(any(grepl("bz_", names(chk[[1]]))))

options(opt_old)
})
