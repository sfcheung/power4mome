skip_on_cran()

library(testthat)

test_that("Boos-Zhang", {

mod <-
"
m ~ x + w + x:w
y ~ m
"

mod_es <- c("m ~ x" = "n",
            "y ~ x" = "m",
            "m ~ w" = "n",
            "m ~ x:w" = "l")

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       R = 119,
                       do_the_test = FALSE,
                       iseed = 1234,
                       parallel = FALSE,
                       progress = FALSE)

test_out1 <- power4test(object = sim_only,
                         test_fun = test_index_of_mome,
                         test_args = list(x = "x",
                                          m = "m",
                                          y = "y",
                                          w = "w",
                                          mc_ci = TRUE,
                                          test_method = "pvalue"),
                         parallel = FALSE,
                         progress = FALSE)

(rr <- rejection_rates(test_out1))
(chk <- test_summary(test_out1))
expect_output(print(rr),
              "Boos and Zhang")
expect_true("nlt0" %in% names(chk[[1]]))

# Alpha/level not supported

test_out1 <- power4test(object = sim_only,
                         test_fun = test_index_of_mome,
                         test_args = list(x = "x",
                                          m = "m",
                                          y = "y",
                                          w = "w",
                                          mc_ci = TRUE,
                                          level = .90,
                                          test_method = "pvalue"),
                         parallel = FALSE,
                         progress = FALSE)

(rr <- rejection_rates(test_out1))
expect_true(is.null(attr(rr, "extra")$bz_model))

})
