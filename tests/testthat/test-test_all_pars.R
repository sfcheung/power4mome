library(testthat)

test_that("All parameters", {

model_simple_med <-
"
m ~ a*x
y ~ b*m + x
ab := a * b
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
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
                       do_the_test = FALSE,
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters)

test_out$test_all[[1]][[2]]

(chk <- test_summary(test_out))
names(chk)

test_out <- power4test(object = test_out,
                       test_fun = test_parameters,
                       test_args = list(standardized = TRUE))

(chk <- test_summary(test_out))
names(chk)
expect_true(length(chk) == 2)

test_out <- power4test(object = test_out,
                       test_fun = test_parameters,
                       test_args = list(op = "~",
                                        pars = "a"))

(chk <- test_summary(test_out))
names(chk)
expect_true(length(chk) == 3)

test_out <- power4test(object = test_out,
                       test_fun = test_parameters,
                       test_args = list(op = ":="))

(chk <- test_summary(test_out))
names(chk)
expect_true(length(chk) == 4)

fits <- lapply(sim_only$sim_all,
               function(x) x$extra$fit)
chk_outs <- sapply(fits,
                   function(x) {
                     est <- lavaan::parameterEstimates(x,
                                                       remove.nonfree = TRUE)
                     (est$ci.lower > 0) | (est$ci.upper < 0)
                   })
chk_sigs <- rowMeans(chk_outs,
                     na.rm = TRUE)
expect_identical(chk[[1]]$sig,
                 chk_sigs)

chk_outs <- sapply(fits,
                   function(x) {
                     lavaan::standardizedSolution(x)$est.std
                   })
chk_est <- rowMeans(chk_outs,
                    na.rm = TRUE)
expect_identical(chk[[2]]$est,
                 chk_est)

expect_true(all(chk[[3]]$op %in% "~"))

expect_identical(chk[[4]]$op,
                 ":=")

test_out_summary <- summarize_tests(test_out)

})
