library(testthat)

test_that("All parameters: lm", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters)

test_out$test_all[[1]][[2]]

(chk <- test_summary(test_out))
names(chk)

test_out <- power4test(object = test_out,
                       test_fun = test_parameters,
                       test_args = list(pars = "y~m"))

(chk <- test_summary(test_out))
names(chk)
expect_true(length(chk) == 2)

fits <- lapply(sim_only$sim_all,
               function(x) x$extra$fit)
chk_outs <- sapply(fits,
                   function(x) {
                     confint(x[[1]])[2, 1]
                   })
expect_equal(chk[[1]]$cilo[1],
             mean(chk_outs))

test_out_summary <- summarize_tests(test_out)

})
