library(testthat)
suppressMessages(library(lavaan))

test_that("fit_model: Error and warning catching", {

# Simple Mediation Model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")
k <- c(y = 8,
       m = 8,
       x = 8)
rel <- c(y = .30,
         m = .30,
         x = .30)

data_i <- sim_data_i(model = model_simple_med,
                     pop_es = model_simple_med_es,
                     n = 50,
                     number_of_indicators = k,
                     reliability = rel,
                     seed = 1234)

expect_no_warning(fit_i <- fit_model_i(data_i))
expect_warning(lavInspect(fit_i, "post.check"))

expect_no_warning(data_all <- sim_data(nrep = 2,
                                       model = model_simple_med,
                                       pop_es = model_simple_med_es,
                                       n = 50,
                                       number_of_indicators = k,
                                       reliability = rel,
                                       iseed = 2345,
                                       parallel = FALSE,
                                       progress = FALSE))
expect_no_warning(fit_all <- fit_model(data_all,
                                       control = list(iter.max = 50)))
suppressWarnings(tmp <- sapply(fit_all,
                               lavInspect,
                               what = "post.check"))
expect_true(any(!tmp))
suppressWarnings(tmp <- sapply(fit_all,
                               lavInspect,
                               what = "converged"))
expect_true(any(!tmp))

expect_no_warning(data_all <- sim_data(nrep = 3,
                                       model = model_simple_med,
                                       pop_es = model_simple_med_es,
                                       n = 200,
                                       number_of_indicators = k,
                                       reliability = rel,
                                       iseed = 2345,
                                       parallel = FALSE,
                                       progress = FALSE))
data_all[[2]]$model_final <- data_all[[2]]$model_original
expect_no_warning(fit_all <- fit_model(data_all,
                                       control = list(iter.max = 50)))
expect_true(inherits(fit_all[[2]], "error"))
})
