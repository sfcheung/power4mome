library(testthat)
suppressMessages(library(lavaan))

# All-In-One

test_that("power4test: Test es", {

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
rel <- c(y = .80,
         m = .80,
         x = .80)

# Generate the data

power_all_sim_only <- power4test(nrep = 2,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 es1 = c("n" = .30,
                                         "nil" = .30,
                                         "s" = .15,
                                         "m" = .20,
                                         "l" = .25),
                                 n = 10000,
                                 number_of_indicators = k,
                                 reliability = rel,
                                 fit_model_args = list(estimator = "ML"),
                                 do_the_test = FALSE,
                                 progress = !is_testing(),
                                 parallel = FALSE,
                                 iseed = 1234)
tmp <- fit_model_i(power_all_sim_only$sim_all[[1]])
std <- standardizedSolution(tmp, se = FALSE)
expect_equal(std$est.std[c(2, 1, 3)],
             c(.25, .20, .30),
             tolerance = 1e-1)

})

