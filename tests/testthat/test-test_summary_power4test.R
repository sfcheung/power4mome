library(testthat)

test_that("Model summary", {

model_simple_med <-
"
m ~ a*x
y ~ b*m + cp*x
ab := a * b
total := a * b + cp
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "-s")
k <- c(y = 3,
       m = 4,
       x = 5)
rel <- c(y = .70,
         m = .80,
         x = .90)

sim_only <- power4test(nrep = 10,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 200,
                       R = 10,
                       number_of_indicators = k,
                       reliability = rel,
                       fit_model_args = list(estimator = "ML"),
                       do_the_test = FALSE,
                       iseed = 1234)

sim_all <- sim_only$sim_all

expect_no_error(print(sim_all))

# Multigroup

model_simple_med2 <-
"
m ~ c(a1, a2)*x
y ~ c(b1, b2)*m + x
ab1 := a1 * b1
ab2 := a2 * b2
"

model_simple_med_es2 <- list("y ~ m" = "l",
                             "m ~ x" = "m",
                             "y ~ x" = c("-s", "m"))

rel2 <- list(y = .70,
             m = c(.80, .70),
             x = .90)

sim_gp02 <- power4test(nrep = 2,
                       model = model_simple_med2,
                       pop_es = model_simple_med_es2,
                       n = c(100, 200),
                       R = 5,
                       ci_type = "boot",
                       number_of_indicators = k,
                       reliability = rel2,
                       fit_model_args = list(estimator = "ML"),
                       do_the_test = FALSE,
                       iseed = 1234)

expect_no_error(print(sim_gp02$sim_all))

})
