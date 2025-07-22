library(testthat)
suppressMessages(library(lavaan))

test_that("power4test: Print Indirect", {

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


# Generate the data

power_all_sim_only <- power4test(nrep = 10,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 n = 100,
                                 number_of_indicators = k,
                                 reliability = rel,
                                 fit_model_args = list(estimator = "ML"),
                                 do_the_test = FALSE,
                                 iseed = 1234,
                                 parallel = FALSE,
                                 progress = FALSE)

expect_no_error(print(power_all_sim_only))

tmp <- pop_indirect(power_all_sim_only$sim_all)
expect_equal(coef(tmp[[1]]),
             .50 * .30,
             ignore_attr = TRUE)

model_simple_med <-
"
m ~ x
y ~ m + x
"

power_all_sim_only <- power4test(nrep = 10,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 n = 100,
                                 fit_model_args = list(fit_function = "lm"),
                                 do_the_test = FALSE,
                                 iseed = 1234,
                                 parallel = FALSE,
                                 progress = FALSE)

expect_no_error(print(power_all_sim_only))

tmp <- pop_indirect(power_all_sim_only$sim_all)
expect_equal(coef(tmp[[1]]),
             .50 * .30,
             ignore_attr = TRUE)

# Case 2

model_simple_med <-
"
m1 ~ x
m2 ~ m1
y ~ m2 + m1 + x
"

model_simple_med_es <-
"
.ind.(x->m1->m2->y): si
y ~ m1: s
y ~ x: m
"

k <- c(y = 3,
       m1 = 3,
       m2 = 4,
       x = 3)
rel <- c(y = .80,
         m1 = .70,
         m2 = .80,
         x = .70)

# Generate the data

power_all_sim_only <- power4test(nrep = 10,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 n = 500,
                                 number_of_indicators = k,
                                 reliability = rel,
                                 fit_model_args = list(estimator = "ML"),
                                 do_the_test = FALSE,
                                 iseed = 1234,
                                 parallel = FALSE,
                                 progress = FALSE)

expect_output(print(power_all_sim_only),
              "x -> m1 -> m2")
tmp <- pop_indirect(power_all_sim_only$sim_all,
                    pure_x = TRUE,
                    pure_y = TRUE)
expect_equal(coef(tmp[[1]])[1],
             .141,
             ignore_attr = TRUE)


power_all_sim_only <- power4test(nrep = 10,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 n = 500,
                                 fit_model_args = list(fit_function = "lm"),
                                 do_the_test = FALSE,
                                 iseed = 1234,
                                 parallel = FALSE,
                                 progress = FALSE)

expect_output(print(power_all_sim_only),
              "x -> m1 -> m2")
tmp <- pop_indirect(power_all_sim_only$sim_all,
                    pure_x = TRUE,
                    pure_y = TRUE)
expect_equal(coef(tmp[[1]])[1],
             .141,
             ignore_attr = TRUE)

})

