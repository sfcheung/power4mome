# A long test

library(testthat)
suppressMessages(library(lavaan))

test_that("power4test: Print Indirect", {

# Multigroup

# Case 1

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <-
"
m ~ x: [m, l]
y ~ m: [s, m]
y ~ x: [-m, nil]
"

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

expect_output(print(power_all_sim_only),
              "Group1.x -> m -> y")

tmp <- pop_indirect(power_all_sim_only$sim_all)
expect_equal(coef(tmp),
             c(.10 * .30, .50 * .30),
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
.ind.(x->m1->m2->y): [mi, li]
y ~ m1: s
y ~ x: [s, nil]
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

power_all_sim_only <- power4test(nrep = 2,
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
              "Group1.x -> m1 -> y")

tmp <- pop_indirect(power_all_sim_only$sim_all)
expect_equal(coef(tmp)["Group2.x -> m1 -> m2 -> y"],
             .51,
             ignore_attr = TRUE)
expect_equal(coef(tmp)["Group1.x -> m1 -> m2 -> y"],
             .361,
             ignore_attr = TRUE)

})

