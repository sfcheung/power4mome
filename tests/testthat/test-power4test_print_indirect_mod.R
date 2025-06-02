library(testthat)
suppressMessages(library(lavaan))

test_that("power4test: Print Indirect", {

mod <-
"
m ~ x + w + x:w
y ~ m + x
"

mod_es <-
"
.ind.(x->m->y): mi
m ~ x:w: l
y ~ x: m
"

# Generate the data

power_all_sim_only <- power4test(nrep = 10,
                                 model = mod,
                                 pop_es = mod_es,
                                 n = 100,
                                 fit_model_args = list(estimator = "ML"),
                                 do_the_test = FALSE,
                                 iseed = 1234,
                                 parallel = FALSE,
                                 progress = FALSE)

expect_output(print(power_all_sim_only),
              "w -> m -> y")
suppressWarnings(tmp <- pop_indirect(power_all_sim_only$sim_all))
expect_equal(coef(tmp)["x -> m -> y"],
             .316,
             ignore_attr = TRUE)

# Case 2

mod <-
"
y ~ x + w + x:w + c1
"

mod_es <-
"
y ~ x:w: s
y ~ x: m
"

# Generate the data

power_all_sim_only <- power4test(nrep = 10,
                                 model = mod,
                                 pop_es = mod_es,
                                 n = 500,
                                 fit_model_args = list(estimator = "ML"),
                                 do_the_test = FALSE,
                                 iseed = 1234,
                                 parallel = FALSE,
                                 progress = FALSE)

expect_false(any(grepl("Indirect",
                 capture.output(print(power_all_sim_only)))))
tmp <- pop_indirect(power_all_sim_only$sim_all)
expect_true(length(tmp) == 0)

# Multigroup

# Case 1

mod <-
"
m ~ x
y ~ m + x + w + x:w
"

mod_es<-
"
m ~ x: [m, l]
y ~ m: [s, m]
y ~ x: [-m, nil]
"

# Generate the data

power_all_sim_only <- power4test(nrep = 10,
                                 model = mod,
                                 pop_es = mod_es,
                                 n = 100,
                                 fit_model_args = list(estimator = "ML"),
                                 do_the_test = FALSE,
                                 iseed = 1234,
                                 parallel = FALSE,
                                 progress = FALSE)

print(power_all_sim_only)

expect_output(print(power_all_sim_only),
              "Group1.x -> m -> y")
tmp <- pop_indirect(power_all_sim_only$sim_all)
expect_equal(coef(tmp),
             c(.10 * .30, .30 * .50),
             ignore_attr = TRUE)

})

