library(testthat)

test_that("All parameters: Multigroup: Compare groups", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <-
"
y ~ m: l
m ~ x:
  - nil
  - s
y ~ x: nil
"

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(exclude_var = TRUE,
                                        op = "~",
                                        compare_groups =  TRUE))
tmp <- rejection_rates(test_out)

expect_true(all(grepl("1==2", tmp$test_label)))

})
