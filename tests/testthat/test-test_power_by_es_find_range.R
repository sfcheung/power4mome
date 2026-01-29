skip_on_cran()

library(testthat)

test_that("Check valid es values", {

verbose <- FALSE

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 2,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       R = 50,
                       ci_type = "boot",
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234,
                       progress = !is_testing(),
                       parallel = FALSE)

test_out <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE),
                       progress = FALSE,
                       parallel = FALSE)

chk1 <- check_valid_es_values(test_out,
                              pop_es_name = "y ~ x",
                              verbose = verbose)
chk2 <- check_valid_es_values(test_out$sim_all[[1]]$ptable,
                              pop_es_name = "y ~ x",
                              verbose = verbose)
expect_equal(chk1,
             c(-.95, .7))
expect_equal(chk1,
             chk2)

chk1 <- check_valid_es_values(test_out,
                              pop_es_name = "y ~ m",
                              verbose = verbose)
chk2 <- check_valid_es_values(test_out$sim_all[[1]]$ptable,
                              pop_es_name = "y ~ m",
                              verbose = verbose)
expect_equal(chk1,
             c(-.95, .95))
expect_equal(chk1,
             chk2)

chk1 <- check_valid_es_values(test_out,
                              pop_es_name = "m ~ x",
                              verbose = verbose)
chk2 <- check_valid_es_values(test_out$sim_all[[1]]$ptable,
                              pop_es_name = "m ~ x",
                              verbose = verbose)
expect_equal(chk1,
             c(-.95, .95))
expect_equal(chk2,
             chk1)

# Case 2

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "-m",
                         "y ~ x" = "-s")

sim_only <- power4test(nrep = 2,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       do_the_test = FALSE,
                       iseed = 1234,
                       progress = !is_testing(),
                       parallel = FALSE)

chk1 <- check_valid_es_values(sim_only,
                              pop_es_name = "y ~ x",
                              verbose = verbose)
chk2 <- check_valid_es_values(sim_only$sim_all[[1]]$ptable,
                              pop_es_name = "y ~ x",
                              verbose = verbose)
expect_equal(chk1,
             c(-.7, .95))
expect_equal(chk1,
             chk2)

chk1 <- check_valid_es_values(sim_only,
                              pop_es_name = "y ~ m",
                              verbose = verbose)
chk2 <- check_valid_es_values(sim_only$sim_all[[1]]$ptable,
                              pop_es_name = "y ~ m",
                              verbose = verbose)
expect_equal(chk1,
             c(-.95, .95))
expect_equal(chk2,
             chk1)

chk1 <- check_valid_es_values(sim_only,
                              pop_es_name = "m ~ x",
                              verbose = verbose)
chk2 <- check_valid_es_values(sim_only$sim_all[[1]]$ptable,
                              pop_es_name = "m ~ x",
                              verbose = verbose)
expect_equal(chk1,
             c(-.95, .95))
expect_equal(chk2,
             chk1)

# Check error

expect_error(check_valid_es_values(sim_only,
                                   pop_es_name = "m ~ x",
                                   es_min = .5,
                                   es_max = -.5,
                                   verbose = verbose))
expect_error(check_valid_es_values(sim_only,
                                   pop_es_name = "m ~ x",
                                   es_min = .5,
                                   es_max = .5,
                                   verbose = verbose))
expect_equal(check_valid_es_values(sim_only,
                                   pop_es_name = "m ~ x",
                                   es_min = 2,
                                   es_max = 3,
                                   verbose = verbose),
             c(NA, NA))

})
