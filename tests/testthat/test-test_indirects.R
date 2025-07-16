library(testthat)

test_that("indirect effects", {

mod <-
"
m1 ~ a1*x
m2 ~ a2*x
y ~ b1*m1 + b2*m2 + x
a1b1 := a1 * b1
a2b2 := a2 * b2
"

mod_es <- c("y ~ m1" = "l",
            "m1 ~ x" = "m",
            "y ~ m2" = "s",
            "m2 ~ x" = "l",
            "y ~ x" = "n")
k <- c(y = 3,
       m1 = 3,
       m2 = 3,
       x = 3)
rel <- c(y = .70,
         m1 = .70,
         m2 = .70,
         x = .70)

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       number_of_indicators = k,
                       reliability = rel,
                       fit_model_args = list(estimator = "ML"),
                       R = 100,
                       do_the_test = FALSE,
                       iseed = 1234)

test_ind <- power4test(object = sim_only,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        y = "y",
                                        mc_ci = TRUE))

(chk <- test_summary(test_ind))
expect_true(length(chk) == 1)

test_ind <- power4test(object = sim_only,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        y = "y",
                                        omnibus = "all_sig",
                                        mc_ci = TRUE))
test_ind$test_all[[1]][[1]][[1]]
(chk <- test_summary(test_ind))
expect_true(length(chk) == 1)

test_ind <- power4test(object = sim_only,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        y = "y",
                                        omnibus = "at_least_one_sig",
                                        mc_ci = TRUE))
test_ind$test_all[[1]][[1]][[1]]
(chk <- test_summary(test_ind))
expect_true(length(chk) == 1)


test_ind2 <- power4test(object = test_ind,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        m = "m2",
                                        y = "y",
                                        mc_ci = TRUE))

(chk <- test_summary(test_ind2))
expect_true(length(chk) == 1)

expect_error(power4test(object = test_ind2,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "m",
                                        y = "y",
                                        mc_ci = TRUE),
                       progress = FALSE,
                       parallel = FALSE))

})

test_that("indirect effects, lm", {

mod <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
"

mod_es <- c("y ~ m1" = "s",
            "m1 ~ x" = "m",
            "y ~ m2" = "s",
            "m2 ~ x" = "s",
            "y ~ x" = "n")

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       fit_model_args = list(fit_function = "lm"),
                       R = 40,
                       ci_type = "boot",
                       do_the_test = FALSE,
                       iseed = 1234)

test_ind <- power4test(object = sim_only,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        y = "y",
                                        boot_ci = TRUE))

(chk <- test_summary(test_ind))
expect_true(length(chk) == 1)

test_ind2 <- power4test(object = test_ind,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "x",
                                        m = "m2",
                                        y = "y",
                                        boot_ci = TRUE))

(chk <- test_summary(test_ind2))
expect_true(length(chk) == 1)

expect_error(power4test(object = test_ind2,
                       test_fun = test_k_indirect_effects,
                       test_args = list(x = "m",
                                        y = "y",
                                        boot_ci = TRUE),
                       progress = FALSE,
                       parallel = FALSE))

})
