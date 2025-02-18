library(testthat)
suppressMessages(library(lavaan))

test_that("power4test: With update", {

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

test_par <- function(object,
                     par,
                     alpha = .05) {
  est <- lavaan::parameterEstimates(object)
  if (par %in% est$label) {
    i <- (est$lhs == par) &
         (est$label == par)
    i <- which(i)
  } else {
    par1 <- lavaan::lavParseModelString(par,
                                        as.data.frame. = TRUE)
    i <- (est$lhs == par1$lhs) &
        (est$op == par1$op) &
        (est$rhs == par1$rhs)
    i <- which(i)
  }
  out <- c(est = est[i, "est"],
           cilo = est[i, "ci.lower"],
           cihi = est[i, "ci.upper"],
           sig = as.numeric(est[i, "pvalue"] < alpha))
  out
}

par_results <- function(object) {
  object
}

# Generate the data

power_all_sim_only <- power4test(nrep = 3,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 n = 50,
                                 number_of_indicators = k,
                                 reliability = rel,
                                 fit_model_args = list(estimator = "ML"),
                                 do_the_test = FALSE,
                                 iseed = 1234)

# Update the model and regenerate the data

power_all_sim_only_new_es <- power4test(object = power_all_sim_only,
                                        pop_es = c("y ~ m" = "s"))

expect_equal(power_all_sim_only_new_es$sim_all[[3]]$ptable[2, "start"],
             .10)
expect_equal(power_all_sim_only$sim_all[[3]]$ptable[2, "start"],
             .50)

# Update the model and regenerate the data

power_all_sim_only_new_n <- power4test(object = power_all_sim_only,
                                       n = 55)
expect_equal(nrow(power_all_sim_only_new_n$sim_all[[3]]$mm_lm_dat_out),
             55)
expect_equal(nrow(power_all_sim_only$sim_all[[3]]$mm_lm_dat_out),
             50)

# Update the model and n and regenerate the data

power_all_sim_only_new_n_pop_es <- power4test(object = power_all_sim_only,
                                              n = 60,
                                              pop_es = c("y ~ m" = "n"))
expect_equal(power_all_sim_only_new_n_pop_es$sim_all[[3]]$ptable[2, "start"],
             .00)
expect_equal(nrow(power_all_sim_only_new_n_pop_es$sim_all[[3]]$mm_lm_dat_out),
             60)

# Indirect effect

ind_results <- function(out) {
  ci0 <- stats::confint(out)
  out1 <- ifelse((ci0[1, 1] > 0) || (ci0[1, 2] < 0),
                  yes = 1,
                  no = 0)
  out2 <- c(est = unname(coef(out)),
            cilo = ci0[1, 1],
            cihi = ci0[1, 2],
            sig = out1)
  return(out2)
}

# Do the test
# - Need only the arguments for the test.

power_all_test_only <- power4test(object = power_all_sim_only,
                                  test_fun = test_par,
                                  test_args = list(par = "y~m"),
                                  map_names = c(object = "fit"))

# power_all_test_only <- power4test(object = power_all_sim_only,
#                                   test_fun = manymome::indirect_effect,
#                                   test_args = list(x = "x",
#                                                    m = "m",
#                                                    y = "y",
#                                                    mc_ci = TRUE),
#                                   map_names = c(fit = "fit",
#                                                 mc_out = "mc_out"),
#                                   results_fun = ind_results)

expect_true(identical(power_all_test_only$sim_all,
                      power_all_sim_only$sim_all))

(chk1 <- test_summary(power_all_test_only))

chk2 <- mean(sapply(power_all_test_only$sim_all,
                    function(x) {coef(x$extra$fit)["b"]}))
expect_equal(chk1[[1]]["est"],
             chk2,
             ignore_attr = TRUE)


# Update the model, regenerate the data, and redo the test

power_all_test_only_new_es <- power4test(object = power_all_test_only,
                                         pop_es = c("y ~ m" = "s"))
expect_false(identical(power_all_test_only$sim_all,
                      power_all_test_only_new_es$sim_all))
expect_equal(power_all_test_only$sim_all[[3]]$ptable[2, "start"],
             .50)
expect_equal(power_all_test_only_new_es$sim_all[[3]]$ptable[2, "start"],
             .10)

expect_false(identical(test_summary(power_all_test_only_new_es),
                       test_summary(power_all_test_only)))

(chk1 <- test_summary(power_all_test_only_new_es))

chk2 <- mean(sapply(power_all_test_only_new_es$sim_all,
                    function(x) {coef(x$extra$fit)["b"]}))
expect_equal(chk1[[1]]["est"],
             chk2,
             ignore_attr = TRUE)

# Update the sample size, regenerate the data, and redo the test

power_all_test_only_new_n <- power4test(object = power_all_test_only,
                                        n = 200)
expect_false(identical(test_summary(power_all_test_only_new_n),
                       test_summary(power_all_test_only)))

# Update the sample size, regenerate the data, and redo the test

power_all_test_only_new_n_pop_es <- power4test(object = power_all_test_only,
                                               pop_es = c("y ~ m" = "s"),
                                               n = 200)
expect_false(identical(test_summary(power_all_test_only_new_n_pop_es),
                       test_summary(power_all_test_only)))

# Add a test

power_all_test_add_an_effect <- power4test(object = power_all_test_only,
                                           test_fun = test_par,
                                           test_args = list(par = "y~x"),
                                           map_names = c(object = "fit"),
                                           test_name = "y~x")

expect_true(length(test_summary(power_all_test_add_an_effect)) == 2)

# Update the sample size, regenerate the data, and redo two tests

power_all_test_add_an_effect_new_n <- power4test(object = power_all_test_add_an_effect,
                                                 pop_es = c("y ~ m" = "s"),
                                                 n = 100)

chk1 <- test_summary(power_all_test_add_an_effect)
chk2 <- test_summary(power_all_test_add_an_effect_new_n)

expect_false(identical(chk1,
                       chk2))

(chk1 <- test_summary(power_all_test_add_an_effect_new_n))

chk2 <- mean(sapply(power_all_test_add_an_effect_new_n$sim_all,
                    function(x) {coef(x$extra$fit)["y~x"]}))
expect_equal(chk1[[2]]["est"],
             chk2,
             ignore_attr = TRUE)

})

