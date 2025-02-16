library(testthat)
suppressMessages(library(lavaan))

# All-In-One

test_that("power4test", {

test_summary <- function(object) {
  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  test_results_all <- sapply(object,
                             function(xx) xx$test_results)
  test_results_all <- as.data.frame(t(test_results_all))
  colMeans(test_results_all, na.rm = TRUE)
}

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

power_all_sim_only <- power4test(nrep = 10,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 n = 100,
                                 number_of_indicators = k,
                                 reliability = rel,
                                 fit_model_args = list(estimator = "ML"),
                                 R = 50,
                                 do_the_test = FALSE,
                                 iseed = 1234,
                                 parallel = FALSE,
                                 progress = FALSE)

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

power_all_test_only <- power4test(sim_all = power_all_sim_only,
                                  test_fun = manymome::indirect_effect,
                                  test_args = list(x = "x",
                                                   m = "m",
                                                   y = "y",
                                                   mc_ci = TRUE),
                                  map_names = c(fit = "fit",
                                                mc_out = "mc_out"),
                                  results_fun = ind_results,
                                  parallel = FALSE,
                                  progress = FALSE)

summary_all <- test_summary(power_all_test_only)

fits <- lapply(power_all_sim_only$sim_all,
               function(x) x$extra$fit)
mc_outs <- lapply(power_all_sim_only$sim_all,
                  function(x) x$extra$mc_out)
ind_outs <- mapply(manymome::indirect_effect,
                   fit = fits,
                   mc_out = mc_outs,
                   MoreArgs = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  SIMPLIFY = FALSE)
ind_cis <- t(sapply(ind_outs,
                    confint))
ind_sigs <- (ind_cis[, 1] > 0) | (ind_cis[, 2] < 0)

expect_equal(summary_all["sig"],
             mean(ind_sigs),
             ignore_attr = TRUE)
expect_equal(summary_all[c("cilo", "cihi")],
             colMeans(ind_cis),
             ignore_attr = TRUE)

power_all_test_only2 <- power4test(sim_all = power_all_test_only,
                                   test_fun = manymome::indirect_effect,
                                   test_args = list(x = "x",
                                                    y = "y",
                                                    mc_ci = TRUE),
                                   map_names = c(fit = "fit",
                                                 mc_out = "mc_out"),
                                   results_fun = ind_results,
                                   parallel = FALSE,
                                   progress = FALSE)

summary_all <- test_summary(power_all_test_only2)

ind_outs <- mapply(manymome::indirect_effect,
                   fit = fits,
                   mc_out = mc_outs,
                   MoreArgs = list(x = "x",
                                   y = "y",
                                   mc_ci = TRUE),
                  SIMPLIFY = FALSE)
ind_cis <- t(sapply(ind_outs,
                    confint))
ind_sigs <- (ind_cis[, 1] > 0) | (ind_cis[, 2] < 0)

expect_equal(summary_all["sig"],
             mean(ind_sigs),
             ignore_attr = TRUE)
expect_equal(summary_all[c("cilo", "cihi")],
             colMeans(ind_cis),
             ignore_attr = TRUE)

})

