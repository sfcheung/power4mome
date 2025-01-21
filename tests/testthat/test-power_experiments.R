skip("Test in an interactive section due to parallel processing")

library(testthat)
library(pbapply)
library(parallel)
suppressMessages(library(lavaan))

# Simple Mediation Model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")
k <- c(y = 8,
       m = 8,
       x = 8)
rel <- c(y = .70,
         m = .80,
         x = .70)

data_i <- sim_data_i(model = model_simple_med,
                     pop_es = model_simple_med_es,
                     n = 200,
                     number_of_indicators = k,
                     reliability = rel,
                     seed = 1234)
names(data_i)

data_all <- sim_data(nrep = 500,
                     model = model_simple_med,
                     pop_es = model_simple_med_es,
                     n = 200,
                     number_of_indicators = k,
                     reliability = rel,
                     iseed = 1234,
                     parallel = TRUE,
                     progress = TRUE)
head(data_all[[1]]$mm_lm_dat_out)[, 1:5]
head(data_all[[2]]$mm_lm_dat_out)[, 1:5]

fit_i <- fit_model_i(data_i)

fit_model_i(data_all[[1]])
fit_model_i(data_all[[2]])

fit_all <- fit_model(data_all,
                     parallel = TRUE,
                     progress = TRUE)

fit_all[[1]]
fit_all[[2]]

mc_i <- gen_mc_i(fit_i = fit_i,
                 R = 1000)
mc_i

mc_all <- gen_mc(fit_all,
                 R = 1000,
                 parallel = TRUE,
                 progress = TRUE)
mc_all[[1]]

length(data_all)
length(fit_all)
length(mc_all)

out_all <- sim_out(data_all = data_all,
                   fit_all = fit_all)
out_all[[1]]$mc_out

out_all <- sim_out(data_all = data_all,
                   fit_all = fit_all,
                   mc_all = mc_all)
out_all[[1]]$mc_out

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

test_i <- do_test_i(out_all[[1]],
                    test_fun = manymome::indirect_effect,
                    test_args = list(x = "x",
                                     m = "m",
                                     y = "y",
                                     mc_ci = TRUE),
                    fit_name = "fit",
                    mc_out_name = "mc_out",
                    results_fun = ind_results)
test_i

test_all <- do_test(out_all,
                    test_fun = manymome::indirect_effect,
                    test_args = list(x = "x",
                                     m = "m",
                                     y = "y",
                                     mc_ci = TRUE),
                    fit_name = "fit",
                    mc_out_name = "mc_out",
                    results_fun = ind_results,
                    parallel = TRUE,
                    progress = TRUE)
names(test_all[[1]])
test_results_all <- sapply(test_all, function(xx) xx$test_results)
test_results_all <- as.data.frame(t(test_results_all))
head(test_results_all)
colMeans(test_results_all)
mean(test_results_all$sig)

# All-In-One

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

# test_lrt <- function(object,
#                      par,
#                      alpha = .05) {
#   lrt_out <- semlrtp::lrtp(object,
#                            op = "~",
#                            progress = FALSE)
#   lrt_out <- as.data.frame(lrt_out)
#   if (par %in% lrt_out$label) {
#     i <- (lrt_out$lhs == par) &
#          (lrt_out$label == par)
#     i <- which(i)
#   } else {
#     par1 <- lavaan::lavParseModelString(par,
#                                         as.data.frame. = TRUE)
#     i <- (lrt_out$lhs == par1$lhs) &
#         (lrt_out$op == par1$op) &
#         (lrt_out$rhs == par1$rhs)
#     i <- which(i)
#   }
#   out <- c(est = lrt_out[i, "est"],
#            cilo = NA,
#            cihi = NA,
#            sig = as.numeric(lrt_out[i, "LRTp"] < alpha))
#   out
# }

# test_lrt_ind <- function(object,
#                          alpha = .05) {
#   lrt_out <- semlrtp::lrtp(object,
#                            op = "~",
#                            progress = FALSE)
#   lrt_out <- as.data.frame(lrt_out)
#   lrt_ind_p <- max(c(lrt_out[1, "LRTp"], lrt_out[2, "LRTp"]))
#   out <- c(est = lrt_out[25, "est"],
#            cilo = NA,
#            cihi = NA,
#            sig = lrt_ind_p < alpha)
#   out
# }

# test_lrt(power_all_sim_only$sim_all[[1]]$fit,
#          par = "y ~ m")

# test_lrt_ind(power_all_sim_only$sim_all[[1]]$fit)


test_par(power_all_sim_only$sim_all[[1]]$fit,
         par = "y ~ m")

test_par(power_all_sim_only$sim_all[[1]]$fit,
         par = "ab")

power_all_sim_only <- power4test(nrep = 500,
                                 model = model_simple_med,
                                 pop_es = model_simple_med_es,
                                 n = 100,
                                 number_of_indicators = k,
                                 reliability = rel,
                                 fit_model_args = list(estimator = "ML"),
                                 R = 1000,
                                 do_the_test = FALSE,
                                 iseed = 1234,
                                 parallel = TRUE,
                                 progress = TRUE)

power_all_test_only <- power4test(sim_all = power_all_sim_only,
                                  test_fun = manymome::indirect_effect,
                                  test_args = list(x = "x",
                                                   m = "m",
                                                   y = "y",
                                                   mc_ci = TRUE),
                                  fit_name = "fit",
                                  mc_out_name = "mc_out",
                                  results_fun = ind_results,
                                  parallel = TRUE,
                                  progress = TRUE)
test_summary(power_all_test_only)

power_all_test_only_par <- power4test(sim_all = power_all_sim_only,
                                      test_fun = test_par,
                                      test_args = list(par = "y ~ m"),
                                      fit_name = "object",
                                      mc_out_name = NULL,
                                      results_fun = par_results,
                                      parallel = TRUE,
                                      progress = TRUE)
test_summary(power_all_test_only_par)

power_all_test_only_par <- power4test(sim_all = power_all_sim_only,
                                      test_fun = test_lrt,
                                      test_args = list(par = "y ~ m"),
                                      fit_name = "object",
                                      mc_out_name = NULL,
                                      results_fun = par_results,
                                      parallel = TRUE,
                                      progress = TRUE)
test_summary(power_all_test_only_par)

power_all_test_only_par <- power4test(sim_all = power_all_sim_only,
                                      test_fun = test_lrt_ind,
                                      fit_name = "object",
                                      mc_out_name = NULL,
                                      results_fun = par_results,
                                      parallel = TRUE,
                                      progress = TRUE)
test_summary(power_all_test_only_par)

power_all_test_only_par <- power4test(sim_all = power_all_sim_only,
                                      test_fun = test_par,
                                      test_args = list(par = "ab"),
                                      fit_name = "object",
                                      mc_out_name = NULL,
                                      results_fun = par_results,
                                      parallel = TRUE,
                                      progress = TRUE)
test_summary(power_all_test_only_par)


power_all <- power4test(nrep = 500,
                        model = model_simple_med,
                        pop_es = model_simple_med_es,
                        n = 200,
                        number_of_indicators = k,
                        reliability = rel,
                        R = 1000,
                        test_fun = manymome::indirect_effect,
                        test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        mc_ci = TRUE),
                        fit_name = "fit",
                        mc_out_name = "mc_out",
                        results_fun = ind_results,
                        iseed = 1234,
                        parallel = TRUE,
                        progress = TRUE)

test_summary(power_all)
