skip("WIP")

library(testthat)
library(pbapply)
library(parallel)
suppressMessages(library(lavaan))

# indirect_effect_mc <- function(x,
#                                m,
#                                y,
#                                fit,
#                                R = 100) {
#   ptable <- lavaan::parameterTable(fit)
#   id_a <- ptable$id[which((ptable$lhs == m) & (ptable$rhs == x))]
#   id_b <- ptable$id[which((ptable$lhs == y) & (ptable$rhs == m))]
#   fit_vcov <- lavaan::lavInspect(fit,
#                                  "vcov")
#   ab_vcov <- fit_vcov[c(id_a, id_b), c(id_a, id_b)]
#   ab_est <- ptable$est[c(id_a, id_b)]
#   ab_est_mc <- MASS::mvrnorm(n = R,
#                              mu = ab_est,
#                              Sigma = ab_vcov)
#   ab_mc <- apply(ab_est_mc,
#                  MARGIN = 1,
#                  prod)
#   ab_ci <- stats::quantile(ab_mc,
#                            c(.025, .975))
#   out1 <- ifelse((ab_ci[1] > 0) || (ab_ci[2] < 0),
#                   yes = 1,
#                   no = 0)
#   est <- lavaan::parameterEstimates(fit,
#                                     se = FALSE,
#                                     standardized = TRUE)
#   id_a <- which((est$lhs == m) & (est$rhs == x))
#   id_b <- which((est$lhs == y) & (est$rhs == m))
#   ab_std <- est[id_a, "std.all"] * est[id_b, "std.all"]
#   out2 <- c(est = ab_std,
#             cilo = unname(ab_ci[1]),
#             cihi = unname(ab_ci[2]),
#             sig = out1)
# }

# power_i <- function(gen_all_out = NULL,
#                     FUN_test = NULL) {
#   test_sig <- do.call(FUN_test,
#                       list(fit0 = gen_all_out$fit,
#                            mc_out = gen_all_out$mc_out))
#   return(list(test_sig = test_sig))
# }

# check_power <- function(check_out) {
#   sig_all <- sapply(check_out,
#                     function(xx) xx$test_sig["sig"])
#   mean(sig_all,
#        na.rm = TRUE)
# }

# get_est <- function(check_out) {
#   sapply(check_out,
#          function(xx) xx$test_sig["est"])
# }

# ind_p <- function(fit0,
#                   mc_out = NULL) {
#   out0 <- manymome::indirect_effect(x = "x",
#                                     y = "y",
#                                     m = "m",
#                                     fit = fit0,
#                                     mc_ci = TRUE,
#                                     mc_out = mc_out,
#                                     R = 1000,
#                                     standardized_x = TRUE,
#                                     standardized_y = TRUE,
#                                     parallel = FALSE,
#                                     progress = FALSE)
#   ci0 <- stats::confint(out0)
#   out1 <- ifelse((ci0[1, 1] > 0) || (ci0[1, 2] < 0),
#                   yes = 1,
#                   no = 0)
#   out2 <- c(est = unname(coef(out0)),
#             cilo = ci0[1, 1],
#             cihi = ci0[1, 2],
#             sig = out1)
#   return(out2)
# }

# Simple Mediation Model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")
k <- c(y = 4,
       m = 5,
       x = 3)
rel <- c(y = .70,
         m = .80,
         x = .70)

data_i <- sim_data_i(model = model_simple_med,
                     pop_es = model_simple_med_es,
                     n = 100,
                     number_of_indicators = k,
                     reliability = rel,
                     seed = 1234)
names(data_i)

data_all <- sim_data(nrep = 500,
                     model = model_simple_med,
                     pop_es = model_simple_med_es,
                     n = 100,
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

out_all <- merge_all(data_all = data_all,
                     fit_all = fit_all)
out_all[[1]]$mc_out

out_all <- merge_all(data_all = data_all,
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
test_all <- as.data.frame(do.call(rbind, test_all))
head(test_all)
colMeans(test_all)
mean(test_all$sig)
