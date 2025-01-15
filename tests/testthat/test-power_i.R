skip("WIP")

library(testthat)
library(pbapply)
suppressMessages(library(lavaan))

indirect_effect_mc <- function(x,
                               m,
                               y,
                               fit,
                               R = 100) {
  ptable <- lavaan::parameterTable(fit)
  id_a <- ptable$id[which((ptable$lhs == m) & (ptable$rhs == x))]
  id_b <- ptable$id[which((ptable$lhs == y) & (ptable$rhs == m))]
  fit_vcov <- lavaan::lavInspect(fit,
                                 "vcov")
  ab_vcov <- fit_vcov[c(id_a, id_b), c(id_a, id_b)]
  ab_est <- ptable$est[c(id_a, id_b)]
  ab_est_mc <- MASS::mvrnorm(n = R,
                             mu = ab_est,
                             Sigma = ab_vcov)
  ab_mc <- apply(ab_est_mc,
                 MARGIN = 1,
                 prod)
  ab_ci <- stats::quantile(ab_mc,
                           c(.025, .975))
  out1 <- ifelse((ab_ci[1] > 0) || (ab_ci[2] < 0),
                  yes = 1,
                  no = 0)
  est <- lavaan::parameterEstimates(fit,
                                    se = FALSE,
                                    standardized = TRUE)
  id_a <- which((est$lhs == m) & (est$rhs == x))
  id_b <- which((est$lhs == y) & (est$rhs == m))
  ab_std <- est[id_a, "std.all"] * est[id_b, "std.all"]
  out2 <- c(est = ab_std,
            cilo = unname(ab_ci[1]),
            cihi = unname(ab_ci[2]),
            sig = out1)
}

power_i <- function(model,
                    pop_es,
                    n = 10000,
                    seed = NULL,
                    number_of_indicators = NULL,
                    reliability = NULL,
                    FUN_test = NULL) {
  if (!is.function(FUN_test)) {
    stop("'FUN_test' must be a function.")
  }
  if (!is.null(seed)) set.seed(seed)
  # TODO:
  # - ptable_pop() should be called only once
  #   because it will find the error variances empirically.
  ptable <- ptable_pop(model,
                       pop_es = pop_es,
                       standardized = TRUE)
  mm_out <- model_matrices_pop(ptable)
  mm_lm_out <- mm_lm(mm_out)
  mm_lm_dat_out <- mm_lm_data(mm_lm_out,
                              n = n,
                              number_of_indicators = number_of_indicators,
                              reliability = reliability,
                              keep_f_scores = FALSE)
  model <- add_indicator_syntax(model,
                                number_of_indicators = number_of_indicators,
                                reliability = reliability)
  fit <- lavaan::sem(model,
                     data = mm_lm_dat_out)
  test_sig <- do.call(FUN_test,
                      list(fit))
  tmp <- ptable
  tmp$est <- tmp$start
  fit0 <- lavaan::sem(tmp,
              do.fit = FALSE)
  out <- list(ptable = ptable,
              mm_out = mm_out,
              mm_lm_out = mm_lm_out,
              mm_lm_dat_out = mm_lm_dat_out,
              fit = fit,
              fit0 = fit0,
              test_sig = test_sig)
  out
}

check_power <- function(check_out) {
  sig_all <- sapply(check_out,
                    function(xx) xx$test_sig["sig"])
  mean(sig_all,
       na.rm = TRUE)
}

get_est <- function(check_out) {
  sapply(check_out,
         function(xx) xx$test_sig["est"])
}


test_that("power_i", {

# Simple mediation model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

ind_p <- function(fit0) {
  out0 <- manymome::indirect_effect(x = "x",
                                    y = "y",
                                    m = "m",
                                    fit = fit0,
                                    mc_ci = TRUE,
                                    R = 1000,
                                    standardized_x = TRUE,
                                    standardized_y = TRUE,
                                    parallel = FALSE,
                                    progress = FALSE)
  ci0 <- stats::confint(out0)
  out1 <- ifelse((ci0[1, 1] > 0) || (ci0[1, 2] < 0),
                  yes = 1,
                  no = 0)
  out2 <- c(est = unname(coef(out0)),
            cilo = ci0[1, 1],
            cihi = ci0[1, 2],
            sig = out1)
  return(out2)
}

set.seed(1234)
check_out <- pbreplicate(
             n = 100,
             power_i(model_simple_med,
                     model_simple_med_es,
                     n = 100,
                     FUN_test = ind_p),
             simplify = FALSE)

check_power(check_out)
est <- get_est(check_out)
mean(est)
pop <- coef(check_out[[1]]$fit0)
pop[1] * pop[2]


# Simple mediation model: With Indicators

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "m",
                         "m ~ x" = "l",
                         "y ~ x" = "n")

ind_p <- function(fit0) {
  out0 <- indirect_effect_mc(x = "x",
                             m = "m",
                             y = "y",
                             fit = fit0,
                             R = 1000)
  return(out0)
}

set.seed(1234)
check_out <- pbreplicate(
             n = 100,
             power_i(model_simple_med,
                     model_simple_med_es,
                     n = 200,
                     number_of_indicators = c(y = 3,
                                              x = 3,
                                              m = 3),
                     reliability = c(x = .70,
                                     m = .70,
                                     y = .70),
                     FUN_test = ind_p),
             simplify = FALSE)

check_power(check_out)
est <- get_est(check_out)
mean(est)
pop <- coef(check_out[[1]]$fit0)
pop[1] * pop[2]


# Parallel mediation model
# Correlated errors

model_med_parallel <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
m1 ~~ m2
"

model_med_parallel_es <- c(".beta." = "s",
                           "y ~ x" = "-m",
                           "m1 ~ x" = "l",
                           "m2 ~ x" = "s",
                           "y ~ m1" = "m",
                           "m1 ~~ m2" = "l")

check_out <- check_gen_dat(model_med_parallel,
                           model_med_parallel_es,
                           seed = 1234,
                           number_of_indicators = c(y = 4,
                                                    x = 3,
                                                    m2 = 5),
                           reliability = c(x = .70,
                                           m2 = .80,
                                           y = .60))

test_check_out(check_out)

# Parallel mediation model
# Correlated errors not specified in the model

model_med_parallel <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
"

model_med_parallel_es <- c(".beta." = "s",
                           "y ~ x" = "-m",
                           "m1 ~~ m2" = "l")

check_out <- check_gen_dat(model_med_parallel,
                                          model_med_parallel_es,
                                          seed = 1234,
                           number_of_indicators = c(y = 4,
                                                    m1 = 2,
                                                    m2 = 5),
                           reliability = c(m1 = .70,
                                           m2 = .80,
                                           y = .60))
test_check_out(check_out)

# Moderated mediation model

model_mod_med <-
"
m ~ x + w
y ~ m + z + m:z + x + u
"

model_mod_med_es <- c(".beta." = "s",
                      "y ~ x" = "-m",
                      "x ~~ w + z + u" = "s",
                      "w ~~ z + u" = "l")

check_out <- check_gen_dat(model_mod_med,
                           model_mod_med_es,
                           seed = 1234,
                           number_of_indicators = c(x = 4,
                                                    w = 2,
                                                    y = 5),
                           reliability = c(x = .70,
                                           w = .80,
                                           y = .60))

test_check_out(check_out)

})
