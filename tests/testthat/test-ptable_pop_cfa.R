library(testthat)
suppressMessages(library(lavaan))

check_gen_dat <- function(model,
                          pop_es,
                          n = 10000,
                          seed = NULL,
                          number_of_indicators = NULL,
                          reliability = NULL,
                          keep_f_scores = FALSE) {
  set.seed(seed)
  ptable <- ptable_pop(model,
                       pop_es = pop_es,
                       standardized = TRUE)
  mm_out <- model_matrices_pop(ptable)
  mm_lm_out <- mm_lm(mm_out)
  mm_lm_dat_out <- mm_lm_data(mm_lm_out,
                              n = n,
                              number_of_indicators = number_of_indicators,
                              reliability = reliability,
                              keep_f_scores = keep_f_scores)
  model <- add_indicator_syntax(model,
                                number_of_indicators = number_of_indicators,
                                reliability = reliability)
  fit <- lavaan::sem(model,
             data = mm_lm_dat_out)
  tmp <- ptable
  tmp$est <- tmp$start
  fit0 <- lavaan::sem(tmp,
              do.fit = FALSE)
  out <- list(ptable = ptable,
              mm_out = mm_out,
              mm_lm_out = mm_lm_out,
              mm_lm_dat_out = mm_lm_dat_out,
              fit = fit,
              fit0 = fit0)
  out
}

test_check_out <- function(check_out) {
  expect_equal(diag(implied_sigma(check_out$mm_out)),
              rep(1, ncol(check_out$mm_lm_dat_out)),
              tolerance = 1e-1,
              ignore_attr = TRUE)
  # expect_equal(check_out$mm_out$psi,
  #             check_out$mm_out_std$psi)
  expect_equal(diag(cov(check_out$mm_lm_dat_out)),
              rep(1, ncol(check_out$mm_lm_dat_out)),
              tolerance = 1e-1,
              ignore_attr = TRUE)
  expect_equal(coef(check_out$fit),
              coef(check_out$fit0),
              tolerance = 1e-1)
  # expect_equal(check_out$mm_out_std$psi,
  #             check_out$mm_out_std2$psi,
  #             tolerance = 1e-1)
  # expect_equal(check_out$ptable_std$start,
  #             check_out$ptable_std2$start,
  #             tolerance = 1e-1)
}

test_that("ptable_pop: cfa", {

# Correlation model

model_cfa <-
"
f1 ~~ f2 + f3
f2 ~~ f3
"

model_cfa_es <- c(".cov." = "m",
                  "f1 ~~ f2" = "l",
                  "f2 ~~ f3" = "m")

check_out <- check_gen_dat(model_cfa,
                           model_cfa_es,
                           seed = 1234)

expect_equal(check_out$ptable$start[1:3],
             c(.50, .00, .30))

# test_check_out(check_out)

# CFA

model_cfa <-
"
f1 ~~ f2 + f3
f2 ~~ f3
"

model_cfa_es <- c(".cov." = "m",
                  "f1 ~~ f2" = "l",
                  "f2 ~~ f3" = "m")

check_out <- check_gen_dat(model_cfa,
                           model_cfa_es,
                           seed = 1234,
                           number_of_indicators = c(f1 = 4,
                                                    f3 = 3,
                                                    f2 = 5),
                           reliability = c(f2 = .70,
                                           f1 = .80,
                                           f3 = .60))

expect_equal(standardizedSolution(check_out$fit)$est.std[1:3],
             c(.50, .00, .30),
             tolerance = 1e-1)

})
