library(testthat)
library(lavaan)

# TODO:
# - Update ptable_pop() and
#   model_matrices_pop() to do
#   standardization by default.

check_gen_dat <- function(model,
                          pop_es,
                          n = 10000,
                          seed = NULL) {
  set.seed(seed)
  ptable <- ptable_pop(model,
                       pop_es = pop_es)
  mm_out <- model_matrices_pop(ptable)
  mm_out_original <- mm_out
  mm_out$psi <- psi_std(mm_out)
  ptable_std <- start_from_mm(ptable,
                              mm_out)
  mm_out_std <- model_matrices_pop(ptable_std)
  mm_lm_out <- mm_lm(mm_out)
  mm_lm_dat_out <- mm_lm_data(mm_lm_out,
                              n = n)
  fit <- lavaan::sem(model,
             data = mm_lm_dat_out)
  tmp <- ptable_std
  tmp$est <- tmp$start
  fit0 <- lavaan::sem(tmp,
              do.fit = FALSE)
  list(ptable = ptable,
       mm_out_original = mm_out_original,
       mm_out = mm_out,
       mm_out_std = mm_out_std,
       ptable_std = ptable_std,
       mm_lm_out = mm_lm_out,
       mm_lm_dat_out = mm_lm_dat_out,
       fit = fit,
       fit0 = fit0)
}

test_that("ptable_pop", {

# Simple mediation model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c(".beta." = "m",
                         "y ~ x" = "n")

check_out <- check_gen_dat(model_simple_med,
                           model_simple_med_es,
                           seed = 1234)

expect_equal(check_out$ptable$start[1:3],
             c(.30, .30, .00))
expect_equal(diag(implied_sigma(check_out$mm_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(check_out$mm_out$psi,
             check_out$mm_out_std$psi)
expect_equal(diag(cov(check_out$mm_lm_dat_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(coef(check_out$fit),
             coef(check_out$fit0),
             tolerance = 1e-1)

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
                           seed = 1234)

expect_equal(check_out$ptable$start[6],
             .50)
expect_equal(diag(implied_sigma(check_out$mm_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(check_out$mm_out$psi,
             check_out$mm_out_std$psi)
expect_equal(diag(cov(check_out$mm_lm_dat_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(coef(check_out$fit),
             coef(check_out$fit0),
             tolerance = 1e-1)

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

# expect_warning(check_out <- check_gen_dat(model_med_parallel,
#                                           model_med_parallel_es,
#                                           seed = 1234))
check_out <- check_gen_dat(model_med_parallel,
                                          model_med_parallel_es,
                                          seed = 1234)
expect_equal(check_out$ptable$start[1:5],
             c(.10, .10, .10, .10, -.30))
expect_equal(diag(implied_sigma(check_out$mm_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(check_out$mm_out$psi,
             check_out$mm_out_std$psi)
expect_equal(diag(cov(check_out$mm_lm_dat_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(coef(check_out$fit),
             coef(check_out$fit0),
             tolerance = 1e-1)

# Moderation only

model_mod <-
"
y ~ x + w + x:w + c1
"

model_mod_es <- c(".beta." = "s",
                  "y ~ w" = "-m",
                  "x ~~ w" = "l")

# expect_warning(check_out <- check_gen_dat(model_mod,
#                                           model_mod_es,
#                                           seed = 1234))
check_out <- check_gen_dat(model_mod,
                           model_mod_es,
                           seed = 1234)
expect_equal(check_out$ptable$start[1:4],
             c(.10, -.30, .05, .10))
expect_equal(check_out$ptable$start[7],
             .50)
expect_equal(diag(implied_sigma(check_out$mm_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(check_out$mm_out$psi,
             check_out$mm_out_std$psi)
expect_equal(diag(cov(check_out$mm_lm_dat_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(coef(check_out$fit),
             coef(check_out$fit0),
             tolerance = 1e-1)

# Moderated mediation model

model_mod_med <-
"
m ~ x + w + x:w
y ~ m + z + m:z + x + u + x:u
"

model_mod_med_es <- c(".beta." = "s",
                      "y ~ x" = "-m",
                      "m ~ x:w" = "l",
                      "x ~~ w + z + u" = "s",
                      "w ~~ z + u" = "l")

check_out <- check_gen_dat(model_mod_med,
                           model_mod_med_es,
                           seed = 1234)
expect_equal(check_out$ptable$start[1:9],
             c(.10, .10, .15, .10, .10, .05, -.30, .10, .05))
expect_equal(check_out$ptable$start[21:22],
             c(.50, .50))
expect_equal(check_out$ptable$start[16:17],
             c(.10, .10))
expect_equal(diag(implied_sigma(check_out$mm_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(check_out$mm_out$psi,
             check_out$mm_out_std$psi)
expect_equal(diag(cov(check_out$mm_lm_dat_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(coef(check_out$fit),
             coef(check_out$fit0),
             tolerance = 1e-1)

# Other models

model2 <-
"
m1 ~ x + c1
m2 ~ m1 + x2 + c1
y ~  m2 + m1 + x + w + x:w + c1
"

model2_es <- c("m1 ~ x" = "-m",
               "m2 ~ m1" = "s",
               "y ~ m2" = "l",
               "y ~ x" = "m",
               "y ~ w" = "s",
               "y ~ x:w" = "s",
               "x ~~ w" = "s")

check_out <- check_gen_dat(model2,
                           model2_es,
                           seed = 1234)
expect_equal(diag(implied_sigma(check_out$mm_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(check_out$mm_out$psi,
             check_out$mm_out_std$psi)
expect_equal(diag(cov(check_out$mm_lm_dat_out)),
             rep(1, ncol(check_out$mm_lm_dat_out)),
             tolerance = 1e-1,
             ignore_attr = TRUE)
expect_equal(coef(check_out$fit),
             coef(check_out$fit0),
             tolerance = 1e-1)

})
