library(testthat)
suppressMessages(library(lavaan))


check_gen_dat <- function(model,
                          pop_es,
                          n = 10000,
                          seed = NULL) {
  set.seed(seed)
  ptable <- ptable_pop(model,
                       pop_es = pop_es,
                       standardized = TRUE)
  mm_out <- model_matrices_pop(ptable)
  mm_lm_out <- mm_lm(mm_out)
  mm_lm_dat_out <- mm_lm_data(mm_lm_out,
                              n = n)
  out <- list(ptable = ptable,
              mm_out = mm_out,
              mm_lm_out = mm_lm_out,
              mm_lm_dat_out = mm_lm_dat_out)
  out
}


test_that("add_indicator_scores", {

# Simple mediation model

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c(".beta." = "m",
                         "m ~ x" = "l",
                         "y ~ x" = "n")

check_out <- check_gen_dat(model_simple_med,
                           model_simple_med_es,
                           seed = 1234,
                           n = 50000)
dat <- as.matrix(check_out$mm_lm_dat_out)
head(dat)

set.seed(1357)
dat0 <- add_indicator_scores(dat,
                             ps = c(x = 4, m = 7),
                             rels = c(x = .80, m = .70),
                             ds = c(m = 0.05, x = 0.10),
                             refs = c(m = "medium", x = "strongest"))
mod <-
"
x =~ x1 + x2 + x3 + x4
m =~ m1 + m2 + m3 + m4 + m5 + m6 + m7
"
fit <- cfa(mod, data = dat0)
std <- standardizedSolution(fit)

expect_equal(sort(std[1:4, "est.std"]) /
             sort(lambda_from_reliability(p = 4, omega = .80, d = .10)),
             rep(1, 4),
             tolerance = 1e-2)

expect_equal(sort(std[1:7 + 4, "est.std"]) /
             sort(lambda_from_reliability(p = 7, omega = .70, d = .05)),
             rep(1, 7),
             tolerance = 1e-2)

# lambda_from_reliability(p = 4, omega = .80, d = .10, ref = "strongest")
expect_true(which.max(std[1:4, "est.std"]) == 1)

# lambda_from_reliability(p = 7, omega = .70, d = .05, ref = "medium")
expect_true(which.max(std[1:7 + 4, "est.std"]) != 1)
expect_true(which.min(std[1:7 + 4, "est.std"]) != 1)

set.seed(1357)
dat0 <- add_indicator_scores(dat,
                             ps = c(x = 4, m = 7),
                             rels = c(x = .80, m = .70),
                             ds = c(m = 0.05, x = 0.10),
                             refs = c(m = "weakest", x = "medium"))
mod <-
"
x =~ x1 + x2 + x3 + x4
m =~ m1 + m2 + m3 + m4 + m5 + m6 + m7
"
fit <- cfa(mod, data = dat0)
std <- standardizedSolution(fit)

expect_equal(sort(std[1:4, "est.std"]) /
             sort(lambda_from_reliability(p = 4, omega = .80, d = .10)),
             rep(1, 4),
             tolerance = 1e-2)

expect_equal(sort(std[1:7 + 4, "est.std"]) /
             sort(lambda_from_reliability(p = 7, omega = .70, d = .05)),
             rep(1, 7),
             tolerance = 1e-2)

# lambda_from_reliability(p = 4, omega = .80, d = .10, ref = "medium")
expect_true(which.max(std[1:4, "est.std"]) != 1)
expect_true(which.min(std[1:4, "est.std"]) != 1)

# lambda_from_reliability(p = 7, omega = .70, d = .05, ref = "weakest")
expect_true(which.min(std[1:7 + 4, "est.std"]) == 1)

})
