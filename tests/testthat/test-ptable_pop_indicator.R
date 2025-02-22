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
             data = mm_lm_dat_out,
             fixed.x = FALSE)
  tmp <- ptable
  tmp$est <- tmp$start
  fit0 <- lavaan::sem(tmp,
              do.fit = FALSE,
              fixed.x = FALSE)
  out <- list(ptable = ptable,
              mm_out = mm_out,
              mm_lm_out = mm_lm_out,
              mm_lm_dat_out = mm_lm_dat_out,
              fit = fit,
              fit0 = fit0)
  out
}

test_check_out <- function(check_out) {
  std <- lavaan::standardizedSolution(check_out$fit,
                                      se = FALSE)
  expect_equal(std$est.std[1:3],
               coef(check_out$fit0)[1:3],
               ignore_attr = TRUE,
               tolerance = 1e-1)
}

test_that("ptable_pop: With indicator scores", {

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
                           number_of_indicators = c(y = 4,
                                                    x = 3,
                                                    m = 5),
                           reliability = c(x = .70,
                                           m = .80,
                                           y = .60))

test_check_out(check_out)

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
