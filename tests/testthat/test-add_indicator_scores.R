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
                           seed = 1234)
dat <- as.matrix(check_out$mm_lm_dat_out)
head(dat)
dat0 <- add_indicator_scores(dat,
                             ps = c(x = 4, m = 2),
                             rels = c(x = .30, m = .70))
expect_setequal(colnames(dat0),
                c("y", "x1", "x2", "x3", "x4", "m1", "m2"))
dat0 <- add_indicator_scores(dat,
                             ps = c(x = 4, m = 2),
                             rels = c(x = .30, m = .70),
                             keep_f_scores = TRUE)
expect_setequal(colnames(dat0),
                c("m", "y", "x", "x1", "x2", "x3", "x4", "m1", "m2"))
dat0 <- add_indicator_scores(dat,
                             ps = c(x = 4, m = 2, y = 10),
                             rels = c(x = .30, m = .70, y = .90))
expect_false(any(c("x", "m", "y") %in% colnames(dat0)))

expect_error(add_indicator_scores(dat,
                                  ps = c(x = 4, m = 2, y2 = 10),
                                  rels = c(x = .30, m = .70, y = .90)))
expect_error(add_indicator_scores(dat,
                                  ps = c(x = 4, m = 2),
                                  rels = c(x = .30, m = .70, y = .90)))
expect_error(add_indicator_scores(dat,
                                  ps = c(x = 4, m = 2, z = 10),
                                  rels = c(x = .30, m = .70, z = .90)))

})
