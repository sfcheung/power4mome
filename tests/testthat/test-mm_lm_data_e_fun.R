library(testthat)
suppressMessages(library(lavaan))

test_that("Generate nonnormal indicator", {

mod <-
"
m ~ x
y ~ m + x
"

set.seed(1234)
n <- 100000
pop_es <- c(".beta." = "m")

ptable <- ptable_pop(model = mod,
                     pop_es = pop_es)
mm_out <- model_matrices_pop(ptable)
mm_lm_out <- mm_lm(mm_out)

set.seed(1234)
mm_lm_dat_out <- mm_lm_data(mm_lm_out,
                            n = 10000,
                            number_of_indicators = c(x = 4, y = 4, m = 4),
                            reliability = c(x = .20, y = .60, m = .70),
                            e_fun = list(x = list(rexp_rs),
                                         m = list(rbinary_rs,
                                                  p1 = .50)))
chk <- mm_lm_dat_out
head(chk)
tmp <- mm_lm_dat_out[, "x2"]
expect_true(mean(tmp) > median(tmp))

})
