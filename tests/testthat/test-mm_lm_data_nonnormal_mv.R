
library(testthat)
suppressMessages(library(lavaan))

test_that("Generate nonnormal data: Multivariate", {

mod <-
"
m ~ x + w + x:w
y ~ m + x
"

set.seed(1234)
n <- 100000
pop_es <- c(".beta." = "m",
            "x ~~ w" = "m")

ptable <- ptable_pop(model = mod,
                     pop_es = pop_es)
mm_out <- model_matrices_pop(ptable)
mm_lm_out <- mm_lm(mm_out)

set.seed(1234)
mm_lm_dat_out <- mm_lm_data(mm_lm_out,
                            n = 50000,
                            x_fun = list(rig_rs,
                                         skew = c(x = 2),
                                         kurt = c(x = 6, w = 3)))
chk <- mm_lm_dat_out
chk_skew <- apply(
  chk,
  MARGIN = 2,
  psych::skew
)
chk_kurt <- apply(
  chk,
  MARGIN = 2,
  psych::kurtosi
)
expect_equal(chk[1:10, "x"] * chk[1:10, "w"],
             chk[1:10, "x:w"])
expect_equal(
  round(chk_skew[c("x", "w")]),
  c(2, 0),
  ignore_attr = TRUE
)
expect_equal(
  round(chk_kurt[c("x", "w")]),
  c(6, 3),
  ignore_attr = TRUE
)
expect_equal(
  round(cov(chk)["x", "w"], 2),
  .30,
  ignore_attr = TRUE
)

})
