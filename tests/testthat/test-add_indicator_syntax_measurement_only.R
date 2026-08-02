library(testthat)
suppressMessages(library(lavaan))

test_that("add_indicator_syntax: Measurement only", {

mod <-
"
m ~ x
y ~ m + x
"

out <- add_indicator_syntax(
  model,
  number_of_indicators = c(y = 3, x = 2),
  measurement_only = TRUE
)

out_pt <- lavaanify(
  out
)

out_pt_chk <- lavaanify(
  "y =~ y1 + y2 + y3\nx =~ x1 + x2"
)

expect_identical(
  out_pt[, c("lhs", "op", "rhs")],
  out_pt_chk[, c("lhs", "op", "rhs")]
)

})