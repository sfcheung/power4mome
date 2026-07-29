library(testthat)

test_that("r* functions: Multivariate", {

set.seed(1234)
sigma <- matrix(c(1, .3, .3, 1), 2, 2)
x <- rig_rs(
      n = 500000,
      sigma = sigma,
      skew = c(0, 2),
      kurt = c(0, 6),
      pmean = c(0, -2),
      psd = c(1, 2)
    )

expect_equal(
  colMeans(x),
  c(0, -2),
  tolerance = 1e-2
)
expect_equal(
  apply(x, MARGIN = 2, sd),
  c(1, 2),
  tolerance = 1e-2
)
expect_equal(
  round(apply(x, MARGIN = 2, psych::skew)),
  c(0, 2)
)
expect_equal(
  round(apply(x, MARGIN = 2, psych::kurtosi)),
  c(0, 6)
)
expect_equal(
  cor(x),
  sigma,
  tolerance = 1e-2
)

set.seed(1234)
sigma <- matrix(c(1, -.3, -.3, 1), 2, 2)
x <- rig_rs(
      n = 500000,
      sigma = sigma,
      skew = c(2, -1),
      kurt = c(6, 4),
      pmean = c(5, 10),
      psd = c(4, 3)
    )

expect_equal(
  colMeans(x),
  c(5, 10),
  tolerance = 1e-2
)
expect_equal(
  apply(x, MARGIN = 2, sd),
  c(4, 3),
  tolerance = 1e-2
)
expect_equal(
  round(apply(x, MARGIN = 2, psych::skew)),
  c(2, -1)
)
expect_equal(
  round(apply(x, MARGIN = 2, psych::kurtosi)),
  c(6, 4)
)
expect_equal(
  cor(x),
  sigma,
  tolerance = 1e-2
)

set.seed(1234)
sigma <- matrix(c(1, -.3, -.3, 1), 2, 2)
expect_error(
  rig_rs(
      n = 500000,
      sigma = sigma,
      skew = c(2, 2),
      kurt = c(6, 1),
      pmean = c(5, 10),
      psd = c(4, 3)
    ),
  "values"
)
})
