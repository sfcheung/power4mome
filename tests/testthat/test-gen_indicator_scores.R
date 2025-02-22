library(testthat)
suppressMessages(library(lavaan))

omega_test <- function(data) {
  p <- ncol(data)
  mod <- paste("f1 =~",
              paste0("item", seq_len(p), collapse = " + "))
  fit <- lavaan::cfa(mod, data)
  lambda <- c(1, coef(fit)[1:(p - 1)])
  f_var <- lavaan::lavInspect(fit, "cov.lv")[1, 1]
  total_var <- sum(lavaan::lavInspect(fit, "sampstats")$cov)
  omega <- (sum(lambda)^2) * f_var / total_var
  omega
}

test_that("gen_indicator_scores", {

set.seed(1234)
n <- 50000
f <- rnorm(n)
p0 <- 7
omega0 <- .65
dat <- gen_indicator_scores(f_score = f,
                            p = p0,
                            omega = omega0,
                            prefix = "item")
dat <- as.data.frame(dat)
expect_equal(omega_test(dat),
             omega0,
             tolerance = 1e-2)
expect_equal(apply(dat, 2, sd),
             rep(1, p0),
             ignore_attr = TRUE,
             tolerance = 1e-2)
expect_equal(colMeans(dat),
             rep(0, p0),
             ignore_attr = TRUE,
             tolerance = 1e-2)

set.seed(1234)
n <- 50000
f <- rnorm(n)
p0 <- 3
omega0 <- .70
dat <- gen_indicator_scores(f_score = f,
                            p = p0,
                            omega = omega0,
                            prefix = "item")
dat <- as.data.frame(dat)
expect_equal(omega_test(dat),
             omega0,
             tolerance = 1e-2)

set.seed(1234)
n <- 50000
f <- rnorm(n)
p0 <- 3
omega0 <- .20
dat <- gen_indicator_scores(f_score = f,
                            p = p0,
                            omega = omega0,
                            prefix = "item")
dat <- as.data.frame(dat)
expect_equal(omega_test(dat),
             omega0,
             tolerance = 1e-2)

})
