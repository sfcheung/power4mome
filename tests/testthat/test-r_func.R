library(testthat)

test_that("r* functions", {

set.seed(1234)
x <- rbeta_rs(n = 50000,
              shape1 = .5,
              shape2 = .5,
              pmean = 3,
              psd = 1)
expect_equal(mean(x),
             3,
             tolerance = 1e-2)
expect_equal(sd(x),
             1,
             tolerance = 1e-2)

set.seed(1234)
x <- rbeta_rs2(n = 50000,
               blow = 3,
               bhigh = 10,
               bmean = 5,
               bsd = 1)
expect_equal(mean(x),
             5,
             tolerance = 1e-2)
expect_equal(sd(x),
             1,
             tolerance = 1e-2)
expect_true(max(x) < 10)
expect_true(min(x) > 3)

set.seed(1234)
x <- rbinary_rs(n = 50000,
                p1 = .2,
                pmean = 3,
                psd = 4)
expect_equal(mean(x),
             3,
             tolerance = 1e-2)
expect_equal(sd(x),
             4,
             tolerance = 1e-2)
expect_equal(table(x) / length(x),
             c(.80, .20),
             ignore_attr = TRUE,
             tolerance = 1e-2)

set.seed(1234)
x <- rexp_rs(n = 50000,
             pmean = 3,
             psd = 4)
expect_equal(mean(x),
             3,
             tolerance = 1e-1)
expect_equal(sd(x),
             4,
             tolerance = 1e-1)
expect_true(median(x) < mean(x))

set.seed(1234)
x <- runif_rs(n = 50000,
              min = -3,
              max = 4,
              pmean = 3,
              psd = 2)
expect_equal(mean(x),
             3,
             tolerance = 1e-1)
expect_equal(sd(x),
             2,
             tolerance = 1e-1)

set.seed(1234)
x <- rt_rs(n = 50000,
           df = 5,
           pmean = 3,
           psd = 2)
expect_equal(mean(x),
             3,
             tolerance = 1e-1)
expect_equal(sd(x),
             2,
             tolerance = 1e-1)

set.seed(1234)
x <- rpgnorm_rs(n = 50000,
                p = 1,
                pmean = 3,
                psd = 2)
expect_equal(mean(x),
             3,
             tolerance = 1e-1)
expect_equal(sd(x),
             2,
             tolerance = 1e-1)

set.seed(1234)
x <- rlnorm_rs(n = 50000,
               pmean = 3,
               psd = 2)
expect_equal(mean(x),
             3,
             tolerance = 1e-1)
expect_equal(sd(x),
             2,
             tolerance = 1e-1)
expect_true(min(x) > 0)

})
