library(testthat)

test_that("miss_pattern", {

set.seed(1234)
n <- 200
p <- 8
prop <- .50
dat <- matrix(rnorm(n * p),
              n, p)
dat[sample.int(n * p, ceiling(n * prop))] <- NA
# dat[sample.int(n, ceiling(n * (1 - prop))), ] <- 1
dat <- as.data.frame(dat)

out <- miss_pattern(dat)
chk0 <- !is.na(dat)
expect_equal(attr(out, "nvalid"),
             colSums(chk0))
expect_equal(as.numeric(rownames(out)[1]),
             sum(complete.cases(dat)))

})
