skip_on_cran()

library(testthat)

test_that("Power by n", {

mod <-
"
m ~ x
y ~ m + x
"
mod_es <-
"
y ~ x: s
.ind.x->m->y: .09
"
out <- power4test(nrep = 5,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  R = 1000,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 1234,
                  progress = !is_testing(),
                  parallel = FALSE)
tmp <- power4test_by_n(out,
                       n = c(10, 50, -10),
                       by_nrep = 10,
                       progress = !is_testing())
expect_true(length(tmp) == 2)
expect_equal(names(tmp), c("10", "50"))
})
