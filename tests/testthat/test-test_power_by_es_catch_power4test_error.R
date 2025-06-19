skip_on_cran()

library(testthat)

test_that("Power by es", {

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
                  progress = FALSE,
                  parallel = FALSE)
tmp <- power4test_by_es(out,
                        pop_es_name = ".ind.x->m->y",
                        pop_es_values = seq(.75, .90, .05),
                        by_nrep = 10,
                        progress = FALSE)
expect_true(length(tmp) == 2)
})
