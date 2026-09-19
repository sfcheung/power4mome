library(testthat)
suppressMessages(library(lavaan))

test_that("Check auto_cov order", {

model2 <-
"
m ~ x
y ~ m + x + w + w:m
"
model_es2 <-
"
m ~ x: m
y ~ w: s
y ~ w:m: l
y ~ m: m
y ~ x: s
"
out2 <- power4test(nrep = 2,
                  model = model2,
                  pop_es = model_es2,
                  n = 500,
                  iseed = 1234,
                  parallel = FALSE,
                  progress = !is_testing())
fit2 <- out2$sim_all[[1]]$extra$fit
expect_equal(
  unname(fitMeasures(fit2, "df")),
  0
)

})
