library(testthat)
suppressMessages(library(lavaan))

test_that("c.power4test_by_n: moderation", {

model <-
"
m ~ x + w + x:w
y ~ m + x
"

model_es <-
"
m ~ x: m
m ~ w: s
m ~ x:w: l
y ~ m: l
y ~ x: s
"

out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 50000,
                  progress = !is_testing(),
                  iseed = 1234)

test_out <- power4test(object = out,
                       test_fun = test_moderation)

test_n_out1 <- power4test_by_n(
                  test_out,
                  n = c(100, 110),
                  progress = !is_testing())
test_n_out2 <- power4test_by_n(
                  test_out,
                  n = c(200, 210),
                  progress = !is_testing())
expect_no_error(c(test_n_out1, test_n_out2))

})
