library(testthat)
suppressMessages(library(lavaan))

test_that("Multigroup models", {

model2 <-
"
m ~ x + w + x:w
y ~  m + x
"

model2_es <- list("m ~ x" = "-m",
                  "y ~ m" = "l",
                  "y ~ x" = c("m", "l", "n"))

dats <- sim_data(nrep = 9,
                 model = model2,
                 pop_es = model2_es,
                 n = c(100, 200, 300),
                 iseed = 1234,
                 progress = !is_testing())
tmp <- capture.output(print(dats))
expect_true(all(!grepl("function", tmp)))
})
