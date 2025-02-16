library(testthat)
suppressMessages(library(lavaan))

test_that("ptable_pop: Wrong pop_es", {

# Simple mediation model

mod <-
"
m ~ x
y ~ m + x
"

pop_es <- c("m ~ x2" = "m",
            "y ~ m" = "s",
            "y ~ x" = "s")

expect_warning(ptable_pop(model = mod,
                          pop_es = pop_es))
})
