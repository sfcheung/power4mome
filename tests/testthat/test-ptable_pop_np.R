library(testthat)
suppressMessages(library(lavaan))

test_that("ptable_pop: Nonpositive definite", {

# Simple mediation model

mod <-
"
m ~ x
y ~ m + x
"

pop_es <- c("m ~ x" = ".90",
            "y ~ m" = ".90",
            "y ~ x" = ".90")

expect_error(ptable_pop(model = mod,
                        pop_es = pop_es))

})
