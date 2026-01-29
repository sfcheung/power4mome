library(testthat)
suppressMessages(library(lavaan))

test_that("Multigroup models", {

mod <-
"
m ~ x
y ~  m + x
"

mod_es1 <- list("m ~ x" = "-m",
                "y ~ m" = "l",
                "y ~ x" = c("m", "l", "n"))

mod_es2 <-
"
m ~ x: -m
y ~ m: l
y ~ x: [m, l, nil]
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

})
