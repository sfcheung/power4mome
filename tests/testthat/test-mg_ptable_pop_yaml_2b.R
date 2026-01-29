library(testthat)
suppressMessages(library(lavaan))

test_that("Multigroup models", {

mod <-
"
m1 ~ x
m2 ~ x
y ~  m1 + m2 + x
"

mod_es1 <- list(".beta." = "-s",
                "m1 + m2 ~ x" = "-m",
                "y ~ m1" = "l",
                "y ~ m2" = c("m", "l"))

mod_es2 <-
"
.beta.: -s
m1 + m2 ~ x: -m
y ~ m1: l
y ~ m2: [m, l]
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

})
