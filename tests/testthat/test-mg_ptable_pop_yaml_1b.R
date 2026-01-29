library(testthat)
suppressMessages(library(lavaan))

# Moderated mediation multigroup model.
# Some are latent factors.

test_that("Multigroup models", {

mod <-
"
m1 ~ x
m2 ~ m1 + x
y ~  m2 + m1 + x + w + m2:w
"
mod_es1 <- list("m1 ~ x" = "m",
                "m2 ~ m1" = "s",
                "y ~ m2:w" = c("-m", "n"))

mod_es2 <-
"
m1 ~ x: m
m2 ~ m1: s
y ~ m2:w: [-m, nil]
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

})
