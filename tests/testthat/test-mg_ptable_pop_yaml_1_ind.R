library(testthat)
library(lavaan)

test_that("Multigroup models: .ind.", {

mod <-
"
m1 ~ x + c1
m2 ~ m1 + x2 + c1
y ~  m2 + m1 + x + w + x:w + c1
"
mod_es1 <- list(".ind.(y~m2~m1~x)" = "si",
                "y ~ x" = c("m", "l", "n"),
                "y ~ w" = "s",
                "y ~ x:w" = "s",
                "x ~~ w" = c("s", "m", "l"))

mod_es2 <-
"
.ind.(x->m1->m2->y): si
y ~ x: [m, l, nil]
y ~ w: s
y ~ x:w: s
x ~~ w: [s, m, l]
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

})
