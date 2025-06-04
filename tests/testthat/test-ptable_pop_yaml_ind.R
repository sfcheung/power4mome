library(testthat)
suppressMessages(library(lavaan))

test_that("ptable_pop: indirect", {

# Simple mediation model

mod <-
"
m ~ x
y ~ m + x
"

mod_es1 <- c(".beta." = "m",
             ".ind.(x->m->y)" = "li")

mod_es2 <-
"
.beta.: m
.ind.(x->m->y): li
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

# Parallel mediation model
# Correlated errors

mod <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
m1 ~~ m2
"

mod_es1 <- c(".beta." = "s",
             ".ind.(y ~ m1 ~ x)" = "si",
             ".ind.(x -> m2 -> y)" = "mi",
             "m1 ~~ m2" = "l")

mod_es2 <-
"
.beta.: s
.ind.(y ~ m1 ~ x): si
.ind.(x -> m2 -> y): mi
m1 ~~ m2: l
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

# Parallel mediation model
# Correlated errors not specified in the model

# Moderated mediation model

mod <-
"
m ~ x + w + x:w
y ~ m + z + m:z + x + u + x:u
"

mod_es1 <- c(".beta." = "s",
             "m ~ x:w" = "l",
             ".ind.(x->m->y)" = "li")

mod_es2 <-
"
.beta.: s
m ~ x:w: l
.ind.(x->m->y): li
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

# Other models

mod <-
"
m1 ~ x + c1
m2 ~ m1 + x2 + c1
y ~  m2 + m1 + x + w + x:w + c1
"

mod_es1 <- c(".ind.(x->m1->m2)" = "li",
             "y ~ m2" = "l",
             "y ~ x" = "m",
             "y ~ w" = "s",
             "y ~ x:w" = "s",
             "x ~~ w" = "s")

mod_es2 <-
"
.ind.(x->m1->m2): li
y ~ m2: l
y ~ x: m
y ~ w: s
y ~ x:w: s
y ~~ w: s
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

})
