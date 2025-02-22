library(testthat)
suppressMessages(library(lavaan))

test_that("ptable_pop", {

# Simple mediation model

mod <-
"
m ~ x
y ~ m + x
"

mod_es1 <- c(".beta." = "m",
             "m ~ x" = "l",
             "y ~ x" = "-s")

mod_es2 <-
"
.beta.: m
m ~ x: l
y ~ x: -s
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
             "y ~ x" = "-m",
             "m1 ~ x" = "l",
             "m2 ~ x" = "s",
             "y ~ m1" = "m",
             "m1 ~~ m2" = "l")

mod_es2 <-
"
.beta.: s
y ~ x: -m
m1 ~ x: l
m2 ~ x: s
y ~ m1: m
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

mod <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
"

mod_es1 <- c(".beta." = "s",
             "y ~ x" = "-m",
             "m1 ~~ m2" = "l")

mod_es2 <-
"
.beta.: s
y ~ x: -m
m1 ~~ m2: l
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

# Moderation only

mod <-
"
y ~ x + w + x:w + c1
"

mod_es1 <- c(".beta." = "s",
            "y ~ w" = "-m",
            "x ~~ w" = "l")

mod_es2 <-
"
.beta.: s
y ~ w: -m
x ~~ w: l
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)

# Moderated mediation model

mod <-
"
m ~ x + w + x:w
y ~ m + z + m:z + x + u + x:u
"

mod_es1 <- c(".beta." = "s",
             "y ~ x" = "-m",
             "m ~ x:w" = "l",
             "x ~~ w + z + u" = "s",
             "w ~~ z + u" = "l")

mod_es2 <-
"
.beta.: s
y ~ x: -m
m ~ x:w: l
x ~~ w + z + u: s
w ~~ z + u: l
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

mod_es1 <- c("m1 ~ x" = "-m",
             "m2 ~ m1" = "s",
             "y ~ m2" = "l",
             "y ~ x" = "m",
             "y ~ w" = "s",
             "y ~ x:w" = "s",
             "x ~~ w" = "s")

mod_es2 <-
"
m1 ~ x: -m
m2 ~ m1: s
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

# Simple mediation model
# Effect sizes set numerically

mod <-
"
m ~ x
y ~ m + x
"

mod_es1 <- c(".beta." = ".21",
             "m ~ x" = ".31",
             "y ~ x" = "n")

mod_es2 <-
"
.beta.: .21
m ~ x: .31
y ~ x: nil
"

pop_es_yaml(mod_es2)

chk1 <- ptable_pop(mod, mod_es1)
chk2 <- ptable_pop(mod, mod_es2)

expect_equal(chk1$start,
             chk2$start,
             tolerance = 1e-1)
})
