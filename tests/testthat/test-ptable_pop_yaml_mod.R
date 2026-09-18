library(testthat)
suppressMessages(library(lavaan))

test_that("ptable_pop: moderation", {

skip("WIP")

mod <-
"
m ~ x + w1 + w1:x
y ~ m + x + w2 + m:w2
"

expand_mod_components <- function(
  x
) {

}

expand_mod_components(".mod.(m~x|-w1)")
expand_mod_components(".mod.(m~x|+w1)")
expand_mod_components(".mod.(m~w1|-x)")

# m~x|-w1: .1
a1 <- .4
dat1 <- data.frame(
  x = c(-1, 1),
  y = c(-a1, a1),
  w = c(-1, -1)
)
mean(dat1$y * c(-1, 1))

# m~x|+w1: .5
a2 <- -.5
dat2 <- data.frame(
  x = c(-1, 1),
  y = c(-a2, a2),
  w = c(1, 1)
)
mean(dat2$y * c(-1, 1))

# m~w1|-x: .3
a3 <- .1
dat3 <- data.frame(
  x = c(-1, -1),
  y = c(-a3, a3),
  w = c(-1, 1)
)
mean(dat3$y * c(-1, 1))

dat1
dat2
dat3

merge(dat1, dat2)

y0 <- merge(dat3, dat1, by = c("x", "w"), suffixes = c(".1", ".2"))
dat1$y <- scale(dat1$y, center = (y0$y.2 - y0$y.1), scale = FALSE)
y0 <- merge(dat3, dat2, by = c("x", "w"), suffixes = c(".1", ".2"))
dat2$y <- scale(dat2$y, center = (y0$y.2 - y0$y.1), scale = FALSE)

dat0 <- rbind(
  dat1,
  dat2
)
dat0
out0 <- lm(y ~ x*w, dat0)
coef(out0)
summary(out0)$r.squared
plot(
  y ~ x,
  dat0[dat0$w == -1, ],
  type = "l",
  ylim = range(dat0$y)
)
points(
  y ~ x,
  dat0[dat0$w ==  1, ],
  type = "l",
  ylim = range(dat0$y)
)

# Two moderator

# m~x|-w10w2
a1 <- .1
dat1 <- data.frame(
  y = c(-a1, a1),
  x = c(-1, 1),
  w1 = c(-1, -1),
  w2 = c(0, 0)
)

# m~x|+w10w2
a2 <- .5
dat2 <- data.frame(
  y = c(-a2, a2),
  x = c(-1, 1),
  w1 = c(+1, +1),
  w2 = c(0, 0)
)

# m~x|0w1-w2
a3 <- .3
dat3 <- data.frame(
  y = c(-a3, a3),
  x = c(-1, 1),
  w1 = c(0, 0),
  w2 = c(-1, -1)
)

# m~x|0w1+w2
a4 <- .8
dat4 <- data.frame(
  y = c(-a4, a4),
  x = c(-1, 1),
  w1 = c(0, 0),
  w2 = c(+1, +1)
)

# m~w1|-x0w2
a5 <- .2
dat5 <- data.frame(
  y = c(-a5, a5),
  x = c(-1, -1),
  w1 = c(-1, +1),
  w2 = c(0, 0)
)

# m~w2|-x0w1
a6 <- .3
dat6 <- data.frame(
  y = c(-a6, a6),
  x = c(-1, -1),
  w1 = c(0, 0),
  w2 = c(-1, +1)
)



merge(dat1, dat2, by = c("x", "w1", "w2"))
merge(dat1, dat3, by = c("x", "w1", "w2"))
merge(dat1, dat4, by = c("x", "w1", "w2"))
merge(dat2, dat3, by = c("x", "w1", "w2"))
merge(dat2, dat4, by = c("x", "w1", "w2"))
merge(dat3, dat4, by = c("x", "w1", "w2"))

merge(dat1, dat5, by = c("x", "w1", "w2"))
merge(dat2, dat5, by = c("x", "w1", "w2"))
merge(dat3, dat5, by = c("x", "w1", "w2"))
merge(dat4, dat5, by = c("x", "w1", "w2"))

merge(dat1, dat6, by = c("x", "w1", "w2"))
merge(dat2, dat6, by = c("x", "w1", "w2"))
merge(dat3, dat6, by = c("x", "w1", "w2"))
merge(dat4, dat6, by = c("x", "w1", "w2"))


y0 <- merge(dat5, dat1, by = c("x", "w1", "w2"), suffixes = c(".1", ".2"))
dat1$y <- scale(dat1$y, center = (y0$y.2 - y0$y.1), scale = FALSE)
y0 <- merge(dat5, dat2, by = c("x", "w1", "w2"), suffixes = c(".1", ".2"))
dat2$y <- scale(dat2$y, center = (y0$y.2 - y0$y.1), scale = FALSE)

y0 <- merge(dat6, dat3, by = c("x", "w1", "w2"), suffixes = c(".1", ".2"))
dat3$y <- scale(dat1$y, center = (y0$y.2 - y0$y.1), scale = FALSE)
y0 <- merge(dat6, dat4, by = c("x", "w1", "w2"), suffixes = c(".1", ".2"))
dat4$y <- scale(dat4$y, center = (y0$y.2 - y0$y.1), scale = FALSE)

dat0 <- rbind(
  dat1,
  dat2,
  dat4
)
dat0
out0 <- lm(y ~ x*w1 + x*w2, dat0)
coef(out0)
summary(out0)$r.squared

dat0 <- rbind(
  dat1,
  dat2,
  dat3
)
dat0
out0 <- lm(y ~ x*w1 + x*w2, dat0)
coef(out0)
summary(out0)$r.squared

dat0 <- rbind(
  dat1,
  dat2,
  dat3,
  dat4
)
dat0
out0 <- lm(y ~ x*w1*w2, dat0)
coef(out0)
summary(out0)$r.squared

expand_mod_components(".mod.(y~m|-w2)")
expand_mod_components(".mod.(y~m|+w2)")
expand_mod_components(".mod.(y~w2|+m)")

expand_mod_components(".mod.(y~m|-w1-w2)")
expand_mod_components(".mod.(y~m|-w1+w2)")
expand_mod_components(".mod.(y~m|+w1-w2)")
expand_mod_components(".mod.(y~m|+w1+w2)")
expand_mod_components(".mod.(y~w1|+m)")


# m ~ x + w1 + x:w1

mod_es1 <- c(
  ".beta." = "m",
  ".mod.(m~x|w1~low)" = "s",
  ".mod.(m~x|w1~high)" = "m",
  ".mod.(m~w1|x~low)" = "-l",
  ".mod.(y~m|w2~low)" = "-s",
  ".mod.(y~m|w2~high)" = "-m",
  ".mod.(y~w2|m~high)" = "nil"
)

mod_es2 <-
"
.beta.: m
.mod.(m~x|w1~low): s
.mod.(m~x|w1~high): m
.mod.(m~w1|x~low): -l
.mod.(y~m|w2~low): -s
.mod.(y~m|w2~high): -m
.mod.(y~w2|m~high): nil
"

pop_es_yaml(mod_es2)

chk01 <- fix_par_es(mod_es1, model = mod)

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
