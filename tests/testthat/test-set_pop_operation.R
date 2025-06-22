# Test

test_that("set_pop", {
model_es1 <- c("m1 ~ x" = "-m",
               "m2 ~ m1" = "s",
               "y ~ m2" = "l",
               "y ~ x" = "l",
               "y ~ w" = "s",
               "y ~ x:w" = "s",
               "x ~~ w" = "s")

model_es2 <- c("m1 ~ x" = "-m",
               "m2 ~ m1" = "s",
               "y ~ m2 + x" = "l",
               "y ~ w" = "s",
               "y ~ x:w" = "s",
               "w ~~ x" = "s")

ptable_es1 <- set_pop(model_es1)
ptable_es2 <- set_pop(fix_par_es(model_es2))
expect_identical(ptable_es1,
                 ptable_es2)

es1_temp <- c("s" = .10,
              "m" = .30,
              "l" = .50)
es1_tmpn <- es1_temp * -1
names(es1_tmpn) <- paste0("-", names(es1_tmpn))
es1_all <- c(es1_temp, es1_tmpn)
es2_temp <- c("s" = .05,
              "m" = .10,
              "l" = .15)
es2_tmpn <- es2_temp * -1
names(es2_tmpn) <- paste0("-", names(es2_tmpn))
es2_all <- c(es2_temp, es2_tmpn)
expect_equal(ptable_es1$pop[6],
             es2_all[ptable_es1$es][6],
             ignore_attr = TRUE)

pop_es <- c("m ~ x" = "log(.20)",
            "y ~ m" = ".20",
            "y ~ x" = "s * m")
tmp <- set_pop(pop_es)
expect_equal(tmp$pop,
             c(log(.20), .20, .10 * .30),
             ignore_attr = TRUE)

pop_es <- c("m ~ x" = "sqrt(.40)",
            "y ~ m:x" = "-s",
            "y ~ x" = "s + .10")
tmp <- set_pop(pop_es)
expect_equal(tmp$pop,
             c(sqrt(.40), -.05, .10 + .10),
             ignore_attr = TRUE)

mod <-
"
m ~ x
y ~ m + x
"

mod_es1 <- c(".beta." = "m",
             ".ind.(x->m->y)" = "li")

pop_es <- fix_par_es(mod_es1,
                      model = mod)

tmp <- set_pop(pop_es)
expect_equal(tmp$pop,
             c(sqrt(.510), sqrt(.510), .30),
             ignore_attr = TRUE)

mod <-
"
m ~ x
y ~ m + x
"

mod_es1 <- c(".beta." = "m",
             ".ind.(x->m->y)" = ".10^2")

pop_es <- fix_par_es(mod_es1,
                      model = mod)

tmp <- set_pop(pop_es)
expect_equal(tmp$pop,
             c(.10, .10, .30),
             ignore_attr = TRUE)

})
