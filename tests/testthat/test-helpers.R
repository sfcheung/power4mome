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
})
