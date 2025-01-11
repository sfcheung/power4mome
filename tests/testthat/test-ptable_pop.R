
test_that("ptable_pop", {
model <-
"
m1 ~ .-m*x + a*c1
m2 ~ .s*m1 + x2 + a*c1
y ~ .l*m2 + m1 + .m*x + .s*w + .s*x:w + c1
x ~~ .s*w
"

model2 <-
"
m1 ~ x + c1
m2 ~ m1 + x2 + c1
y ~  m2 + m1 + x + w + x:w + c1
"

model2_es <- c("m1 ~ x" = "-m",
               "m2 ~ m1" = "s",
               "y ~ m2" = "l",
               "y ~ x" = "m",
               "y ~ w" = "s",
               "y ~ x:w" = "s",
               "x ~~ w" = "s")

ptable2_es <- set_pop(model2_es)[, -5]
ptable2_es

ptable_final1 <- ptable_pop(model2,
                            pop_es = ptable2_es)
ptable_final2 <- ptable_pop(model2,
                            pop_es = model2_es)
expect_identical(ptable_final1,
                 ptable_final2)

})
