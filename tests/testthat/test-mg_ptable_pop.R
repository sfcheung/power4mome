library(testthat)

skip("WIP")

model1 <-
"
m1 ~ x + c1
m2 ~ m1 + x2 + c1
y ~  m2 + m1 + x + w + x:w + c1
"
model1_es <- list("m1 ~ x" = "-m",
                  "m2 ~ m1" = "s",
                  "y ~ m2" = "l",
                  "y ~ x" = c("m", "l", "n"),
                  "y ~ w" = "s",
                  "y ~ x:w" = "s",
                  "x ~~ w" = c("s", "m", "l"))

out <- ptable_pop(model = model1,
                  pop_es = model1_es)
mm_out <- model_matrices_pop(out)
mm_lm_out <- mm_lm(mm_out)
