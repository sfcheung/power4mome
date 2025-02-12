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


model2 <-
"
m ~ x
y ~  m + x
"

model2_es <- list("m ~ x" = "-m",
                  "y ~ m" = "l",
                  "y ~ x" = c("m", "l", "n"))


data_i <- sim_data_i(model = model2,
                     pop_es = model2_es,
                     n = 200,
                     number_of_indicators = c(x = 3, y = 4, m = 5),
                     reliability = c(x = .60, y = .70, m = .80),
                     seed = 1234)
