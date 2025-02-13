library(testthat)
library(lavaan)

test_that("Multigroup models", {

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

model1_es_ng <- list("m1 ~ x" = "-m",
                     "m2 ~ m1" = "s",
                     "y ~ m2" = "l",
                     "y ~ x" = c("n"),
                     "y ~ w" = "s",
                     "y ~ x:w" = "s",
                     "x ~~ w" = c("l"))
out_ng <- ptable_pop(model = model1,
                     pop_es = model1_es_ng)
mm_out_ng <- model_matrices_pop(out_ng)

tmp1 <- out[out$group == 3, ]
tmp2 <- out_ng
tmp1$ptlabel <- lav_partable_labels(tmp1)
tmp2$ptlabel <- lav_partable_labels(tmp2)

expect_equal(tmp1[tmp1$ptlabel == "m2~~m2", "start"],
             tmp2[tmp2$ptlabel == "m2~~m2", "start"],
             tolerance = 1e-3)

expect_identical(mm_out[[3]],
                 mm_out_ng,
                 tolerance = 1e-3)

data_i <- sim_data_i(model = model1,
                     pop_es = model1_es,
                     n = 200,
                     seed = 1234)
fit_i <- fit_model_i(data_i)
summary(fit_i)

data_i2 <- sim_data_i(model = model1,
                      ptable = out,
                      mm_out = mm_out,
                      mm_lm_out = mm_lm_out,
                      n = 200,
                      seed = 1234)
fit_i2 <- fit_model_i(data_i)

expect_equal(fitMeasures(fit_i, "chisq"),
             fitMeasures(fit_i2, "chisq"))

})
