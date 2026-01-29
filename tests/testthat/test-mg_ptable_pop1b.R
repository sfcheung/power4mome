library(testthat)
suppressMessages(library(lavaan))

# Moderated mediation multigroup model.
# Some are latent factors.

test_that("Multigroup models", {

model1 <-
"
m1 ~ x
m2 ~ m1 + x
y ~  m2 + m1 + x + w + m2:w
"
model1_es <- list("m1 ~ x" = "m",
                  "m2 ~ m1" = "s",
                  "y ~ m2:w" = c("-m", "n"))
set.seed(1234)
out <- ptable_pop(model = model1,
                  pop_es = model1_es,
                  n_std = 5e4)
mm_out <- model_matrices_pop(out)
mm_lm_out <- mm_lm(mm_out)

model1_es_ng <- c("m1 ~ x" = "m",
                  "m2 ~ m1" = "s",
                  "y ~ m2:w" = "-m")
out_ng <- ptable_pop(model = model1,
                     pop_es = model1_es_ng)
mm_out_ng <- model_matrices_pop(out_ng)

tmp1 <- out[out$group == 1, ]
tmp2 <- out_ng
tmp1$ptlabel <- lav_partable_labels(tmp1)
tmp2$ptlabel <- lav_partable_labels(tmp2)

expect_equal(tmp1[tmp1$ptlabel == "y~~m2:w", "start"],
             tmp2[tmp2$ptlabel == "y~~m2:w", "start"],
             tolerance = 1e-3)

tmp <- mm_out[[1]]
tmp$nu <- NULL
tmp$alpha <- NULL
expect_identical(tmp,
                 mm_out_ng,
                 tolerance = 1e-3)

data_i <- sim_data_i(model = model1,
                     pop_es = model1_es,
                     n = 200,
                     number_of_indicators = c(m1 = 3, x = 4),
                     reliability = list(x = .60,
                                        m1 = c(.80, .70)),
                     seed = 1234)
fit_i <- fit_model_i(data_i)
summary(fit_i)

data_i2 <- sim_data_i(model = model1,
                      ptable = out,
                      mm_out = mm_out,
                      mm_lm_out = mm_lm_out,
                      n = 200,
                      number_of_indicators = c(m1 = 3, x = 4),
                      reliability = list(x = .60,
                                         m1 = c(.80, .70)),
                      seed = 1234)
fit_i2 <- fit_model_i(data_i)

expect_equal(fitMeasures(fit_i, "chisq"),
             fitMeasures(fit_i2, "chisq"))

})
