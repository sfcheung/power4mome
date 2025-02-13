library(testthat)
library(lavaan)

test_that("Multigroup models", {

model2 <-
"
m ~ x
y ~  m + x
"

model2_es <- list("m ~ x" = "-m",
                  "y ~ m" = "l",
                  "y ~ x" = c("m", "l", "n"))


out <- ptable_pop(model = model2,
                  pop_es = model2_es)
mm_out <- model_matrices_pop(out)
mm_lm_out <- mm_lm(mm_out)

model2_es_ng <- list("m ~ x" = "-m",
                     "y ~ m" = "l",
                     "y ~ x" = c("n"))
out_ng <- ptable_pop(model = model2,
                     pop_es = model2_es_ng)
mm_out_ng <- model_matrices_pop(out_ng)

tmp1 <- out[out$group == 3, ]
tmp2 <- out_ng
tmp1$ptlabel <- lav_partable_labels(tmp1)
tmp2$ptlabel <- lav_partable_labels(tmp2)

expect_equal(tmp1[tmp1$ptlabel == "m~~m", "start"],
             tmp2[tmp2$ptlabel == "m~~m", "start"])

expect_identical(mm_out[[3]],
                 mm_out_ng)

data_i <- sim_data_i(model = model2,
                     pop_es = model2_es,
                     n = 200,
                     number_of_indicators = c(x = 3, y = 4, m = 5),
                     reliability = c(x = .60, y = .70, m = .80),
                     seed = 1234)

fit_i <- fit_model_i(data_i)

dats <- sim_data(nrep = 9,
                 model = model2,
                 pop_es = model2_es,
                 n = c(100, 200, 300),
                 number_of_indicators = c(x = 3, y = 4, m = 5),
                 reliability = c(x = .60, y = .70, m = .80),
                 iseed = 1234)
expect_equal(coef(dats[[1]]$fit0)["m~x"],
             -.30,
             ignore_attr = TRUE)
expect_equal(nrow(dats[[1]]$mm_lm_dat_out),
             100 + 200 + 300)
expect_true(all(paste0("x", 1:3) %in%
                colnames(dats[[1]]$mm_lm_dat_out)))
expect_true(all(paste0("y", 1:4) %in%
                colnames(dats[[1]]$mm_lm_dat_out)))
expect_true(all(paste0("m", 1:5) %in%
                colnames(dats[[1]]$mm_lm_dat_out)))

})
