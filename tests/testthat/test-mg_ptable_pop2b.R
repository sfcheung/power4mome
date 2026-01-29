library(testthat)
suppressMessages(library(lavaan))

test_that("Multigroup models", {

model2 <-
"
m1 ~ x
m2 ~ x
y ~  m1 + m2 + x
"

model2_es <- list(".beta." = "-s",
                  "m1 + m2 ~ x" = "-m",
                  "y ~ m1" = "l",
                  "y ~ m2" = c("m", "l"))
set.seed(1234)
out <- ptable_pop(model = model2,
                  pop_es = model2_es,
                  n_std = 5e4)
mm_out <- model_matrices_pop(out)
mm_lm_out <- mm_lm(mm_out)

model2_es_ng <- c("y ~ x" = "-s",
                  "m1 + m2 ~ x" = "-m",
                  "y ~ m1" = "l",
                  "y ~ m2" = "m")
out_ng <- ptable_pop(model = model2,
                     pop_es = model2_es_ng)
mm_out_ng <- model_matrices_pop(out_ng)

tmp1 <- out[out$group == 1, ]
tmp2 <- out_ng
tmp1$ptlabel <- lav_partable_labels(tmp1)
tmp2$ptlabel <- lav_partable_labels(tmp2)

expect_equal(tmp1[tmp1$ptlabel == "y~~m2", "start"],
             tmp2[tmp2$ptlabel == "y~~m2", "start"])

expect_equal(tmp1[tmp1$ptlabel == "y~~x", "start"],
             tmp2[tmp2$ptlabel == "y~~x", "start"])

expect_identical(mm_out[[1]],
                 mm_out_ng)

data_i <- sim_data_i(model = model2,
                     pop_es = model2_es,
                     n = 200,
                     number_of_indicators = c(m1 = 3, m2 = 4),
                     reliability = c(m2 = .60, m1 = .70),
                     seed = 1234)

fit_i <- fit_model_i(data_i)

dats <- sim_data(nrep = 9,
                 model = model2,
                 pop_es = model2_es,
                 n = c(300, 100),
                 number_of_indicators = c(m1 = 3, m2 = 4),
                 reliability = c(m2 = .60, m1 = .70),
                 iseed = 1234,
                 progress = !is_testing())
expect_equal(coef(dats[[1]]$fit0)["y~m2"],
             .30,
             ignore_attr = TRUE)
expect_equal(nrow(dats[[1]]$mm_lm_dat_out),
             300 + 100)
expect_true(all(paste0("m1", 1:3) %in%
                colnames(dats[[1]]$mm_lm_dat_out)))
expect_true(all(paste0("m2", 1:4) %in%
                colnames(dats[[1]]$mm_lm_dat_out)))
})
