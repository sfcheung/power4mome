skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("target misspec", {

# ---- Serial Mediation Model ----

mod <-
"m1 ~ x
 m2 ~ m1
 y ~ m2 + x"
es <-
c("y ~ m2" = "m",
  "m1 ~ x" = "m",
  ".rmsea." = .11)

target_fm_from_pop_es(es)

out <- beta_nil_from_fit_measures(
  model = mod,
  pop_es = es,
  iseed = 1234,
  progress = !is_testing()
)

out$beta_nil

data_all <- sim_data(
  nrep = 1,
  model = mod,
  pop_es = c(es, out$beta_nil),
  n = 100000,
  progress = !is_testing(),
  iseed = 1234
)
out0 <- data_all[[1]]
fit <- lavaan::update(
  out0$fit0,
  model = out0$model_final,
  data = out0$mm_lm_dat_out,
  do.fit = TRUE
)
expect_lt(
  abs(.110 - fitMeasures(fit, "rmsea")),
  .01
)

out <- beta_nil_from_fit_measures(
  model = mod,
  pop_es = es,
  iseed = 1234,
  method = "multi",
  progress = !is_testing()
)

out$beta_nil

data_all <- sim_data(
  nrep = 1,
  model = mod,
  pop_es = c(es, out$beta_nil),
  n = 100000,
  progress = !is_testing(),
  iseed = 1234
)
out0 <- data_all[[1]]
fit <- lavaan::update(
  out0$fit0,
  model = out0$model_final,
  data = out0$mm_lm_dat_out,
  do.fit = TRUE
)
expect_lt(
  abs(.110 - fitMeasures(fit, "rmsea")),
  .01
)


# ---- Some paths bounded ----

mod <-
"
y1 ~ x1
y2 ~ x2
"
es <-
c("y1 ~ x1" = .90,
  "y2 ~ x2" = "m",
  "x1 ~~ x2" = "l",
  ".cfi." = .85)

out <- beta_nil_from_fit_measures(
  model = mod,
  pop_es = es,
  iseed = 1234,
  progress = !is_testing()
)

out$beta_nil

data_all <- sim_data(
  nrep = 1,
  model = mod,
  pop_es = c(es, out$beta_nil),
  n = 100000,
  progress = !is_testing(),
  iseed = 1234
)
out0 <- data_all[[1]]
fit <- lavaan::update(
  out0$fit0,
  model = out0$model_final,
  data = out0$mm_lm_dat_out,
  do.fit = TRUE
)
expect_lt(
  abs(.85 - fitMeasures(fit, "cfi")),
  .01
)
out <- beta_nil_from_fit_measures(
  model = mod,
  pop_es = es,
  iseed = 1234,
  method = "multi",
  progress = !is_testing()
)

out$beta_nil

data_all <- sim_data(
  nrep = 1,
  model = mod,
  pop_es = c(es, out$beta_nil),
  n = 10000,
  progress = !is_testing(),
  iseed = 1234
)
out0 <- data_all[[1]]
fit <- lavaan::update(
  out0$fit0,
  model = out0$model_final,
  data = out0$mm_lm_dat_out,
  do.fit = TRUE
)
expect_lt(
  abs(.85 - fitMeasures(fit, "cfi")),
  .01
)

})
