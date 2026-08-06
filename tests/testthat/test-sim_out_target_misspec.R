skip_on_cran()

library(testthat)
suppressMessages(library(lavaan))

test_that("target misspec", {

# To be added to sim_data()

  # # ==== Find .beta_nil. ====

  # if (TRUE &&
  #     misspec &&
  #     !is.null(model) &&
  #     !is.null(pop_es)) {

  #   out <- beta_nil_from_fit_measures(
  #     nrep = 1,
  #     model = model,
  #     pop_es = pop_es,
  #     ...,
  #     n = n,
  #     number_of_indicators = NULL,
  #     reliability = NULL,
  #     loading_difference = NULL,
  #     reference = NULL,
  #     x_fun = x_fun,
  #     e_fun = e_fun,
  #     process_data = NULL,
  #     iseed = iseed,
  #     n_ratio = n_ratio,
  #     just_once = TRUE
  #   )

  # }

# ---- Serial Mediation Model ----

mod <-
"m1 ~ x
 m2 ~ m1
 y ~ m2 + x"
es <-
c("y ~ m2" = "m",
  "m1 ~ x" = "m")

out <- beta_nil_from_fit_measures(
  nrep = 1,
  model = mod,
  pop_es = es,
  n = 100,
  progress = !is_testing()
)

out$beta_nil

data_all <- sim_data(
  nrep = 1,
  model = mod,
  pop_es = c(es, out$beta_nil),
  n = 50000,
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
expect_equal(
  round(unname(fitMeasures(fit, "rmsea")), 2),
  .100
)

out <- beta_nil_from_fit_measures(
  nrep = 1,
  model = mod,
  pop_es = es,
  n = 100,
  method = "multi",
  progress = !is_testing()
)

out$beta_nil

data_all <- sim_data(
  nrep = 1,
  model = mod,
  pop_es = c(es, out$beta_nil),
  n = 50000,
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
expect_equal(
  round(unname(fitMeasures(fit, "rmsea")), 2),
  .100
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
  "x1 ~~ x2" = "l")

out <- beta_nil_from_fit_measures(
  target_fm = "cfi",
  nrep = 1,
  model = mod,
  pop_es = es,
  n = 100,
  progress = !is_testing()
)

out$beta_nil

data_all <- sim_data(
  nrep = 1,
  model = mod,
  pop_es = c(es, out$beta_nil),
  n = 50000,
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
expect_equal(
  round(unname(fitMeasures(fit, "cfi")), 2),
  .89,
  tolerance = 1e-1
)

out <- beta_nil_from_fit_measures(
  target_fm = "cfi",
  nrep = 1,
  model = mod,
  pop_es = es,
  n = 100,
  method = "multi",
  progress = !is_testing()
)

out$beta_nil

data_all <- sim_data(
  nrep = 1,
  model = mod,
  pop_es = c(es, out$beta_nil),
  n = 50000,
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
expect_equal(
  round(unname(fitMeasures(fit, "cfi")), 2),
  .89,
  tolerance = 1e-1
)

})
