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
expect_lt(
  abs(.100 - fitMeasures(fit, "rmsea")),
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
expect_lt(
  abs(.100 - fitMeasures(fit, "rmsea")),
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
  "x1 ~~ x2" = "l")

out <- beta_nil_from_fit_measures(
  target_fm = "cfi",
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
expect_lt(
  abs(.89 - fitMeasures(fit, "cfi")),
  .01
)

out <- beta_nil_from_fit_measures(
  target_fm = "cfi",
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
expect_lt(
  abs(.89 - fitMeasures(fit, "cfi")),
  .01
)

# ---- Check fit measure target key ----

es <-
c("y1 ~ x1" = .90,
  "y2 ~ x2" = "m",
  "x1 ~~ x2" = "l")

expect_null(
  target_fm_from_pop_es(es)
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: [m, l, nil]
y ~ w: s
y ~ x:w: s
x ~~ w: [s, m, l]
"

expect_null(
  target_fm_from_pop_es(es)
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: [m, l, nil]
y ~ w: s
y ~ x:w: s
x ~~ w: [s, m, l]
.rmsea.: .12
"

expect_equal(
  as.numeric(target_fm_from_pop_es(es)),
  .12
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: m
y ~ w: s
y ~ x:w: s
x ~~ w: s
"

expect_null(
  target_fm_from_pop_es(es)
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: m
y ~ w: s
y ~ x:w: s
x ~~ w: s
.cfi.: .80
"

expect_equal(
  as.numeric(target_fm_from_pop_es(es)),
  .8
)

es <-
"
m1 ~ x: -m
m2 ~ m1: s
y ~ m2: l
y ~ x: m
y ~ w: s
y ~ x:w: s
x ~~ w: s
.cfi.: [.80, .90]
"

pop_es_yaml_check(es)

expect_error(
  target_fm_from_pop_es(es),
  "value"
)

})
