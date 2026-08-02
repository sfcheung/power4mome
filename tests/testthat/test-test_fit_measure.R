library(testthat)
suppressMessages(library(lavaan))

test_that("test_fit_measure", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

# model_simple_med_es <-
# "
# y ~ m: l
# m ~ x:
#   - nil
#   - s
# y ~ x: nil
# "

model_simple_med_es <-
"
y ~ m: l
m ~ x: s
y ~ x: m
"

model_simple_med_0 <-
"
m ~ x
y ~ m
"

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       fit_model_args = list(
                        estimator = "MLR",
                        model_append = "y ~ 0*x"
                       ),
                       n = 100,
                       progress = !is_testing(),
                       iseed = 1234)

# Default

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    fit_measure = "chisq.scaled"
  )
)

tmp1 <- rejection_rates(test_out1)

test_out2 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    model_to_fit = model_simple_med_0,
    fit_measure = "chisq.scaled"
  )
)

tmp2 <- rejection_rates(test_out2)

expect_equal(
  tmp1$est,
  tmp2$est,
  tolerance = 1e-4
)

fit_chk <- lapply(
  sim_only$sim_all,
  \(x) x$extra$fit
)
fm_chk <- sapply(
  fit_chk,
  fitMeasures,
  "chisq.scaled"
)

expect_equal(
  tmp1$est,
  mean(fm_chk),
  tolerance = 1e-4
)

sig_chk <- sapply(
  fit_chk,
  fitMeasures,
  "pvalue.scaled"
)

expect_equal(
  tmp1$reject,
  mean(sig_chk < .05),
  tolerance = 1e-4
)

# RMSEA

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    fit_measure = "rmsea"
  )
)

tmp1 <- rejection_rates(test_out1)

test_out2 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    model_to_fit = model_simple_med_0,
    fit_measure = "rmsea"
  )
)

tmp2 <- rejection_rates(test_out2)

expect_equal(
  tmp1$est,
  tmp2$est,
  tolerance = 1e-4
)

fit_chk <- lapply(
  sim_only$sim_all,
  \(x) x$extra$fit
)
fm_chk <- sapply(
  fit_chk,
  fitMeasures,
  "rmsea"
)

expect_equal(
  tmp1$est,
  mean(fm_chk),
  tolerance = 1e-4
)

sig_chk <- sapply(
  fit_chk,
  fitMeasures,
  "rmsea.pvalue"
)

expect_equal(
  tmp1$reject,
  mean(sig_chk < .05),
  tolerance = 1e-4
)

# CFI

test_out1 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    fit_measure = "cfi",
    sig_if = "<.95"
  )
)

tmp1 <- rejection_rates(test_out1)

test_out2 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    model_to_fit = model_simple_med_0,
    fit_measure = "cfi",
    sig_if = "<.95"
  )
)

tmp2 <- rejection_rates(test_out2)

expect_equal(
  tmp1$est,
  tmp2$est,
  tolerance = 1e-4
)

fit_chk <- lapply(
  sim_only$sim_all,
  \(x) x$extra$fit
)
fm_chk <- sapply(
  fit_chk,
  fitMeasures,
  "cfi"
)

expect_equal(
  tmp1$est,
  mean(fm_chk),
  tolerance = 1e-4
)

expect_equal(
  tmp1$reject,
  mean(fm_chk < .95),
  tolerance = 1e-4
)

# Refit with new option

test_out2 <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    always_refit = TRUE,
    refit_args = list(
      se = "none",
      estimator = "ML"
    )
  )
)

tmp2 <- rejection_rates(test_out2)

sim_only_ml <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       fit_model_args = list(
                        estimator = "ML",
                        model_append = "y ~ 0*x"
                       ),
                       n = 100,
                       progress = !is_testing(),
                       iseed = 1234)

fit_chk <- lapply(
  sim_only_ml$sim_all,
  \(x) x$extra$fit
)
fm_chk <- sapply(
  fit_chk,
  fitMeasures,
  "chisq"
)

expect_equal(
  tmp2$est,
  mean(fm_chk),
  tolerance = 1e-4
)

tmp_chk <- sapply(
  fit_chk,
  \(x) length(fitMeasures(x, "chisq.scaled"))
)

expect_all_equal(tmp_chk, 0)

})
