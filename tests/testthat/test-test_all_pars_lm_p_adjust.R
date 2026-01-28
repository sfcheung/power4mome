library(testthat)

test_that("All parameters: lm: p-adjust", {

model_simple_med <-
"
m1 ~ x
m2 ~ x
m3 ~ x
y ~ m1 + m2 + m3 + x
"

model_simple_med_es <- c("y ~ m1" = "l",
                         "m1 ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(p_adjust_method = "BH"))

p_org <- test_out$test_all[[1]][[2]]$test_results$pvalue_org
p_adj <- test_out$test_all[[1]][[2]]$test_results$pvalue

expect_equal(
  p.adjust(p_org, method = "BH"),
  p_adj
)

# Only some parameters are selected

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(p_adjust_method = "BH",
                                        par = c("m1~x", "m2~x", "y~m1", "y~x")))

p_org <- test_out$test_all[[1]][[2]]$test_results$pvalue_org
p_adj <- test_out$test_all[[1]][[2]]$test_results$pvalue

expect_equal(
  p.adjust(p_org, method = "BH"),
  p_adj
)

})
