library(testthat)

test_that("Test moderation: lm", {

mod <-
"
m ~ x + w1 + x:w1 + w3 + x:w3
y ~ m + w2 + m:w2 + x + w4 + m:w4
"

mod_es <- c("m ~ x" = "n",
            "y ~ x" = "m",
            "m ~ w1" = "n",
            "m ~ x:w1" = "l",
            "y ~ m:w2" = "-s")

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       iseed = 1234)

test_out <- power4test(object = sim_only,
                       test_fun = test_moderation,
                       test_args = list(p_adjust_method = "BH"))

p_org <- test_out$test_all[[1]][[1]]$test_results$pvalue_org
p_adj <- test_out$test_all[[1]][[1]]$test_results$pvalue

expect_equal(
  p.adjust(p_org, method = "BH"),
  p_adj
)

})
