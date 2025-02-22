library(testthat)
suppressMessages(library(lavaan))

test_that("sim_out and do_test: k models", {

mod <-
"m ~ x
 y ~ m + x"
es <-
c("y ~ m" = "m",
  "m ~ x" = "m",
  "y ~ x" = "n")
data_all <- sim_data(nrep = 3,
                     model = mod,
                     pop_es = es,
                     n = 100,
                     iseed = 1234)
fit_all <- fit_model(data_all)
fit_all2 <- fit_model(data_all,
                      model = c("m ~ x", "y ~ m"))
fit_all3 <- fit_model(data_all,
                      model = c("m ~ x", "y ~ 0*m"))
# mc_all <- gen_mc(fit_all,
#                  R = 100,
#                  iseed = 4567)

sim_all <- sim_out(data_all = data_all,
                   fit = fit_all,
                   fit2 = fit_all2,
                   fit3 = fit_all3)

lrt_test <- function(fit,
                     fit2) {
  lrt_out <- lavaan::lavTestLRT(fit,
                                fit2)
  lrt_out
}

lrt_results <- function(lrt_out,
                        alpha = .05) {
  pvalue <- lrt_out[2, "Pr(>Chisq)"]
  out <- c(est = lrt_out[2, "Chisq diff"],
           pvalue = pvalue,
           sig = ifelse(pvalue < alpha, 1, 0))
  out
}

test_all <- do_test(sim_all,
                    test_fun = lrt_test,
                    map_names = c(fit = "fit",
                                  fit2 = "fit2"),
                    results_fun = lrt_results,
                    parallel = FALSE,
                    progress = FALSE)

chk <- lavTestLRT(fit_all[[2]],
                  fit_all2[[2]])
expect_equal(unname(test_all[[2]]$test_results["pvalue"]),
             chk[2, "Pr(>Chisq)"],
             ignore_attr = FALSE)

# Check update models

sim_all1 <- sim_out(data_all = data_all,
                    fit = fit_all)
sim_all1 <- sim_out(data_all = sim_all1,
                    fit2 = fit_all2)
sim_all1 <- sim_out(data_all = sim_all1,
                    fit3 = fit_all3)

expect_true(identical(sim_all,
                      sim_all1))

# Check NULL

sim_all2 <- sim_out(data_all = data_all)

expect_true(is.null(sim_all2[[1]]$extra))

})
