library(testthat)
suppressMessages(library(lavaan))

test_that("sim_out and do_test", {

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
# mc_all <- gen_mc(fit_all,
#                  R = 100,
#                  iseed = 4567)
sim_all <- sim_out(data_all = data_all,
                   fit = fit_all)

# Check fit_external

tmp <- sim_all[[3]]$fit_external$all_paths
expect_equal(unname(unlist(manymome::all_paths_to_df(tmp))),
             c("x", "y", "m"))
tmp <- fit_all[[3]]@external$fit_external$all_paths
expect_equal(unname(unlist(manymome::all_paths_to_df(tmp))),
             c("x", "y", "m"))

fit_all_lm <- fit_model(data_all, fit_function = "lm")
tmp <- attr(fit_all_lm[[3]], "fit_external")$all_paths
expect_equal(unname(unlist(manymome::all_paths_to_df(tmp))),
             c("x", "y", "m"))

est_test <- function(fit,
                     par_name = NULL,
                     alpha = .05,
                     out_type = c("vector", "list")) {
  out_type <- match.arg(out_type)
  est <- lavaan::parameterEstimates(fit,
                                    ci = TRUE)
  est$lavlabel <- lavaan::lav_partable_labels(est)
  i <- which(est$lavlabel == par_name)
  out <- switch(out_type,
          vector = c(est = est[i, "est"],
                     se = est[i, "se"],
                     cilo = est[i, "ci.lower"],
                     cihi = est[i, "ci.upper"],
                     sig = ifelse(est[i, "pvalue"] < alpha, 1, 0)),
          list = list(est = est[i, "est"],
                      se = est[i, "se"],
                      cilo = est[i, "ci.lower"],
                      cihi = est[i, "ci.upper"],
                      sig = ifelse(est[i, "pvalue"] < alpha, 1, 0)))
  out
}

test_i <- do_test_i(sim_all[[1]],
                    test_fun = est_test,
                    test_args = list(par_name = "y~x"))
expect_equal(test_i$test_results["se"],
             parameterEstimates(fit_all[[1]])[3, "se"],
             ignore_attr = TRUE)

test_all <- do_test(sim_all,
                    test_fun = est_test,
                    test_args = list(par_name = "y~x"),
                    parallel = FALSE,
                    progress = FALSE)

expect_equal(test_all[[3]]$test_results["se"],
             parameterEstimates(fit_all[[3]])[3, "se"],
             ignore_attr = TRUE)

test_all <- do_test(sim_all,
                    test_fun = est_test,
                    test_args = list(par_name = "y~x",
                                     out_type = "list"),
                    parallel = FALSE,
                    progress = FALSE)

expect_equal(test_all[[3]]$test_results$se,
             parameterEstimates(fit_all[[3]])[3, "se"],
             ignore_attr = TRUE)

est_results <- function(est,
                        par_name = NULL,
                        alpha = .05,
                        out_type = c("vector", "list")) {
  out_type <- match.arg(out_type)
  est$lavlabel <- lavaan::lav_partable_labels(est)
  i <- which(est$lavlabel == par_name)
  out <- switch(out_type,
          vector = c(est = est[i, "est"],
                     se = est[i, "se"],
                     cilo = est[i, "ci.lower"],
                     cihi = est[i, "ci.upper"],
                     sig = ifelse(est[i, "pvalue"] < alpha, 1, 0)),
          list = list(est = est[i, "est"],
                      se = est[i, "se"],
                      cilo = est[i, "ci.lower"],
                      cihi = est[i, "ci.upper"],
                      sig = ifelse(est[i, "pvalue"] < alpha, 1, 0)))
  out
}

test_all <- do_test(sim_all,
                    test_fun = lavaan::parameterEstimates,
                    map_names = c(object = "fit"),
                    results_fun = est_results,
                    results_args = list(par_name = "y~x"),
                    parallel = FALSE,
                    progress = FALSE)
expect_equal(test_all[[3]]$test_results["se"],
             parameterEstimates(fit_all[[3]])[3, "se"],
             ignore_attr = TRUE)

test_all <- do_test(sim_all,
                    test_fun = lavaan::parameterEstimates,
                    map_names = c(object = "fit"),
                    results_fun = est_results,
                    results_args = list(par_name = "y~x",
                                        out_type = "list"),
                    parallel = FALSE,
                    progress = FALSE)
expect_equal(test_all[[3]]$test_results$se,
             parameterEstimates(fit_all[[3]])[3, "se"],
             ignore_attr = TRUE)


})
