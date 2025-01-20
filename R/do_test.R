#' @title Do a Test on Each Replication
#'
#' @description Do a test on each
#' replication in the output of
#' [sim_out()].
#'
#' @details
#' Do an arbitrary test in each
#' replication using the function set to
#' `test_fun`. This function should work
#' on the output of [lavaan::sem()]. if
#' the function can use the output of
#' [manymome::do_mc()], set
#' `mc_out_name` to the name of the
#' argument (`mc_out` for
#' [manymome::indirect_effect()]).
#'
#' The test results will be extracted
#' from the output of `test_fun` by the
#' function set to `results_fun`.
#'
#' The function set to `results_fun`
#' must accept the output of `test_fun`,
#' as the first argument, and return a
#' named vector of four elements:
#'
#' - `est`: The estimate of a parameter.
#'  `NA` if the test does not return an
#'  estimate.
#'
#' - `cilo`: The lower limit of the
#'  confidence interval. `NA` if the
#'  test does not return a confidence
#'  interval.
#'
#' - `cihi`: The upper limit of the
#'  confidence interval. `NA` if the
#'  test does not return a confidence
#'  interval.
#'
#' - `sig`: If `1`, the test is
#'  significant. If `0`, the test is not
#'  significant. If the test cannot be
#'  done for any reason, it should be
#'  `NA`.
#'
#' The results can then be used to
#' estimate the power of the test.
#'
#' @return
#' An object of the class `test_out`,
#' which is a list of length equal to
#' `sim_out`. Each element of the list
#' has two elements:
#'
#' - `test`: The output of `test_fun`.
#'
#' - `test_results`: The output of
#'  `results_fun`.
#'
#' @param sim_all The output of
#' [sim_out()].
#'
#' @param test_fun A function to do the
#' test. See 'Details' for the requirement
#' of this function.
#'
#' @param test_args A list of arguments
#' to be passed to the `test_fun`
#' function. Default is `list()`.
#'
#' @param fit_name The name of the
#' argument of the `test_fun` function
#' that accepts the output of [lavaan::sem()].
#'
#' @param mc_out_name The name of the
#' argument of the `test_fun` function
#' that accepts the output of [manymome::do_mc()].
#' If the `test_fun` function does not
#' use the Monte Carlo estimates, set
#' this to `NULL`.
#'
#' @param results_fun The function to be
#' used to extract the test results.
#' See `Details` for the requirements
#' of this function.
#'
#' @param results_args A list of
#' arguments to be passed to the
#' `results_fun` function. Default is
#' `list()`.
#'
#' @param parallel If `TRUE`, parallel
#' processing will be used to do the
#' tests. Default is `FALSE`.
#'
#' @param progress If `TRUE`, the progress
#' of tests will be displayed.
#' Default is `FALSE.
#'
#' @param ncores The number of CPU
#' cores to use if parallel processing
#' is used.
#'
#' @examples
#' mod <-
#' "m ~ x
#'  y ~ m + x"
#' es <-
#' c("y ~ m" = "m",
#'   "m ~ x" = "m",
#'   "y ~ x" = "n")
#' data_all <- sim_data(nrep = 5,
#'                  model = mod,
#'                  pop_es = es,
#'                  n = 100,
#'                  iseed = 1234)
#'
#' fit_all <- fit_model(data_all)
#' mc_all <- gen_mc(fit_all,
#'                  R = 100,
#'                  iseed = 4567)
#' sim_all <- sim_out(data_all = data_all,
#'                    fit_all = fit_all,
#'                    mc_all = mc_all)
#'
#' ind_results <- function(out) {
#'   ci0 <- stats::confint(out)
#'   out1 <- ifelse((ci0[1, 1] > 0) || (ci0[1, 2] < 0),
#'                   yes = 1,
#'                   no = 0)
#'   out2 <- c(est = unname(coef(out)),
#'             cilo = ci0[1, 1],
#'             cihi = ci0[1, 2],
#'             sig = out1)
#'   return(out2)
#' }
#'
#' test_all <- do_test(sim_all,
#'                     test_fun = manymome::indirect_effect,
#'                     test_args = list(x = "x",
#'                                      m = "m",
#'                                      y = "y",
#'                                      mc_ci = TRUE),
#'                     fit_name = "fit",
#'                     mc_out_name = "mc_out",
#'                     results_fun = ind_results,
#'                     parallel = FALSE,
#'                     progress = FALSE)
#'
#' @export
#'
do_test <- function(sim_all,
                    test_fun,
                    test_args = list(),
                    fit_name = "fit",
                    mc_out_name = "mc_out",
                    results_fun = NULL,
                    results_args = list(),
                    parallel = FALSE,
                    progress = FALSE,
                    ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = sim_all,
                FUN = do_test_i,
                test_fun = test_fun,
                test_args = test_args,
                fit_name = fit_name,
                mc_out_name = mc_out_name,
                results_fun = results_fun,
                results_args = results_args,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  class(out) <- c("test_out", class(out))
  return(out)
}

#' @title Title In Title Case
#'
#' @description One paragraph description.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @examples
#' \donttest{
#' }
#'
#' @noRd
#'
do_test_i <- function(out_i,
                      test_fun,
                      test_args = list(0),
                      fit_name = "fit",
                      mc_out_name = "mc_out",
                      results_fun = NULL,
                      results_args = list()) {
# FUN must be a function that:
# - Extracts the outcome of sim_out(), e.g.,
#   - fit
#   - mc_all (optional)
# FUN_get_results: Extract the following from
# the output of FUN
# - A list with these elements:
#   - est: Scalar
#   - cilo: Scalar
#   - cihi: Scalar
#   - sig: Logical
  if (is.null(mc_out_name)) {
    args <- list(x = out_i$fit)
    names(args) <- c(fit_name)
  } else {
    args <- list(x = out_i$fit,
                 y = out_i$mc_out)
    names(args) <- c(fit_name, mc_out_name)
  }
  args1 <- utils::modifyList(test_args,
                             args)
  out0 <- do.call(test_fun,
                  args1)
  out1 <- do.call(results_fun,
                  c(list(out0), results_args))
  return(list(test = out0,
              test_results = out1))
}
