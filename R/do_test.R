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
#' on the output of [lavaan::sem()].
#'
#' The test results will be extracted
#' from the output of `test_fun` by the
#' function set to `results_fun`. If
#' the `test_fun` already returns an
#' output of the expected format
#' (see below), then set `results_fun`
#' to `NULL`, the default. The output
#' of `test_fun` will be used for
#' estimating power.
#'
#' The function set to `results_fun`
#' must accept the output of `test_fun`,
#' as the first argument, and return a
#' named list or named vector with some
#' of the following
#' elements:
#'
#' - `est`: Optional. The estimate of a
#'  parameter.
#'
#' - `se`: Optional. The standard error
#'  of the estimate.
#'
#' - `cilo`: Optional. The lower limit of the
#'  confidence interval.
#'
#' - `cihi`: Optional. The upper limit of the
#'  confidence interval.
#'
#' - `sig`: Required. If `1`, the test is
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
#' @param map_names A named character
#' vector specifying how the content of
#' the element `extra` in
#' each replication of `sim_all` map
#' to the argument of `test_fun`.
#' Default is `c(fit = "fit")`,
#' indicating that the element `fit`
#' in the element `extra` is set to
#' the argument `fit` of `test_fun`.
#' That is, for the first replication,
#' `fit = sim_out[[1]]$extra$fit` when
#' calling `test_fun`.
#'
#' @param results_fun The function to be
#' used to extract the test results.
#' See `Details` for the requirements
#' of this function. Default is `NULL`,
#' assuming that the output of
#' `test_fun` can be used directly.
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
#'                    fit = fit_all,
#'                    mc_out = mc_all)
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
#'                     map_name = c(fit = "fit",
#'                                  mc_out = "mc_out"),
#'                     results_fun = ind_results,
#'                     parallel = FALSE,
#'                     progress = FALSE)
#'
#' @export
#'
do_test <- function(sim_all,
                    test_fun,
                    test_args = list(),
                    map_names = c(fit = "fit"),
                    results_fun = NULL,
                    results_args = list(),
                    parallel = FALSE,
                    progress = FALSE,
                    ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = sim_all,
                FUN = do_test_i,
                test_fun = test_fun,
                test_args = test_args,
                map_names = map_names,
                results_fun = results_fun,
                results_args = results_args,
                parallel = parallel,
                progress = progress,
                ncores = ncores)

  attr(out, "test_fun") <- test_fun
  attr(out, "test_args") <- test_args
  attr(out, "map_names") <- map_names
  attr(out, "results_fun") <- results_fun
  attr(out, "results_args") <- results_args

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
                      map_names = c(fit = "fit"),
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
  args_map <- sapply(map_names,
                     function(x,
                              out_i) {
                       out_i$extra[[x]]
                     },
                     out_i = out_i,
                     simplify = FALSE,
                     USE.NAMES = FALSE)
  args1 <- c(args_map,
             test_args)
  out0 <- do.call(test_fun,
                  args1)
  if (is.function(results_fun)) {
    out1 <- do.call(results_fun,
                    c(list(out0), results_args))
  } else {
    out1 <- out0
  }

  return(list(test = out0,
              test_results = out1))
}
