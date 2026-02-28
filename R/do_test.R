#' @title Do a Test on Each Replication
#'
#' @description Do a test on each
#' replication in the output of
#' [sim_out()].
#'
#' @details
#' The function [do_test()] does
#' an arbitrary test in each
#' replication using the function set to
#' `test_fun`.
#'
#' # The role of `do_test()`
#'
#' The function [do_test()] is used by
#' the all-in-one function
#' [power4test()]. Users usually do not
#' call this function directly, though
#' developers can use this function to
#' develop other functions for power
#' analysis, or to build their own
#' workflows to do the power analysis.
#'
#' # Major Test-Related Arguments
#'
#' ## The test function (test_fun)
#'
#' The function set to `test_fun`,
#' the *test function*, usually
#' should work
#' on the output of [lavaan::sem()],
#' [lmhelprs::many_lm()], or
#' [stats::lm()], but can also be a
#' function that works on the output
#' of the function set to `fit_function`
#' when calling [fit_model()] or
#' [power4test()] (see `fit_model_args`).
#'
#' The function has two default
#' requirements.
#'
#' First, it has an argument `fit`, to
#' be set to the output of
#' [lavaan::sem()] or another output
#' stored in the element `extra$fit` of
#' a replication in the `sim_all`
#' object. (This requirement can be
#' relaxed; see the section on `map_names`.)
#'
#' That is, the function definition
#' should be of this from: `FUN(fit,
#' ...)`. This is the form of all
#' `test_*` functions in `power4mome`.
#'
#' If other arguments are to be passed
#' to the test function, they can be set
#' to `test_args` as a named list.
#'
#' Second, the test function must
#' returns an output that (a) can be
#' processed by the results function
#' (see below), or (b) is of
#' the required format for the output of
#' a results function (see the next
#' section). If it already returns
#' an output of the required format,
#' then there is no need to set the
#' results function.
#'
#' ## The results function (results_fun)
#'
#' The test results will be extracted
#' from the output of `test_fun` by the
#' function set to `results_fun`,
#' the *results function*. If
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
#' named list (which can be a data frame)
#' or a named vector with some
#' of the following
#' elements:
#'
#' - `est`: Optional. The estimate of a
#'  parameter, if applicable.
#'
#' - `se`: Optional. The standard error
#'  of the estimate, if applicable.
#'
#' - `cilo`: Optional. The lower limit of the
#'  confidence interval, if applicable.
#'
#' - `cihi`: Optional. The upper limit of the
#'  confidence interval, if applicable.
#'
#' - `sig`: Required. If `1`, the test is
#'  significant. If `0`, the test is not
#'  significant. If the test cannot be
#'  done for any reason, it should be
#'  `NA`.
#'
#' The results can then be used to
#' estimate the power or Type I
#' error of the test.
#'
#' For example, if
#' the null hypothesis is false, then
#' the proportion of significant, that
#' is, the mean of the values of `sig`
#' across replications, is the power.
#'
#' ## Built-in test functions
#'
#' The package `power4mome` has some ready-to-use
#' test functions:
#'
#' - [test_indirect_effect()]
#'
#' - [test_cond_indirect()]
#'
#' - [test_cond_indirect_effects()]
#'
#' - [test_moderation()]
#'
#' - [test_index_of_mome()]
#'
#' - [test_parameters()]
#'
#' Please refer to their help pages
#' for examples.
#'
#' ## The argument map_names
#'
#' This argument is for developers using
#' a test function that has a different
#' name for the argument of the fit
#' object (`"fit"`, the default).
#'
#' If `test_fun` is set to a function
#' that works on an output of, say,
#' [lavaan::sem()] but the argument name
#' for the output is not `fit`, the
#' mapping can be changed by
#' `map_names`.
#'
#' For example,
#' [lavaan::parameterEstimates()]
#' receives an output of [lavaan::sem()]
#' and reports the test results of model
#' parameters. However, the argument
#' name for the `lavaan` output is
#' `object.` To instruct [do_test()] to
#' do the test correctly when setting
#' `test_fun` to
#' `lavaan::parameterEstimates`, add
#' `map_names = c(object = "fit")`. The
#' element `fit` in a replication will
#' then set to the argument `object` of
#' [lavaan::parameterEstimates()].
#'
#' # The background for having the `results_fun` argument
#'
#' In the early development of
#' `power4mome`, `test_fun` is designed
#' to accept existing functions from
#' other packages, such as
#' [manymome::indirect_effect()]. Their
#' outputs are not of the required
#' format for power analysis, and so
#' results functions are needed to
#' process their outputs. In the current
#' version of `power4mome`, some
#' ready-to-use test functions, usually
#' wrappers of these existing functions
#' from other packages, have been
#' developed, and they no longer need
#' results functions to process the
#' output. The argument `results_fun` is
#' kept for backward compatibility and
#' advanced users to use test functions
#' from other packages.
#'
#' @seealso See [power4test()] for
#' the all-in-one function that uses
#' this function. See [test_indirect_effect()],
#' [test_cond_indirect()],
#' [test_cond_indirect_effects()],
#' [test_moderation()],
#' [test_index_of_mome()], and
#' [test_parameters()] for examples of
#' test functions.
#'
#' @return
#' An object of the class `test_out`,
#' which is a list of length equal to
#' `sim_all`, one element for each
#' replication. Each element of the list
#' has two elements:
#'
#' - `test`: The output of the function
#'   set to `test_fun`.
#'
#' - `test_results`: The output of
#'   the function set to `results_fun`.
#'
#' @param sim_all The output of
#' [sim_out()].
#'
#' @param test_fun A function to do the
#' test. See 'Details' for the requirement
#' of this function. There are some
#' built-in test functions in `power4mome`,
#' described in 'Details'.
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
#' @param cl A cluster, such as one created
#' by [parallel::makeCluster()]. If `NULL`,
#' a cluster will be created, but will be
#' stopped on exit. If set to an existing
#' cluster, it will not be stopped when
#' the function exits; users need to
#' stop it manually.
#'
#' @examples
#'
#' # Specify the population model
#'
#' mod <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' # Specify the effect sizes (population parameter values)
#'
#' es <-
#' "
#' y ~ m: m
#' m ~ x: m
#' y ~ x: n
#' "
#'
#' # Generate several simulated datasets
#'
#' data_all <- sim_data(nrep = 5,
#'                      model = mod,
#'                      pop_es = es,
#'                      n = 100,
#'                      iseed = 1234)
#'
#' # Fit the population model to each datasets
#'
#' fit_all <- fit_model(data_all)
#'
#' # Generate Monte Carlo estimates for forming Monte Carlo confidence intervals
#'
#' mc_all <- gen_mc(fit_all,
#'                  R = 50,
#'                  iseed = 4567)
#'
#' # Combine the results to a 'sim_all' object
#' sim_all <- sim_out(data_all = data_all,
#'                    fit = fit_all,
#'                    mc_out = mc_all)
#'
#' # Test the indirect effect in each replication
#' # Set `parallel` to TRUE for faster, usually much faster, analysis
#' # Set `progress` to TRUE to display the progress of the analysis
#'
#' test_all <- do_test(sim_all,
#'                     test_fun = test_indirect_effect,
#'                     test_args = list(x = "x",
#'                                      m = "m",
#'                                      y = "y",
#'                                      mc_ci = TRUE),
#'                     parallel = FALSE,
#'                     progress = FALSE)
#'
#' # The result for each dataset
#' lapply(test_all, function(x) x$test_results)
#'
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
                    ncores = max(1, parallel::detectCores(logical = FALSE) - 1),
                    cl = NULL) {
  out <- do_FUN(X = sim_all,
                FUN = do_test_i,
                test_fun = test_fun,
                test_args = test_args,
                map_names = map_names,
                results_fun = results_fun,
                results_args = results_args,
                parallel = parallel,
                progress = progress,
                ncores = ncores,
                cl = cl)

  attr(out, "test_fun") <- test_fun
  attr(out, "test_args") <- test_args
  attr(out, "map_names") <- map_names
  attr(out, "results_fun") <- results_fun
  attr(out, "results_args") <- results_args

  class(out) <- c("test_out", class(out))
  return(out)
}

#' @noRd
do_test_i <- function(out_i,
                      test_fun,
                      test_args = list(0),
                      map_names = c(fit = "fit"),
                      results_fun = NULL,
                      results_args = list()) {
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
  # # Disable error catching
  # # Error *should* be raised
  # out0 <- tryCatch(
  #           do.call(
  #             test_fun,
  #             args1
  #           ),
  #           error = function(e) e)
  # if (inherits(out0, "error")) {
  #   return(list(test = NULL,
  #               test_results = NULL))
  # }
  out0 <- do.call(
            test_fun,
            args1
          )
  if (is.function(results_fun)) {
    # # Disable error catching
    # # Error *should* be raised
    # out1 <- tryCatch(
    #           do.call(
    #             results_fun,
    #             c(list(out0), results_args)
    #           ),
    #           error = function(e) NULL)
    out1 <- do.call(
                results_fun,
                c(list(out0), results_args)
              )
  } else {
    out1 <- out0
  }

  return(list(test = out0,
              test_results = out1))
}
