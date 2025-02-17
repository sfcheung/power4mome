#' @title Estimate the Power of a Test
#'
#' @description An all-in-one function
#' that receives a model specification,
#' generate datasets, fits a model, does
#' the target test, and returns the test
#' results.
#'
#' @details
#' It is an all-in-one function for
#' estimating the power of a test for
#' a model, given the sample size
#' and effect sizes.
#'
#' This is the workflow:
#'
#' - If an object with model and data
#'  already generated is supplied
#'  through `sim_all`, such as the
#'  output of [sim_out()] or of
#'  [power4test()] with `do_the_test`
#'  set to `FALSE`, the following steps
#'  will be skipped and go directly to
#'  doing the test.
#'
#'    - Call [sim_data()] to determine
#'      the population model and
#'      generate the datasets.
#'
#'    - Call [fit_model()] to fit the
#'      model to each of the datasets.
#'
#'    - If `R` is not `NULL`, call
#'      [gen_mc()] to generate Monte
#'      Carlo estimates using
#'      [manymome::do_mc()].
#'
#'    - Merge the results into a
#'      `sim_out` object by calling
#'      [sim_out()].
#'
#'    - If `do_the_test` is `FALSE`,
#'      skip the reaming steps and
#'      return a `power4test` object,
#'      which contains only the
#'      generated and optionally the
#'      Monte Carlo estimates.
#'
#' - If `do_the_test` is `TRUE`, do
#'   the test.
#'
#'    - [do_test()] will be called to do
#'      the test in the fit output of
#'      each datasets.
#'
#' - Return a `power4test` object which
#'   include the output of `sim_out`
#'   and, if `do_the_test` is `TRUE`,
#'   the output of [do_test()].
#'
#' This function is to be used when
#' users are interested only in the
#' power of a specific test on a
#' particular aspect of the model, such
#' as a parameter, given a specific
#' effect sizes and sample sizes.
#'
#' @return
#' An object of the class `power4test`,
#' which is a list of with two elements:
#'
#' - `sim_all`: The output of [sim_out()].
#'
#' - `test_all`: A named list of the
#'    output of [do_test()]. The names
#'    are the values of `test_name`.
#'    This list can have more than one
#'    test because a call to
#'    [power4test()] can add new tests
#'    to a `power4mome` object.
#'
#' @param nrep The number of replications
#' to generate the simulated datasets.
#' Default is 10.
#'
#' @param model The `lavaan` model
#' syntax of the population model.
#' Required.
#'
#' @param pop_es The character to
#' specify population effect sizes.
#' See 'Details' on how to set the
#' effect sizes for this argument.
#' Required.
#'
#' @param n The sample size for each
#' dataset. Default is 100.
#'
#' @param iseed The seed for the random
#' number generator. Default is `NULL`
#' and the seed is not changed.
#'
#' @param number_of_indicators A named
#' vector to specify the number of
#' indicators for each factors. See
#' 'Details' on how to set this
#' argument. Default is `NULL` and all
#' variables in the model syntax are
#' observed variables.
#'
#' @param reliability A named vector
#' to set the reliability coefficient
#' of each set of indicators. Default
#' is `NULL`.
#'
#' @param fit_model_args A list of the
#' arguments to be passed to
#' [lavaan::sem()] when fitting the
#' model.
#' Should be a named argument
#' with names being the names of the
#' arguments.
#'
#' @param R The number of replications
#' to generate the Monte Carlo estimates
#' for each fit output. No Monte Carlo
#' estimates will be generated if `R`
#' is set to `NULL`.
#'
#' @param gen_mc_args A list of
#' arguments to be passed to
#' [manymome::do_mc()] when generating
#' the Monte Carlo estimates.
#' Should be a named argument
#' with names being the names of the
#' arguments.
#'
#' @param test_fun A function to do the
#' test. See the help page of
#' [do_test()] for the requirements of
#' this function.
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
#' @param test_name String. The name
#' of the test. Default is `NULL`,
#' and the name will be created from
#' `test_fun`. Note that if `sim_out`
#' is a `power4test` object and already
#' has a test of this name stored, it
#' will be replaced by the new results
#'
#' @param test_note String. An optional
#' note for the test, stored in the
#' attribute `test_note` of the output
#' of [do_test()]. Default is `NULL`.
#'
#' @param do_the_test If `TRUE`,
#' [do_test()] will be called to do the
#' specified test in the fit output of
#' each dataset.
#'
#' @param sim_all If set to either a
#' `sim_out` object (the output of
#' [sim_out()] or a `power4test` object
#' (the output of [power4test()]), the
#' stored datasets and fit outputs will
#' be used for doing the test.
#'
#' @param iseed The seed for the random
#' number generator. Default is `NULL`
#' and the seed is not changed. This
#' seed will be set only once, when
#' calling [sim_data()].
#'
#' @param parallel If `TRUE`, parallel
#' processing will be used when calling
#' other functions, if appropriate.
#' Default is `FALSE`.
#'
#' @param progress If `TRUE`, the progress
#' of each step will be displayed.
#' Default is `FALSE.
#'
#' @param ncores The number of CPU
#' cores to use if parallel processing
#' is used.
#'
#' @examples
#'
#'
#' model_simple_med <-
#' "
#' m ~ a*x
#' y ~ b*m + x
#' ab := a * b
#' "
#'
#' model_simple_med_es <- c("y ~ m" = "l",
#'                          "m ~ x" = "m",
#'                          "y ~ x" = "n")
#'
#' test_ab <- function(object,
#'                     alpha = .05) {
#'   est <- lavaan::parameterEstimates(object)
#'   i <- match("ab", est$lhs)
#'   out <- c(est = est[i, "est"],
#'            cilo = est[i, "ci.lower"],
#'            cihi = est[i, "ci.upper"],
#'            sig = as.numeric(est[i, "pvalue"] < alpha))
#'   out
#' }
#'
#' ab_results <- function(object) {
#'   object
#' }
#'
#' power_all_test_only_par <- power4test(nrep = 50,
#'                                       model = model_simple_med,
#'                                       pop_es = model_simple_med_es,
#'                                       n = 100,
#'                                       fit_model_args = list(estimator = "ML"),
#'                                       R = NULL,
#'                                       test_fun = test_ab,
#'                                       map_names = c(object = "fit"),
#'                                       results_fun = ab_results,
#'                                       parallel = FALSE,
#'                                       progress = TRUE)
#' names(power_all_test_only_par$test_all)
#' test_results_all <- sapply(power_all_test_only_par$test_all$test_ab,
#'                            function(xx) xx$test_results)
#' test_results_all <- as.data.frame(t(test_results_all))
#' colMeans(test_results_all, na.rm = TRUE)
#'
#' @export

power4test <- function(nrep = 10,
                       model,
                       pop_es,
                       n,
                       number_of_indicators = NULL,
                       reliability = NULL,
                       fit_model_args = list(),
                       R = 100,
                       gen_mc_args = list(),
                       test_fun = NULL,
                       test_args = list(),
                       map_names = c(fit = "fit"),
                       results_fun = NULL,
                       results_args = list(),
                       test_name = NULL,
                       test_note = NULL,
                       do_the_test = TRUE,
                       sim_all = NULL,
                       iseed = NULL,
                       parallel = FALSE,
                       progress = FALSE,
                       ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  update_test <- FALSE
  if (is.null(sim_all)) {
    if (progress) {
      cat("Simulate the data:\n")
    }
    data_all <- sim_data(nrep = nrep,
                        model = model,
                        pop_es = pop_es,
                        n = n,
                        number_of_indicators = number_of_indicators,
                        reliability = reliability,
                        iseed = iseed,
                        parallel = parallel,
                        progress = progress,
                        ncores = ncores)

    fit_args0 <- utils::modifyList(fit_model_args,
                                  list(data_all = data_all,
                                        parallel = parallel,
                                        progress = progress,
                                        ncores = ncores))
    if (progress) {
      cat("Fit the model:\n")
    }
    fit_all <- do.call(fit_model,
                      fit_args0)

    if (!is.null(R)) {
      # iseed should be used only once
      mc_args0 <- utils::modifyList(gen_mc_args,
                                    list(fit_all = fit_all,
                                        R = R,
                                        parallel = parallel,
                                        progress = progress,
                                        ncores = ncores))
      if (progress) {
        cat("Generate Monte Carlo estimates:\n")
      }
      mc_all <- do.call(gen_mc,
                        mc_args0)
    } else {
      mc_all <- rep(NA, length(fit_all))
    }

    sim_all <- sim_out(data_all = data_all,
                       fit = fit_all,
                       mc_out = mc_all)

  } else {
    if (inherits(sim_all, "power4test")) {
      update_test <- TRUE
      out <- sim_all
      sim_all <- sim_all$sim_all
    }
  }

  if (!do_the_test) {
    if (update_test) {
      return(out)
    } else {
      test_all <- NULL
      out <- list(sim_all = sim_all,
                  test_all = test_all)
      class(out) <- c("power4test", class(out))
      return(out)
    }
  }

  if (progress) {
    cat("Do the test:\n")
  }
  test_all <- do_test(sim_all,
                      test_fun = test_fun,
                      test_args = test_args,
                      map_names = map_names,
                      results_fun = results_fun,
                      results_args = results_args,
                      parallel = parallel,
                      progress = progress,
                      ncores = ncores)
  if (is.null(test_name)) {
    test_name <- deparse(substitute(test_fun))
  }
  attr(test_all, "test_note") <- test_note
  if (update_test) {
    out$test_all[[test_name]] <- test_all
    return(out)
  } else {
    test_all <- list(test_all)
    names(test_all) <- test_name
    out <- list(sim_all = sim_all,
                test_all = test_all)
    class(out) <- c("power4test", class(out))
    return(out)
  }
}
