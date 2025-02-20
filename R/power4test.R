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
#'    to a `power4test` object.
#'
#' @param object Optional. If set to a
#' `power4test` object, it will be
#' updated using the value(s) in `n`
#' and/or `pop_es`. Default is `NULL`.
#'
#' @param nrep The number of replications
#' to generate the simulated datasets.
#' Default is 10.
#'
#' @param ptable The output of
#' [ptable_pop()], which is a
#' `ptable_pop` object, representing the
#' population model. If `NULL`, the
#' default, [ptable_pop()] will be
#' called to generate the `ptable_pop`
#' object.
#'
#' @param model The `lavaan` model
#' syntax of the population model.
#' Required. Ignored if `ptable` is
#' specified.
#'
#' @param pop_es The character to
#' specify population effect sizes. See
#' 'Details' of [ptable_pop()] on how to
#' set the effect sizes for this
#' argument. Ignored if `ptable` is
#' specified.
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
#' @param x_fun The function(s) used to
#' generate the exogenous variables. If
#' not supplied, or set to `list()`, the
#' default, the variables are generated
#' from a multivariate normal
#' distribution. See 'Details' of
#' [sim_data()] on how to use this
#' argument.
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
#' to generate the Monte Carlo or
#' bootstrapping estimates
#' for each fit output. No Monte Carlo
#' nor bootstrapping
#' estimates will be generated if `R`
#' is set to `NULL`.
#'
#' @param ci_type The type of
#' simulation-based confidence
#' intervals to use. Can be either
#' `"mc"` for Monte Carlo method
#' (the default) or `"boot"` for
#' nonparametric bootstrapping method.
#'
#' @param gen_mc_args A list of
#' arguments to be passed to
#' [manymome::do_mc()] when generating
#' the Monte Carlo estimates.
#' Should be a named argument
#' with names being the names of the
#' arguments. Used only if
#' `ci_type` is `"mc".`
#'
#' @param gen_boot_args A list of
#' arguments to be passed to
#' [manymome::do_boot()] when generating
#' the bootstrap estimates.
#' Should be a named argument
#' with names being the names of the
#' arguments. Used only if
#' `ci_type` is `"boot".
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

power4test <- function(object = NULL,
                       nrep = NULL,
                       ptable = NULL,
                       model = NULL,
                       pop_es = NULL,
                       n = NULL,
                       number_of_indicators = NULL,
                       reliability = NULL,
                       x_fun = list(),
                       fit_model_args = list(),
                       R = NULL,
                       ci_type = "mc",
                       gen_mc_args = list(),
                       gen_boot_args = list(),
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
                       progress = TRUE,
                       ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {

  # TOOD:
  # - Should allow only limited changes
  #   when updating a power4test object.

  if (progress) {
    cat("Displaying progress enabled. Set 'progress = FALSE' to hide the progress.\n")
  }

  ci_type <- match.arg(ci_type,
                       c("mc", "boot"))

  update_test <- FALSE

  update_power4test <- FALSE
  update_data <- TRUE
  if (!is.null(object)) {
    if (!inherits(object, "power4test")) {
      stop("object is not a power4test object.")
    }
    update_power4test <- TRUE
  }

  if (!update_power4test && is.null(nrep)) {
    stop("'nrep' must be set unless updating a power4test object.")
  }

  if (!update_power4test) {
    # Store the evaluated arguments
    args <- formals(power4test)
    args <- lapply(args,
                   eval,
                   envir = parent.frame())
  } else {
    args <- attr(object, "args")
  }
  my_call <- match.call()
  call_args <- as.list(my_call)[-1]
  call_args <- lapply(call_args,
                      eval,
                      envir = parent.frame())
  args <- utils::modifyList(args,
                            as.list(call_args))
  args$object <- NULL
  # args available in all cases.
  # It should be used whenever possible,
  # unless we explicitly need the value in this call.

  if (update_power4test && !is.null(pop_es)) {
    # Population effect size changed
    # Data must be updated
    update_data <- TRUE
    # Update ptable
    old_ptable <- object$sim_all[[1]]$ptable
    new_ptable <- update_ptable_pop(old_ptable,
                                    new_pop_es = pop_es)
    ptable <- new_ptable
    # Use the updated population model
    args$ptable <- ptable
    args$model <- NULL
    args$pop_es <- NULL
  } else {
    # Population model not updated
    # Still update data if any of the following has changed
    if (is.null(n) &&
        is.null(number_of_indicators) &&
        is.null(reliability) &&
        identical(x_fun, list()) &&
        is.null(nrep)) {
      update_data <- FALSE
    }
  }

  if (update_data) {
    # Determine the arguments to use
    if (update_power4test) {
      # Regenerate the data
      # Mandatory use of stored arguments.
      # Changes in this call already incorporated into args.
      if (progress) {
        cat("Re-simulate the data:\n")
      }
      sim_data_args <- list(nrep = args$nrep,
                            ptable = ptable,
                            model = args$model,
                            pop_es = args$pop_es,
                            n = args$n,
                            number_of_indicators = args$number_of_indicators,
                            reliability = args$reliability,
                            x_fun = args$x_fun,
                            iseed = args$iseed,
                            parallel = args$parallel,
                            progress = args$progress,
                            ncores = args$ncores)

      fit_model_args <- args$fit_model_args
    } else {
      # A fresh run to generate the data
      if (progress) {
        cat("Simulate the data:\n")
      }
      sim_data_args <- list(nrep = nrep,
                            ptable = ptable,
                            model = model,
                            pop_es = pop_es,
                            n = n,
                            number_of_indicators = number_of_indicators,
                            reliability = reliability,
                            x_fun = x_fun,
                            iseed = iseed,
                            parallel = parallel,
                            progress = progress,
                            ncores = ncores)
    }
    data_all <- do.call(sim_data,
                        sim_data_args)

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
    if (!is.null(args$R) && (args$ci_type == "mc")) {
      # TODO:
      # - iseed should be used only once
      mc_args0 <- utils::modifyList(args$gen_mc_args,
                                    list(fit_all = fit_all,
                                        R = args$R,
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

    if (!is.null(args$R) && (args$ci_type == "boot")) {
      # TODO:
      # - iseed should be used only once
      boot_args0 <- utils::modifyList(args$gen_boot_args,
                                      list(fit_all = fit_all,
                                           R = args$R,
                                           parallel = parallel,
                                           progress = progress,
                                           ncores = ncores))
      if (progress) {
        cat("Generate bootstrap estimates:\n")
      }
      boot_all <- do.call(gen_boot,
                          boot_args0)
    } else {
      boot_all <- rep(NA, length(fit_all))
    }

    sim_all <- sim_out(data_all = data_all,
                       fit = fit_all,
                       mc_out = mc_all,
                       boot_out = boot_all)
  } else {
    sim_all <- object$sim_all
  }

  if (is.null(object$test_all) &&
      is.null(test_fun)) {
    do_the_test <- FALSE
  }

  if (update_data && !is.null(object$test_all)) {
    # Data updated and test exits.
    # Mandatory update of test to make the object
    # internally consistent.
    update_test <- TRUE
  }

  test_all <- NULL

  if (do_the_test && !update_test) {
    if (is.null(test_name)) {
      test_name <- deparse(substitute(test_fun))
      test_args_tmp <- utils::modifyList(test_args,
                                         list(get_test_name = TRUE))
      test_name0 <- tryCatch(do.call(test_fun,
                                     test_args_tmp),
                             error = function(e) e)
      if (!inherits(test_name0, "error")) {
        if (is.character(test_name0) &&
            length(test_name0) == 1) {
          test_name <- test_name0
        }
      }
    }
    if (progress) {
      cat("Do the test:",
          test_name,
          "\n")
    }
    map_names0 <- tryCatch(test_fun(get_map_names = TRUE),
                           error = function(e) e)
    if (!inherits(map_names, "error")) {
      if (is.character(map_names0) && !is.null(names(map_names0))) {
        map_names <- map_names0
      }
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
    attr(test_all, "test_note") <- test_note
    attr(test_all, "test_name") <- test_name
    test_all <- list(test_all)
    names(test_all) <- test_name
  }

  if (update_test) {
    if (progress) {
      cat("Update the test(s):\n")
    }
    # Only update tests. Ignore test arguments
    # Clear all argument values about test
    tmp <- formals(power4test)
    args$test_fun <- tmp$test_fun
    args$test_args <- tmp$test_args
    args$map_names <- tmp$map_names
    args$results_fun <- args$results_fun
    args$results_args <- args$results_args
    args$test_name <- args$test_name
    args$test_note <- args$test_note
    test_all <- sapply(object$test_all,
                       update_test_i,
                       sim_all = sim_all,
                       parallel = args$parallel,
                       progress = args$progress,
                       ncores = args$ncores,
                       simplify = FALSE,
                       USE.NAMES = TRUE)

  }

  if (!update_power4test) {
    out <- list(sim_all = sim_all,
                test_all = test_all)
    attr(out, "args") <- args
    class(out) <- c("power4test", class(out))
  } else {
    attr(object, "args") <- args
    if (update_data) {
      object$sim_all <- sim_all
    }
    if (!is.null(test_all)) {
      if (update_test) {
        object$test_all <- test_all
      } else {
        object$test_all[[test_name]] <- test_all[[1]]
      }
    }
    out <- object
  }
  if (!is.null(out$test_all)) {
    if (!inherits(out$test_all, "test_out_list")) {
      class(out$test_all) <- c("test_out_list", class(out$test_all))
    }
  }
  out
}

#' @param digits The numbers of digits
#' displayed after the decimal.
#'
#' @param digits_descriptive The
#' number of digits displayed after
#' the decimal for the descriptive
#' statistics table.
#'
#' @param x The `power4test` object
#' to be printed.
#'
#' @param what A string vector of
#' what to print, `"data"` for
#' simulated data and `"test"` for
#' stored test(s). Default is
#' `c("data", "test")`.
#'
#' @param data_long If `TRUE`, detailed
#' results will be printed when printing
#' the simulated data.
#'
#' @param test_long If `TRUE`, detailed
#' results will be printed when printing
#' test(s).
#'
#' @param ... Optional arguments to
#' be passed to other print methods.
#'
#' @return
#' The `print` method of `power4test`
#' return `x` invisibly. Called for
#' its side effect.
#'
#' @rdname power4test
#' @export
print.power4test <- function(x,
                             what = c("data", "test"),
                             digits = 3,
                             digits_descriptive = 2,
                             data_long = FALSE,
                             test_long = FALSE,
                             ...) {
  what <- match.arg(what, several.ok = TRUE)
  if ("data" %in% what) {
    print(x$sim_all,
          data_long = data_long,
          digits = digits,
          digits_descriptive = digits_descriptive,
          ...)
  }
  if (("test" %in% what) &&
      !is.null(x$test_all)) {
    print(x$test_all,
          test_long = test_long,
          digits = digits,
          digits_descriptive = digits_descriptive,
          ...)
  }
  invisible(x)
}

#' @noRd
update_test_i <- function(test_i,
                          sim_all,
                          parallel = FALSE,
                          progress = FALSE,
                          ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  test_fun <- attr(test_i, "test_fun")
  test_args <- attr(test_i, "test_args")
  map_names <- attr(test_i, "map_names")
  results_fun <- attr(test_i, "results_fun")
  results_args <- attr(test_i, "results_args")
  test_name <- attr(test_i, "test_name")
  if (progress) {
      cat("Update",
          test_name,
          ":\n")
  }
  test_note <- attr(test_i, "test_note")
  test_new <- do_test(sim_all,
                      test_fun = test_fun,
                      test_args = test_args,
                      map_names = map_names,
                      results_fun = results_fun,
                      results_args = results_args,
                      parallel = parallel,
                      progress = progress,
                      ncores = ncores)
  attr(test_new, "test_note") <- test_note
  attr(test_new, "test_name") <- test_name

  test_new
}
