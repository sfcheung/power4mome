#' @title Estimate the Power of a Test
#'
#' @description An all-in-one function
#' that receives a model specification,
#' generates datasets, fits a model, does
#' the target test, and returns the test
#' results.
#'
#' @details
#' It is an all-in-one function for
#' estimating the power of a test for
#' a model, given the sample size
#' and effect sizes.
#'
#' @details
#'
#' # Workflow
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
#'    - If `R` is not `NULL` and
#'      `ci_type = "mc"`, call
#'      [gen_mc()] to generate Monte
#'      Carlo estimates using
#'      [manymome::do_mc()].
#
#'    - If `R` is not `NULL` and
#'      `ci_type = "boot"`, call
#'      [gen_boot()] to generate
#'      bootstrap estimates using
#'      [manymome::do_boot()].
#'
#'    - Merge the results into a
#'      `sim_out` object by calling
#'      [sim_out()].
#'
#'    - If `do_the_test` is `FALSE`,
#'      skip the remaining steps and
#'      return a `power4test` object,
#'      which contains only the data
#'      generated and optionally the
#'      Monte Carlo or bootstrap
#'      estimates.
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
#' power of one or several tests on a
#' particular aspect of the model, such
#' as a parameter, given a specific
#' effect sizes and sample sizes.
#'
#' Detailed description on major
#' arguments can be found in sections
#' below.
#'
#' @details
#' # Model Fitting Arguments
#'
#' For power analysis, usually, the
#' population model (`model`) is to be
#' fitted, and there is no need to
#' set `fit_model_args`.
#'
#' If power analysis is to be conducted
#' for fitting a model that is not the
#' population model, of if non-default
#' settings are desired when fitting
#' a model, then the argument `fit_model_args`
#' needed to be set to customize the
#' call to [fit_model()]. For example,
#' users may want to examine the power
#' of a test when a misspecified model
#' is fitted, or the power of a test
#' when MLR is used as the estimator
#' when calling [lavaan::sem()].
#'
#' This is
#' an advanced feature, to be described
#' in a vignette.
#'
#' @inheritSection do_test Major Test-Related Arguments
#'
# It is a known issue that inherited
# section cannot be placed before @details
# https://github.com/r-lib/roxygen2/issues/900
#'
#' @details
#'
#' # Advanced Features
#'
#' ## Updating a Condition
#'
#' This function can also be used to
#' update a condition when only some
#' selected aspects changed. For example,
#' without calling this function with
#' all the arguments set just to change
#' the sample size, it can be called
#' by supplying an existing
#' `power4test` object and set only
#' `n` to a new sample size. The data
#' and the tests will be updated
#' automatically.
#'
#' ## Multiple Models
#'
#' More than one model can be fitted to
#' each replication. This is done
#' by setting `fit_model_args` to
#' a named list. The names are the names
#' used to identify the models, and
#' each element is a list of named
#' list of arguments for a model.
#'
#' For example:
#'
#' \preformatted{
#' fit_model_args = list(fit = list(),
#'                       fit2 = list(model = mod2),
#'                       fit3 = list(model = mod3))
#' }
#'
#' Three models will be fitted. The
#' first model is the model ued to
#' generate the data, named `"fit"`.
#' The second and first models are
#' named `"fit2"` and `"fit3"`,
#' respectively, with `"fit2"` fitted
#' with `model = mod2` and `"fit3"`
#' fitted with `model = mod3`.
#'
#' If Monte Carlos or bootstrap estimates
#' are to be generated, they will be
#' generated for each model, using the
#' values for their arguments.
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
#' @inheritParams do_test
#'
#' @inheritParams fit_model
#'
#' @param object Optional. If set to a
#' `power4test` object, it will be
#' updated using the value(s) in `n`,
#' `pop_es`, and/or `nrep`. Default is `NULL`.
#'
#' @param nrep The number of replications
#' to generate the simulated datasets.
#' Default is `NULL`. Must be set when
#' called to create a `power4test`
#' object.
#'
#' @param ptable The output of
#' [ptable_pop()], which is a
#' `ptable_pop` object, representing the
#' population model. If `NULL`, the
#' default, [ptable_pop()] will be
#' called to generate the `ptable_pop`
#' object using `model` and `pop_es`.
#'
#' @param model The `lavaan` model
#' syntax of the population model.
#' Required. Ignored if `ptable` is
#' specified. See 'Details' of
#' [ptable_pop()] on how to use it for
#' models with latent factors
#' and indicators. Ignored if `ptable` is
#' specified.
#'
#' @param pop_es The character vector to
#' specify population effect sizes. See
#' 'Details' of [ptable_pop()] on how to
#' set the effect sizes for this
#' argument. Ignored if `ptable` is
#' specified.
#'
#' @param standardized Logical. If
#' `TRUE`, the default, variances and
#' error variances are scaled to ensure
#' the population variances of the
#' endogenous variables are close to
#' one, and hence the effect sizes are
#' standardized effect sizes if the
#' variances of the continuos exogenous
#' variables are also equal to one.
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
#' 'Details' of [sim_data()] on how to set this
#' argument. Default is `NULL` and all
#' variables in the model syntax are
#' observed variables.
#'
#' @param reliability A named vector
#' to set the reliability coefficient
#' of each set of indicators. Default
#' is `NULL`. See 'Details' of
#' [sim_data()] on how to set this
#' argument.
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
#' arguments to be passed to [fit_model()]
#' when fitting the
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
#' See [sim_data()] on the details.
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
#' @param test_name String. The name
#' of the test. Default is `NULL`,
#' and the name will be created from
#' `test_fun`. Note that if `sim_out`
#' is a `power4test` object and already
#' has a test of this name stored, it
#' will be replaced by the new results.
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
#' @param es1 A named vector to set the
#' values for each label of the effect
#' size of correlations and regression
#' paths.
#' Default is `c("n" = .00, "nil" = .00, "s" = .10, "m" = .30, "l" = .50)`.
#' Used only if `pop_es` is a named
#' vector. See [ptable_pop()] for
#' further information.
#'
#' @param es2 A named vector to set the
#' values for each label of the effect
#' size of product term.
#' Default is `c("n" = .00, "nil" = .00, "s" = .05, "m" = .10, "l" = .15)`.
#' Used only if `pop_es` is a named
#' vector. See [ptable_pop()] for
#' further information.
#'
#' @param n_std The sample size used to
#' determine the error variances by
#' simulation when `std_force_monte_carlo`
#' is `TRUE`. Default is 100000.
#'
#' @param std_force_monte_carlo Logical.
#' If `FALSE`, the default,
#' standardization is done analytically
#' if the model has no product terms,
#' and by simulation if the model has
#' product terms. If `TRUE`, simulation
#' will be used whether the model has
#' product terms or not. Always fall
#' back to standardization if
#' analytical standardization failed.
#'
#' @examples
#'
#' model_simple_med <-
#' "
#' m ~ a * x
#' y ~ b * m + x
#' ab := a * b
#' "
#'
#' model_simple_med_es <- c("y ~ m" = "l",
#'                          "m ~ x" = "m",
#'                          "y ~ x" = "n")
#'
#' out <- power4test(nrep = 50,
#'                   model = model_simple_med,
#'                   pop_es = model_simple_med_es,
#'                   n = 100,
#'                   test_fun = test_parameters,
#'                   test_args = list(pars = "a"),
#'                   iseed = 1234,
#'                   parallel = FALSE,
#'                   progress = TRUE)
#'
#' print(out,
#'       test_long = TRUE)
#'
#' # Add one more test
#'
#' out <- power4test(out,
#'                   test_fun = test_parameters,
#'                   test_args = list(op = ":="))
#' print(out,
#'       test_long = TRUE)
#'
#' @export

power4test <- function(object = NULL,
                       nrep = NULL,
                       ptable = NULL,
                       model = NULL,
                       pop_es = NULL,
                       standardized = TRUE,
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
                       ncores = max(1, parallel::detectCores(logical = FALSE) - 1),
                       es1 = c("n" = .00,
                               "nil" = .00,
                               "s" = .10,
                               "m" = .30,
                               "l" = .50),
                       es2 = c("n" = .00,
                               "nil" = .00,
                               "s" = .05,
                               "m" = .10,
                               "l" = .15),
                       n_std = 1090000,
                       std_force_monte_carlo= FALSE) {

  # TOOD:
  # - Should allow only limited changes
  #   when updating a power4test object.

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

  if (args$progress) {
    cat("Displaying progress enabled. Set 'progress = FALSE' to hide the progress.\n")
  }

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
      if (args$progress) {
        cat("Re-simulate the data:\n")
      }
      sim_data_args <- list(nrep = args$nrep,
                            ptable = ptable,
                            model = args$model,
                            pop_es = args$pop_es,
                            es1 = es1,
                            es2 = es2,
                            standardized = standardized,
                            n_std = n_std,
                            std_force_monte_carlo = std_force_monte_carlo,
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
      if (args$progress) {
        cat("Simulate the data:\n")
      }
      sim_data_args <- list(nrep = args$nrep,
                            ptable = args$ptable,
                            model = args$model,
                            pop_es = args$pop_es,
                            es1 = args$es1,
                            es2 = args$es2,
                            standardized = standardized,
                            n_std = n_std,
                            std_force_monte_carlo = std_force_monte_carlo,
                            n = args$n,
                            number_of_indicators = args$number_of_indicators,
                            reliability = args$reliability,
                            x_fun = args$x_fun,
                            iseed = args$iseed,
                            parallel = args$parallel,
                            progress = args$progress,
                            ncores = args$ncores)
    }
    data_all <- do.call(sim_data,
                        sim_data_args)

    # fit_model_args must always be a named list of models
    fit_k <- FALSE
    if (length(fit_model_args) > 1) {
      fit_k <- all(sapply(fit_model_args,
                          is.list))
      if (fit_k) {
        if (is.null(names(fit_model_args))) {
          # Auto names
          names(fit_model_args) <- paste0("fit", seq_along(fit_model_args))
          names(fit_model_args)[1] <- "fit"
        } else {
          if (any(names(fit_model_args) %in% "")) {
            stop("All models in 'fit_model_args' must be named")
          }
          if (names(fit_model_args)[1] != "fit") {
            stop("The first model must be named 'fit'.")
          }
        }
      }
    } else {
      # One model or not specified
      fit_model_args <- list(fit = fit_model_args)
    }
    fit_args0 <- sapply(fit_model_args,
                        utils::modifyList,
                        val = list(parallel = args$parallel,
                                   progress = args$progress,
                                   ncores = args$ncores),
                        simplify = FALSE)

    # fit_args0 <- utils::modifyList`(fit_model_args,
    #                               list(data_all = data_all,
    #                                     parallel = parallel,
    #                                     progress = progress,
    #                                     ncores = ncores))
    if (args$progress) {
      cat("Fit the model(s):\n")
    }
    # fit_all is always a named list
    fit_all <- sapply(fit_args0,
                      function(x,
                               data_all) {
                        do.call(fit_model,
                                c(list(data_all = data_all),
                                  x))
                      },
                      data_all = data_all,
                      simplify = FALSE)
    # fit_all <- do.call(fit_model,
    #                    fit_args0)
    if (!is.null(args$R) && (args$ci_type == "mc")) {
      # - It's OK to reuse iseed because the processes are different.
      mc_args0 <- utils::modifyList(args$gen_mc_args,
                                    list(R = args$R,
                                        parallel = args$parallel,
                                        progress = args$progress,
                                        ncores = args$ncores,
                                        iseed = args$iseed))
      if (args$progress) {
        cat("Generate Monte Carlo estimates:\n")
      }
      # mc_all is always a named list
      mc_all <- sapply(fit_all,
                        function(x) {
                          do.call(gen_mc,
                                  c(list(fit_all = x),
                                    mc_args0))
                        },
                        simplify = FALSE)
      # mc_all <- do.call(gen_mc,
      #                   mc_args0)
    } else {
      mc_all <- rep(NA, length(fit_all[[1]]))
      mc_all <- sapply(names(fit_all),
                       function(x) mc_all,
                       simplify = FALSE)
    }
    # The first mc_all is always named "mc_out"
    tmp <- paste0(names(fit_all),
                  "_mc_out")
    tmp[1] <- "mc_out"
    names(mc_all) <- tmp

    if (!is.null(args$R) && (args$ci_type == "boot")) {
      # - It's OK to reuse the seed because the processes are different.
      boot_args0 <- utils::modifyList(args$gen_boot_args,
                                      list(R = args$R,
                                           parallel = args$parallel,
                                           progress = args$progress,
                                           ncores = args$ncores,
                                           iseed = args$iseed))
      if (args$progress) {
        cat("Generate bootstrap estimates:\n")
      }
      # boot_all is always a named list
      boot_all <- sapply(fit_all,
                        function(x) {
                          do.call(gen_boot,
                                  c(list(fit_all = x),
                                    boot_args0))
                        },
                        simplify = FALSE)
      # boot_all <- do.call(gen_boot,
      #                     boot_args0)
    } else {
      boot_all <- rep(NA, length(fit_all[[1]]))
      boot_all <- sapply(names(fit_all),
                         function(x) boot_all,
                         simplify = FALSE)
    }
    # The first boot_all is always named "boot_out"
    tmp <- paste0(names(fit_all),
                  "_boot_out")
    tmp[1] <- "boot_out"
    names(boot_all) <- tmp

    sim_all <- do.call(sim_out,
                       c(list(data_all = data_all),
                         fit_all,
                         mc_all,
                         boot_all))

    # sim_all <- sim_out(data_all = data_all,
    #                    fit = fit_all,
    #                    mc_out = mc_all,
    #                    boot_out = boot_all)
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
    if (args$progress) {
      cat("Do the test:",
          test_name,
          "\n")
    }
    tmp_args <- test_args
    tmp_args$get_map_names <- TRUE
    map_names0 <- tryCatch(do.call(test_fun, tmp_args),
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
#' @param x The object
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
#' @param fit_to_all_args A named list
#' of arguments to be passed to
#' [lavaan::sem()] when the model is
#' fitted to a sample combined from
#  all samples stored.
#'
#' @param ... Optional arguments to
#' be passed to other print methods
#'
#' @return
#' The `print` method of `power4test`
#' returns `x` invisibly. Called for
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
                             fit_to_all_args = list(),
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
