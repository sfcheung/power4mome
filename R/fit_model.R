
#' @title Fit a Model to a List of Datasets
#'
#' @description Get the output of
#' [sim_data()] and fit a model to each
#' of the stored datasets.
#'
#' @details
#' By default, the function [fit_model()]
#'
#' - extracts the model
#' stored in the output of [sim_data()],
#'
#' - fits the model to each dataset
#' simulated using `fit_function`,
#' default to `"lavaan"` and
#' [lavaan::sem()] will be called,
#'
#' - and returns the results.
#'
#' If the datasets
#' were generated from a multigroup
#' model when calling [sim_data()],
#' a multigroup model is fitted.
#'
#' # The role of `fit_model()`
#'
#' This function is used by the
#' all-in-one function [power4test()].
#' Users usually do not call this
#' function directly, though
#' developers can use this function to
#' customize the model fitting step in
#' power analysis.
#'
#' @seealso See [power4test()] for
#' the all-in-one function that uses
#' this function, and [sim_data()]
#' for the function generating datasets
#' for this function.
#'
#' @return
#' An object of the class `fit_out`,
#' which is a list of the output of
#' `fit_function` ([lavaan::sem()]
#' by default). If an error occurred
#' when fitting the model to a dataset,
#' then this element will be the error
#' message from the fit function.
#'
#' @param data_all The output
#' of [sim_data()], or a `sim_data`
#' class object.
#'
#' @param model The model to be fitted.
#' If `NULL`, the default, the model
#' stored in `data_all`, which should
#' be the data generation model,
#' will be used.
#'
#' @param fit_function The function to
#' be used to fit the model. Can also
#' be a string: `"lavaan"` (the default) for
#' [lavaan::sem()], and `"lm"` or `many_lm`
#' for [lmhelprs::many_lm()].
#' Other functions can also be used
#' if necessary.
#'
#' @param arg_data_name The name of the
#' argument of `fit_function` expecting
#' the dataset. Default is `"data"`.
#'
#' @param arg_model_name The name of
#' the argument of `fit_function`
#' expecting the model definition.
#' Default is `"model"`.
#'
#' @param arg_group_name The name of
#' the argument of `fit_function`
#' expecting the name of the group
#' variable. Used only for multigroup
#' models. Default is `"group"`.
#'
#' @param ... Optional arguments to be
#' passed to `fit_function` when
#' fitting the model.
#'
#' @param fit_out If set to a `fit_out`
#' object (the output of [fit_model()]),
#' then all missing arguments will be
#' retrieved from `fit_out`. That is,
#' users can use `fit_model(data_all = new_data, fit_out = old_out)`
#' to re-fit a model originally fitted
#' in `old_out` on a new list of dataset
#' (`new_data`). No need to include
#' all other arguments.
#'
#' @param parallel If `TRUE`, parallel
#' processing will be used to fit the
#' models. Default is `FALSE`.
#'
#' @param progress If `TRUE`, the progress
#' of model fitting will be displayed.
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
#' "m ~ x
#'  y ~ m + x"
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
#' fit_all[[1]]
#'
#' # Fit the population model using the MLR estimator
#'
#' fit_all_mlr <- fit_model(data_all,
#'                          estimator = "MLR")
#' fit_all_mlr[[1]]
#'
#' # Fit a model different from the population model,
#' # with the MLR estimator
#'
#' mod2 <-
#' "m ~ x
#'  y ~ m"
#'
#' fit_all_mlr2 <- fit_model(data_all,
#'                           mod2,
#'                           estimator = "MLR")
#' fit_all_mlr2[[1]]
#'
#' @export
fit_model <- function(data_all = NULL,
                      model = NULL,
                      fit_function = "lavaan",
                      arg_data_name = "data",
                      arg_model_name = "model",
                      arg_group_name = "group",
                      ...,
                      fit_out = NULL,
                      parallel = FALSE,
                      progress = FALSE,
                      ncores = max(1, parallel::detectCores(logical = FALSE) - 1),
                      cl = NULL) {
  # Store the arguments such the
  # it can be updated with new data.
  # It is intentional not to store the call.

  # This check can be removed after lmhelprs is updated on CRAN.
  # The version requirement should be added to DESCRIPTION later.
  tmp <- deparse(substitute(fit_function))
  if (isTRUE(grepl("lmhelprs", tmp)) || identical("lm", tmp)) {
    lmhelprs_supported <- (utils::packageVersion("lmhelprs") >= "0.4.2")
    if (isFALSE(lmhelprs_supported)) {
      stop("lmhelprs 0.4.2 or later is required. ",
          "If not available from CRAN, can be installed from ",
          "GitHub: remotes::install_github('sfcheung/lmhelprs')")
    }
  }

  update_fit <- FALSE
  if (!is.null(fit_out)) {
    if (inherits(fit_out, "fit_out")) {
      update_fit <- TRUE
    } else {
      stop("'fit_out' is not a supported object.")
    }
  }
  if (!update_fit) {
    # Store the evaluated arguments
    args <- formals(fit_model)
    args$`...` <- NULL
    args <- lapply(args,
                   eval,
                   envir = parent.frame())
  } else {
    args <- attr(fit_out, "args")
  }
  my_call <- match.call()
  call_args <- as.list(my_call)[-1]
  i <- which(names(call_args) == "data_all")
  if (length(i) != 0) {
    call_args <- call_args[-i]
  }
  call_args <- lapply(call_args,
                      eval,
                      envir = parent.frame())
  args <- utils::modifyList(args,
                            as.list(call_args))
  args$fit_out <- NULL
  args$data_all <- NULL
  # args available in all cases.
  # It should be used whenever possible,
  # unless we explicitly need the value in this call.
  # Parallel *will* be used, though
  # specified by args.
  out <- do.call(do_FUN,
                 c(list(X = data_all,
                        FUN = fit_model_i),
                   args))
  class(out) <- c("fit_out", class(out))
  attr(out, "args") <- args
  return(out)
}


#' @noRd
fit_model_i <- function(data_i,
                        model = NULL,
                        fit_function = "lavaan",
                        arg_data_name = "data",
                        arg_model_name = "model",
                        arg_group_name = "group",
                        ...) {
  if (is.character(fit_function)) {
    fit_function_org <- fit_function
    fit_function <- switch(fit_function,
                           lavaan = lavaan::sem,
                           lm = lmhelprs::many_lm,
                           many_lm = lmhelprs::many_lm,
                           fit_function)

  } else {
    fit_function_org <- character(0)
  }
  # Anomalies should be checked in
  # subsequent steps, not during fitting
  # the model to many datasets.
  # TODO:
  # - Update the following lines to
  #   support using a parameter table.
  if (is.null(model)) {
    model_to_fit <- data_i$model_final
  } else {
    model_to_fit <- model
  }
  tmp <- attr(model_to_fit, "ptable")
  if (!is.null(tmp) &&
      is.null(data_i$number_of_indicators[[1]]) &&
      identical(getNamespaceName(environment(fit_function)),
                c(name = "lavaan"))) {
    # Do this only for lavaan
    # TODO:
    # - Support moderated mediation model with indicators.
    model_to_fit <- tmp
  }

  # Fix the model if lm() is used to fitting the model
  if (fit_function_org %in% c("lm", "many_lm")) {
    model_to_fit <- fix_many_lm_model(model_to_fit)
  }

  # For single-group models,
  # data_i$group_name would be NULL.
  fit_args0 <- list()
  fit_args0[[arg_model_name]] <- model_to_fit
  fit_args0[[arg_data_name]] <- data_i$mm_lm_dat_out
  fit_args0[[arg_group_name]] = data_i$group_name
  fit_args <- utils::modifyList(list(...),
                                fit_args0)
  fit <- tryCatch(suppressWarnings(do.call(fit_function,
                                           fit_args)),
                  error = function(e) e)
  if (inherits(fit, "lavaan")) {
    fit@external$fit_external <- data_i$fit_external
  } else {
    try({attr(fit, "fit_external") <- data_i$fit_external},
        silent = TRUE)
  }
  return(fit)
}
