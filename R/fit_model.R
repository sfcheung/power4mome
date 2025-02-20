
#' @title Fit a Model to a List of Datasets
#'
#' @description Get the output of
#' [sim_data()] and fit a model to each
#' of the stored datasets.
#'
#' @details
#' By default, it extracts the model
#' stored in the output of [sim_data()],
#' fits the model to each dataset
#' simulated using `fit_function`,
#' default to `"lavaan"` and
#' [lavaan::sem()] will be called, and
#' returns the results. If the datasets
#' were generated from a multigroup
#' model when calling [sim_data()],
#' a multigroup model is fitted.
#'
#' This function is used by the
#' all-in-one function [power4test()].
#' Users usually do not call this
#' function directly.
#'
#' @seealso [power4test()]
#'
#' @return
#' An object of the class `fit_out`,
#' which is a list of the output of
#' `fit_function` ([lavaan::sem()]
#' by default). If an error occurred
#' when fitting the model to a dataset,
#' then element will be the error
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
#' fit_all[[1]]
#'
#' @export
fit_model <- function(data_all,
                      model = NULL,
                      fit_function = "lavaan",
                      arg_data_name = "data",
                      arg_model_name = "model",
                      arg_group_name = "group",
                      ...,
                      parallel = FALSE,
                      progress = FALSE,
                      ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = data_all,
                FUN = fit_model_i,
                model = model,
                fit_function = fit_function,
                arg_data_name = arg_data_name,
                arg_model_name = arg_model_name,
                arg_group_name = arg_group_name,
                ...,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  class(out) <- c("fit_out", class(out))
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
    fit_function <- switch(fit_function,
                           lavaan = lavaan::sem,
                           lm = lmhelprs::many_lm,
                           many_lm = lmhelprs::many_lm,
                           fit_function)

  }
  # Anomalies should be checked in
  # subsequent steps, not during fitting
  # the model to many datasets.
  if (is.null(model)) {
    model_to_fit <- data_i$model_final
  } else {
    model_to_fit <- model
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
  return(fit)
}
