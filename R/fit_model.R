
#' @title Fit a Model to a List of Datasets
#'
#' @description Get the output of
#' [sim_data()] and fit a model to each
#' of the stored dataset.
#'
#' @details
#' By default, it extracts the model
#' stored in the output of [sim_data()],
#' fits the model to each dataset
#' simulated using [lavaan::sem()], and
#' returns the results.
#'
#' @return
#' An object of the class `fit_out`,
#' which is a list of the output of
#' [lavaan::sem()].
#'
#' @param data_all The output
#' of [sim_data()], or a `sim_data`
#' class object.
#'
#' @param ... Optional arguments to be
#' passed to [lavaan::sem()] when
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
                      ...,
                      parallel = FALSE,
                      progress = FALSE,
                      ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = data_all,
                FUN = fit_model_i,
                ...,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  class(out) <- c("fit_out", class(out))
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
fit_model_i <- function(data_i,
                        ...) {
  fit <- lavaan::sem(model = data_i$model_final,
                     data = data_i$mm_lm_dat_out,
                     ...)
  return(fit)
}
