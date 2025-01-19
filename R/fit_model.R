
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
#' An object of the class `fit_model`,
#' which is a list of the output of
#' [lavaan::sem()].
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
fit_model <- function(sim_data_all,
                      ...,
                      parallel = FALSE,
                      progress = FALSE,
                      ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = sim_data_all,
                FUN = fit_model_i,
                ...,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  class(out) <- c("fit_model", class(out))
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
