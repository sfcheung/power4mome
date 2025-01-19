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
#' @export
#'
fit_model_i <- function(data_i,
                        ...) {
  fit <- lavaan::sem(model = data_i$model_final,
                     data = data_i$mm_lm_dat_out,
                     ...)
  return(fit)
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
#' @export
#'
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
  return(out)
}
