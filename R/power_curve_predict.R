#' @title Predict Method for a 'power_curve' Object
#'
#' @description Compute the predicted
#' values in a model fitted by
#' [power_curve()].
#'
#' @details
#' It retrieve the stored results
#' and call the corresponding methods.
#'
#' @return
#' It returns a numeric vector of
#' the predicted rejection rates.
#'
#' @param object A `power_curve`
#' object.
#'
#' @param newdata A data frame with
#' a column named `x`. This is a
#' required argument.
#'
#' @param ...  Additional arguments.
#' Passed to the corresponding
#' `predict` method.
#'
#' @seealso [power_curve()].
#'
#' @examples
#' # TODO:
#' # - To write
#' x <- 1
#'
#' @export

predict.power_curve <- function(object,
                                newdata,
                                ...) {
  fit <- object$fit
  if (missing(newdata)) {
    newdata <- object$reject_df
  }
  if (inherits(fit, "glm")) {
    out <- stats::predict(object = fit,
                          newdata = newdata,
                          type = "response",
                          ...)
  } else {
    out <- stats::predict(object = fit,
                          newdata = newdata,
                          ...)
  }
  return(out)
}