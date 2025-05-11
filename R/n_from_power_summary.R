#' @title Summarize 'n_from_power' Results
#'
#' @description The summary method of
#' the output of [n_from_power()].
#'
#' @details It simply prepare the
#' results of [n_from_power()]
#' to be printed in details.
#'
#' @return
#' Return an object of the class
#' `summary.n_from_power`, which is
#' simply the output of [n_from_power()],
#' with a `print` method dedicated for
#' detailed summary.
#'
#' @param object An `n_from_power`-class
#' object, such as the output of
#' [n_from_power()].
#'
#' @param ... Additional arguments.
#' Not used for now.
#'
#' @seealso [n_from_power()]
#'
#' @examples
#'
#' # TO PREPARE
#' x <- 1
#' \donttest{
#' }
#'
#' @export
summary.n_from_power <- function(object,
                                 ...) {
  class(object) <- "summary.n_from_power"
  return(object)
}

#' @rdname summary.n_from_power
#'
#' @param x The output of
#' [summary.n_from_power()], the
#' `summary` method of
#' an `n_from_power` object,
#' which is the output of
#' [n_from_power()].
#'
#' @param digits The number of digits
#' after the decimal when printing
#' the results.
#'
#' @return
#' The `print`-method of `summary.n_from_power`
#' objects returns the object `x`
#' invisibly.
#' It is called for its side effect.
#'
#' @export
print.summary.n_from_power <- function(x,
                                       digits = 3,
                                       ...) {

  cat("\n====== n_from_power Results ======\n\n")
  my_call <- x$call
  cat("Call:\n")
  print(my_call)
  solution_found <- !is.na(x$n_final)

  cat("\n- Target Power:",
      formatC(x$target_power, digits = digits, format = "f"),
      "\n")

  cat("\n=== Major Results ===\n\n")
  if (solution_found) {
    cat("- Final Sample Size:", x$n_final, "\n")
    cat("- Final Estimated Power:",
        formatC(x$power_final, digits = digits, format = "f"),
        "\n")
    cat("- Confidence Interval: [",
        paste0(formatC(x$ci_final, digits = digits, format = "f"), collapse = "; "),
        "]\n", sep = "")
    cat("- Level of confidence: ",
        formatC(x$ci_level*100, digits = max(0, digits - 2), format = "f"), "%", "\n", sep = "")
    cat("- Based on", x$nrep_final, "replications.\n")
  } else {
    cat("- Solution not found.\n")
    catwrap(paste(c("- None of the sample sizes examined",
                    "in the interval meet the target power."),
                    collapse = " "),
            exdent = 2)
    if (isFALSE(identical(NA, x$n_estimated))) {
      cat("- The crude estimate of required sample size is ",
          x$n_estimated,
          ".\n", sep = "")
      cat("- Note: Estimated by the power curve.\n")
    }
    catwrap(paste(c("- Try changing the settings, such as",
                    "expanding the range of sample sizes",
                    "by setting 'n_interval' to one that",
                    "includes the crude estimate, if",
                    "available."),
                    collapse = " "),
            exdent = 2)
  }

  cat("\n=== Technical Information ===\n\n")
  cat("- The range of sample sizes explored:",
      paste(range(x$n_tried), collapse = " to "), "\n")
  cat("- Time spent in the search:",
      format(x$time_spent, digits = 4),
      "\n")
  if (isFALSE(identical(NA, x$power_curve))) {
    tmp <- class(x$power_curve)[1]
    power_curve_name <- switch(tmp,
                               nls = "Nonlinear Regression Model",
                               glm = "Logistic Regression",
                               lm = "Linear Regression")
    cat("- The final crude model for the power-sample-size relation:\n")
    cat("\nModel Type:",
        power_curve_name,
        "\n\n")
    tmp1 <- x$power_curve
    if (identical(tmp, "nls")) {
      tmp1$data <- "(Internal)"
    }
    print(tmp1)
    cat("\n")
  }
  cat("- Detailed Results:\n\n")
  print(x$rejection_rates,
        digits = 3)
  cat("\n")

  invisible(x)
}