#' @title Summarize 'x_from_power' Results
#'
#' @description The summary method of
#' the output of [x_from_power()].
#'
#' @details It simply prepares the
#' results of [x_from_power()]
#' to be printed in details.
#'
#' @return
#' It returns an object of the class
#' `summary.x_from_power`, which is
#' simply the output of [x_from_power()],
#' with a `print` method dedicated for
#' detailed summary. Please refer
#' to [x_from_power()] for the contents.
#'
#' @param object An `x_from_power`-class
#' object, such as the output of
#' [x_from_power()].
#'
#' @param ... Additional arguments.
#' Not used for now.
#'
#' @seealso [x_from_power()]
#'
#' @examples
#'
#' # Specify the population model
#'
#' mod <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' # Specify the population values
#'
#' mod_es <-
#' "
#' m ~ x: m
#' y ~ m: l
#' y ~ x: n
#' "
#'
#' # Generate the datasets
#'
#' sim_only <- power4test(nrep = 5,
#'                        model = mod,
#'                        pop_es = mod_es,
#'                        n = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 2345)
#'
#' # Do a test
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_parameters,
#'                        test_args = list(pars = "m~x"))
#'
#' # Determine the sample size with a power of .80 (default)
#'
#' power_vs_n <- x_from_power(test_out,
#'                            x = "n",
#'                            progress = TRUE,
#'                            target_power = .80,
#'                            final_nrep = 5,
#'                            max_trials = 1,
#'                            seed = 1234)
#' summary(power_vs_n)
#'
#' @export
summary.x_from_power <- function(object,
                                 ...) {
  class(object) <- "summary.x_from_power"
  return(object)
}

#' @rdname summary.x_from_power
#'
#' @param x The output of
#' [summary.x_from_power()], the
#' `summary` method of
#' an `x_from_power` object,
#' which is the output of
#' [x_from_power()].
#'
#' @param digits The number of digits
#' after the decimal when printing
#' the results.
#'
#' @return
#' The `print`-method of `summary.x_from_power`
#' objects returns the object `x`
#' invisibly.
#' It is called for its side effect.
#'
#' @export
print.summary.x_from_power <- function(x,
                                       digits = 3,
                                       ...) {

  cat("\n====== x_from_power Results ======\n\n")
  my_call <- x$call
  cat("Call:\n")
  print(my_call)
  solution_found <- !is.na(x$x_final)

  predictor <- x$x
  cat("\n")
  cat("Predictor (x):",
      switch(predictor,
             n = "Sample Size",
             es = "Effect Size"),
      "\n")
  if (predictor == "es") {
    cat("Parameter Name (pop_es_name):",
        x$pop_es_name,
        "\n")
  }

  target_power_str <- formatC(x$target_power, digits = digits, format = "f")
  cat("\n- Target Power:",
      target_power_str,
      "\n")

  if (x$goal == "close_enough") {
    tmp1 <- switch(x$what,
                   point = "estimated power",
                   lb = "lower confidence bound",
                   ub = "upper confidence bound")
    tmp2 <- paste0(
        "- Goal: ",
        "Find 'x' with estimated ",
        tmp1,
        " close enough to the target power.")
    catwrap(tmp2,
            exdent = 2)
  }

  if (x$goal == "ci_hit") {
    tmp2 <- paste0(
        "- Goal: ",
        "Find 'x' with the confidence interval of ",
        "the estimated power enclosing the ",
        "target power.")
    catwrap(tmp2,
            exdent = 2)
  }

  cat("\n=== Major Results ===\n\n")
  if (solution_found) {
    x_final_str <- formatC(x$x_final,
                           digits = switch(predictor,
                                           n = 0,
                                           es = digits),
                           format = "f")
    tmp <- switch(x$x,
                  n = "(Sample Size)",
                  es = paste0("(", x$pop_es_name, ")"))
    cat("- Final Value ", tmp, ": ", x_final_str, "\n\n", sep = "")
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
    catwrap(paste(c("- None of the values examined",
                    "in the interval meet the target power."),
                    collapse = " "),
            exdent = 2)
    if (isFALSE(identical(NA, x$x_estimated))) {
      x_estimated_str <- formatC(x$x_estimated,
                                 digits = switch(predictor,
                                                 n = 0,
                                                 es = digits),
                                 format = "f")
      cat("- The crude estimate of required value is ",
          x_estimated_str,
          ".\n", sep = "")
      cat("- Note: Estimated by the power curve.\n")
    }
    catwrap(paste(c("- Try changing the settings, such as",
                    "expanding the range of values",
                    "by setting 'x_interval' to one that",
                    "includes the crude estimate, if",
                    "available, increasing 'max_trials',",
                    "or increasing 'tolerance' if the goal",
                    "is 'close_enough'."),
                    collapse = " "),
            exdent = 2)
  }

  cat("\n=== Technical Information ===\n\n")
  cat("- Algorithm:",
      x$algorithm,
      "\n")
  if (x$goal == "close_enough") {
    cat("- Tolerance for 'close enough':",
      "Within",
      formatC(x$technical$tol,
              digits + 2,
              format = "f"),
      "of",
      target_power_str,
      "\n")
  }
  tmp0 <- switch(x$x,
                 n = ceiling(x$x_tried),
                 es = x$x_tried)
  tmp <- formatC(tmp0,
                  digits = switch(x$x,
                                  n = 0,
                                  es = digits),
                  format = "f")
  cat("- The range of values explored:",
      paste(range(tmp), collapse = " to "), "\n")
  cat("- Time spent in the search:",
      format(x$time_spent, digits = 4),
      "\n")
  if (isFALSE(identical(NA, x$power_curve))) {
    tmp <- class(x$power_curve$fit)[1]
    power_curve_name <- switch(tmp,
                               nls = "Nonlinear Regression Model",
                               glm = "Logistic Regression",
                               lm = "Linear Regression")
    cat("- The final crude model for the power-predictor relation:\n")
    cat("\nModel Type:",
        power_curve_name,
        "\n\n")
    tmp1 <- x$power_curve
    # Use the print method of power_curve objects
    print(tmp1)
    cat("\n")
  }
  cat("- Detailed Results:\n\n")
  print(x$rejection_rates,
        digits = 3)
  cat("\n")

  invisible(x)
}