#' @title Plot The Results of 'x_from_power'
#'
#' @description It plots the results
#' of 'x_from_power', such as the
#' estimated power against sample size.
#'
#' @details
#' It currently plots the relation
#' between estimated power and
#' the values examined by [x_from_power()].
#' Other elements
#' can be requested (see the argument
#' `what`), and they can be customized
#' individually.
#'
#' @return
#' The `plot`-method of `x_from_power`
#' returns `x` invisibly.
#' It is called for its side effect.
#'
#' @param x An `x_from_power` object,
#' the output of [x_from_power()].
#'
#' @param what A character vector of
#' what to include in the
#' plot. Possible values are
#' `"ci"` (confidence intervals
#' for the estimated value of the
#' predictor),
#' `"power_curve"` (the crude power
#' curve, if available), `"final_x"`
#' (a vertical line for
#' the value of the predictor with estimated
#' power close enough to the target
#' power by confidence interval),
#' `"final_power"` (a horizontal line
#' for the estimated power of the
#' final value of the predictor), and
#' `"target_power"` (a horizontal
#' line for the target power).
#' By default, all these elements will
#' be plotted.
#'
#' @param text_what A character vector
#' of what numbers to be added as
#' labels. Possible values are
#' `"final_x"` (the value of the
#' predictor with
#' estimated power close enough to
#' the target power by confidence interval)
#' and `"final_power"` (the estimated
#' power of the final value of the
#' predictor).
#' By default, all these labels will
#' be added.
#'
#' @param digits The number of digits
#' after the decimal that will be used
#' when adding numbers.
#'
#' @param main The title of the plot.
#'
#' @param xlab,ylab The labels for the
#' horizontal and vertical axes,
#' respectively.
#'
#' @param pars_ci A named list of
#' arguments to be passed to [arrows()]
#' to customize the drawing of the
#' confidence intervals.
#'
#' @param pars_power_curve A named list of
#' arguments to be passed to [points()]
#' to customize the drawing of the
#' power curve.
#'
#' @param pars_ci_final_x A named list of
#' arguments to be passed to [arrows()]
#' to customize the drawing of the
#' confidence interval of the final
#' value of the predictor.
#'
#' @param pars_target_power A named list
#' of arguments to be passed to [abline()]
#' when drawing the horizontal line
#' for the target power.
#'
#' @param pars_final_x A
#' named list of arguments to be passed
#' to [abline()] when drawing the
#' vertical line for the final value
#' of the predictor.
#'
#' @param pars_final_power A
#' named list of arguments to be passed
#' to [abline()] when drawing the
#' horizontal line for the estimated
#' power at the final value of the
#' predictor.
#'
#' @param pars_text_final_x A
#' named list of arguments to be passed
#' to [text()] when adding the
#' label for the final value of the
#' predictor.
#'
#' @param pars_text_final_power A
#' named list of arguments to be passed
#' to [text()] when adding the
#' label for the estimated power
#' of final value of the predictor.
#'
#' @param ... Optional arguments.
#' Passed to [plot()] when drawing
#' the estimated power against the
#' predictor.
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
#' sim_only <- power4test(nrep = 10,
#'                        model = mod,
#'                        pop_es = mod_es,
#'                        n = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
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
#'                            final_nrep = 10,
#'                            xs_per_trial = 1,
#'                            nrep_steps = 1,
#'                            max_trials = 1,
#'                            seed = 2345)
#' plot(power_vs_n)
#'
#' @importFrom graphics abline arrows par points text title
#'
#' @export
plot.x_from_power <- function(x,
                              what = c("ci", "power_curve", "final_x", "final_power", "target_power"),
                              text_what = c("final_x", "final_power"),
                              digits = 3,
                              main = paste0("Power Curve ",
                                            "(Target Power: ",
                                            formatC(x$target_power, digits = digits, format = "f"),
                                            ")"),
                              xlab = NULL,
                              ylab = "Estimated Power",
                              pars_ci = list(),
                              pars_power_curve = list(),
                              pars_ci_final_x = list(lwd = 2,
                                                               length = .2,
                                                               col = "blue"),
                              pars_target_power = list(lty = "dotted"),
                              pars_final_x = list(lty = "dotted"),
                              pars_final_power = list(lty = "dotted", col = "blue"),
                              pars_text_final_x = list(y = 0, pos = 3, cex = 1),
                              pars_text_final_power = list(pos = 3, cex = 1),
                              ...) {

  what <- match.arg(what, several.ok = TRUE)
  text_what <- match.arg(text_what, several.ok = TRUE)
  predictor <- x$x

  # Set xlab

  if (is.null(xlab)) {
    if (predictor == "n") {
      xlab <- "Sample Size"
    }
    if (predictor == "es") {
      xlab <- paste0("Parameter: ",
                     x$pop_es_name)
    }
  }

  # Was a solution found?

  solution_found <- isFALSE(identical(NA, x$x_final))

  # === Draw the base plot: Power vs. Predictor

  # It is intended *not* to use plot.power_curve().
  # It is possible that the fit failed.

  do.call(plot_power_x,
          list(object = x$power4test_trials,
               predictor = predictor,
               main = main,
               xlab = xlab,
               ylab = ylab,
               ...))

  # === Add CIs?

  if ("ci" %in% what) {

    if (solution_found && ("final_x" %in% what)) {
      # A solution was found and final_x line is to be drawn.
      # Draw the final_x CI separately

      # Draw the CI for the final x
      tmp <- x$power4test_trials[x$i_final]
      class(tmp) <- class(x$power4test_trials)
      tmp_args <- utils::modifyList(pars_ci_final_x,
                                    list(object = tmp,
                                         predictor = predictor))
      do.call(plot_power_x_ci,
              tmp_args)
      # Drop final_n from the CI lists
      tmp_for_ci <- x$power4test_trials[-x$i_final]
      class(tmp_for_ci) <- class(x$power4test_trials)
    } else {
      # Keep all CIs in the same way
      tmp_for_ci <- x$power4test_trials
    }

    # Draw the other CIs
    tmp_args <- utils::modifyList(pars_ci,
                                  list(object = tmp_for_ci,
                                       predictor = predictor))
    do.call(plot_power_x_ci,
            tmp_args)
  }

  # === Draw the power curve?

  # It is intentional *not* to use the power curve plot method.

  if ("power_curve" %in% what) {
    tmp_args <- utils::modifyList(pars_power_curve,
                                  list(object = x$power4test_trials,
                                       predictor = predictor,
                                       power_x_fit = x$power_curve))
    do.call(plot_power_curve_x,
            tmp_args)
  }

  # === Draw a horizontal line for the target power?

  if ("target_power" %in% what) {
    tmp_args <- utils::modifyList(pars_target_power,
                                  list(h = x$target_power))
    do.call(abline,
            tmp_args)
  }

  if (solution_found) {

    # === Draw a vertical line for the final N?

    if ("final_x" %in% what) {
      tmp_args <- utils::modifyList(pars_final_x,
                                    list(v = x$x_final))
      do.call(abline,
              tmp_args)
    }

    # === Draw a horizontal line for the final power?

    if ("final_power" %in% what) {
      tmp_args <- utils::modifyList(pars_final_power,
                                    list(h = x$power_final))
      do.call(abline,
              tmp_args)
    }

    # === Add a label for the final x?

    if ("final_x" %in% text_what) {
      x_final_str <- formatC(x$x_final,
                             digits = switch(predictor,
                                             n = 0,
                                             es = digits),
                             format = "f")
      tmp_args <- utils::modifyList(pars_text_final_x,
                                    list(x = x$x_final,
                                         labels = x_final_str))
      do.call(text,
              tmp_args)
    }

    # === Add a label for the final power?

    if ("final_power" %in% text_what) {
      tmp <- par("usr")
      tmp_args <- utils::modifyList(pars_text_final_power,
                                    list(y = x$power_final,
                                         x = tmp[1] + (tmp[2] - tmp[1]) * .05,
                                         labels = formatC(x$power_final,
                                                          digits = digits,
                                                          format = "f")))
      if (!is.null(pars_text_final_power$x)) {
        tmp_args <- utils::modifyList(tmp_args,
                                      list(x = pars_text_final_power$x))
      }
      do.call(text,
              tmp_args)
    }

  }

  invisible(x)
}

#' @noRd
plot_power_x <- function(object,
                         predictor,
                         type = "l",
                         ylim = c(0, 1),
                         ...) {
  reject0 <- rejection_rates_add_ci(object)
  x <- switch(predictor,
              n = "n",
              es = "es")
  reject0$x <- reject0[[x]]
  reject0$power <- reject0$reject
  plot(power ~ x,
       data = reject0,
       type = type,
       ylim = ylim,
       ...)
}

#' @noRd
plot_power_x_ci <- function(object,
                            predictor,
                            length = .1,
                            angle = 90,
                            code = 3,
                            col = "grey50",
                            ...) {
  reject0 <- rejection_rates_add_ci(object)
  x <- switch(predictor,
              n = "n",
              es = "es")
  reject0$x <- reject0[[x]]
  reject0$power <- reject0$reject

  # Some CIs may be of zero width
  i <- !(reject0$reject_ci_lo == reject0$reject_ci_hi)
  if (any(i)) {
    arrows(x0 = reject0$x[i],
           y0 = reject0$reject_ci_lo[i],
           x1 = reject0$x[i],
           y1 = reject0$reject_ci_hi[i],
           length = length,
           angle = angle,
           code = code,
           col = col,
           ...)
  }
}

#' @noRd

#' @noRd

plot_power_curve_x <- function(object,
                               predictor,
                               power_x_fit,
                               type = "l",
                               lwd = 2,
                               col = "red",
                               length_of_new_x = 20,
                               ...) {
  # power_x_fit is a power_curve object
  reject0 <- rejection_rates_add_ci(object)
  x <- switch(predictor,
              n = "n",
              es = "es")
  reject0$x <- reject0[[x]]
  reject0$power <- reject0$reject
  x_new <- seq(min(reject0$x),
               max(reject0$x),
               length.out = length_of_new_x)
  if (inherits(power_x_fit$fit, "nls") || inherits(power_x_fit$fit, "lm")) {
    y_new <- stats::predict(power_x_fit,
                            newdata = list(x = x_new))
    points(x = x_new,
           y = y_new,
           type = type,
           lwd = lwd,
           col = col,
           ...)
  }
}
