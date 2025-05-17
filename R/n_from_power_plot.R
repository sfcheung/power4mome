#' @title Plot The Results of 'n_from_power'
#'
#' @description It plot the results
#' of 'n_from_power', such as the
#' estimated power against sample size.
#'
#' @details
#' It currently plots the relation
#' between estimated power and
#' sample size. Other elements
#' can be requested (see the argument
#' `what`), and they can be customized
#' individually.
#'
#' @return
#' The `plot`-method of `n_from_power`
#' is called for its side effect.
#'
#' @param x A `n_from_power` object,
#' the output of [n_from_power()].
#'
#' @param what A character vector of
#' what to include in the
#' plot. Possible values are
#' `"ci"` (confidence intervals
#' for the estimated sample size),
#' `"power_curve"` (the crude power
#' curve, if available), `"final_n"`
#' (a vertical line for
#' the sample size with estimated
#' power close enough to the target
#' power by confidence interval),
#' `"final_power"` (a horizontal line
#' for the estimated power of the
#' final sample size), and
#' `"target_power"` (a horizontal
#' line for the target power).
#' By default, all these elements will
#' be plotted.
#'
#' @param text_what A character vector
#' of what numbers to be added as
#' labels. Possible values are
#' `"final_n"` (the sample size with
#' estimated power close enough to
#' the target power by confidence interval)
#' and `"final_power"` (the estimated
#' power of the final sample sizes).
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
#' @param pars_ci_final_sample_size A named list of
#' arguments to be passed to [arrows()]
#' to customize the drawing of the
#' confidence interval of the final
#' sample size.
#'
#' @param pars_target_power A named list
#' of arguments to be passed to [abline()]
#' when drawing the horizontal line
#' for the target power.
#'
#' @param pars_final_sample_size A
#' named list of arguments to be passed
#' to [abline()] when drawing the
#' vertical line for the final sample
#' size.
#'
#' @param pars_final_power A
#' named list of arguments to be passed
#' to [abline()] when drawing the
#' horizontal line for the estimated
#' power at the final sample size.
#'
#' @param pars_text_final_sample_size A
#' named list of arguments to be passed
#' to [text()] when adding the
#' label for the final sample size.
#'
#' @param pars_text_final_power A
#' named list of arguments to be passed
#' to [text()] when adding the
#' label for the estimated power
#' of final sample size.
#'
#' @param ... Optional arguments.
#' Passed to [plot()] when drawing
#' the estimated power against the
#' sample sizes.
#'
#' @seealso [n_from_power()]
#'
#' @examples
#'
#' mod <-
#' "
#' m ~ a*x
#' y ~ b*m + x
#' ab := a * b
#' "
#'
#' mod_es <- c("y ~ m" = "l",
#'             "m ~ x" = "m",
#'             "y ~ x" = "n")
#'
#' sim_only <- power4test(nrep = 10,
#'                        model = mod,
#'                        pop_es = mod_es,
#'                        n = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_parameters,
#'                        test_args = list(pars = "ab"))
#'
#' # In real analysis, to have more stable results:
#' # - Use a larger final_nrep (e.g., 500).
#' # - Use the default ns_per_trial of 3, or just remove it.
#'
#' # If the default values are OK, this call is sufficient:
#' # power_vs_n <- n_from_power(test_out,
#' #                            target_power = .80,
#' #                            seed = 4567)
#' power_vs_n <- n_from_power(test_out,
#'                            progress = TRUE,
#'                            target_power = .80,
#'                            final_nrep = 10,
#'                            ns_per_trial = 1,
#'                            nrep_steps = 1,
#'                            max_trials = 1,
#'                            seed = 4567)
#' summary(power_vs_n)
#' plot(power_vs_n)
#'
#' @importFrom graphics abline arrows par points text title
#'
#' @export
plot.n_from_power <- function(x,
                              what = c("ci", "power_curve", "final_n", "final_power", "target_power"),
                              text_what = c("final_n", "final_power"),
                              digits = 3,
                              main = paste0("Power Curve ",
                                            "(Target Power: ",
                                            formatC(x$target_power, digits = digits, format = "f"),
                                            ")"),
                              xlab = "Sample Size",
                              ylab = "Estimated Power",
                              pars_ci = list(),
                              pars_power_curve = list(),
                              pars_ci_final_sample_size = list(lwd = 2,
                                                               length = .2,
                                                               col = "blue"),
                              pars_target_power = list(lty = "dotted"),
                              pars_final_sample_size = list(lty = "dotted"),
                              pars_final_power = list(lty = "dotted", col = "blue"),
                              pars_text_final_sample_size = list(y = 0, pos = 3, cex = 1),
                              pars_text_final_power = list(pos = 3, cex = 1),
                              ...) {

  what <- match.arg(what, several.ok = TRUE)
  text_what <- match.arg(what, several.ok = TRUE)

  # Was a solution found?

  solution_found <- isFALSE(identical(NA, x$n_final))

  # === Draw the base plot: Power vs. N

  # It is intended *not* to use plot.power_curve().
  # It is possible that the fit failed.

  do.call(plot_power_n,
          list(object = x$power4test_trials,
               main = main,
               xlab = xlab,
               ylab = ylab,
               ...))

  # === Add CIs?

  if ("ci" %in% what) {

    if (solution_found && ("final_n" %in% what)) {
      # A solution was found and final_n line is to be drawn.
      # Draw the final_n CI separately

      # Draw the CI for the final N
      tmp <- x$power4test_trials[x$i_final]
      tmp_args <- utils::modifyList(pars_ci_final_sample_size,
                                    list(object = tmp))
      do.call(plot_power_n_ci,
              tmp_args)
      # Drop final_n from the CI lists
      tmp_for_ci <- x$power4test_trials[-x$i_final]
    } else {
      # Keep all CIs in the same way
      tmp_for_ci <- x$power4test_trials
    }

    # Draw the other CIs
    tmp_args <- utils::modifyList(pars_ci,
                                  list(object = tmp_for_ci))
    do.call(plot_power_n_ci,
            tmp_args)
  }

  # === Draw the power curve?

  # It is intentional *not* to use the power curve plot method.

  if ("power_curve" %in% what) {
    tmp_args <- utils::modifyList(pars_power_curve,
                                  list(object = x$power4test_trials,
                                       power_n_fit = x$power_curve))
    do.call(plot_power_curve,
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

    if ("final_n" %in% what) {
      tmp_args <- utils::modifyList(pars_final_sample_size,
                                    list(v = x$n_final))
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

    # === Add a label for the final N?

    if ("final_n" %in% text_what) {
      tmp_args <- utils::modifyList(pars_text_final_sample_size,
                                    list(x = x$n_final,
                                         labels = x$n_final))
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

plot_power_n <- function(object,
                         type = "l",
                         ylim = c(0, 1),
                         ...) {
  reject0 <- rejection_rates_add_ci(object)
  reject0$power <- reject0$reject
  plot(power ~ n,
       data = reject0,
       type = type,
       ylim = ylim,
       ...)
}


#' @noRd

plot_power_curve <- function(object,
                             power_n_fit,
                             type = "l",
                             lwd = 2,
                             col = "red",
                             ...) {
  reject0 <- rejection_rates_add_ci(object)
  reject0$power <- reject0$reject
  x_new <- seq(min(reject0$n),
               max(reject0$n),
               length.out = 20)
  if (inherits(power_n_fit$fit, "nls") || inherits(power_n_fit$fit, "lm")) {
    y_new <- stats::predict(power_n_fit,
                            newdata = list(x = x_new))
    points(x = x_new,
           y = y_new,
           type = type,
           lwd = lwd,
           col = col,
           ...)
  }
}

#' @noRd

plot_power_n_ci <- function(object,
                            length = .1,
                            angle = 90,
                            code = 3,
                            col = "grey50",
                            ...) {
  reject0 <- rejection_rates_add_ci(object)
  reject0$power <- reject0$reject
  # Some CIs may be of zero width
  i <- !(reject0$reject_ci_lo == reject0$reject_ci_hi)
  if (any(i)) {
    arrows(x0 = reject0$n[i],
           y0 = reject0$reject_ci_lo[i],
           x1 = reject0$n[i],
           y1 = reject0$reject_ci_hi[i],
           length = length,
           angle = angle,
           code = code,
           col = col,
           ...)
  }
}