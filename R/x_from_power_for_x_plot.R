#' @title Plot The Results of 'x_from_power'
#'
#' @description It plots the results
#' of 'x_from_power', such as the
#' estimated power against sample size.
#'
#' @details
#' The `plot` method of `x_from_power`
#' objects currently plots the relation
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
#' final value of the predictor),
#' `"target_power"` (a horizontal
#' line for the target power),
#' and `"sig_area"` (the area significantly
#' higher or lower than the target
#' power, if `goal` is `"close_enough"`
#' and `what` is `"lb"` or `"ub"`).
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
#' `"final_power"` (the estimated
#' power of the final value of the
#' predictor), and
#' `"sig_area"` (labeling the area significantly
#' higher or lower than the target
#' power, if `goal` is `"close_enough"`
#' and `what` is `"lb"` or `"ub"`).
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
#' @param ci_level The level of
#' confidence of the confidence intervals,
#' if requested. Default is `.95`, denoting
#' 95%.
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
#' @param pars_sig_area A named list
#' of arguments to be passed to
#' [rect()] when shading the area
#' significantly higher or lower than
#' the target power.
#'
#' @param pars_text_sig_area A named list
#' of arguments to be passed to
#' [text()] when labelling the area
#' significantly higher or lower than
#' the target power.
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
#'                            max_trials = 1,
#'                            seed = 2345)
#' plot(power_vs_n)
#'
#' @importFrom graphics abline arrows par points text title rect
#' @importFrom grDevices adjustcolor

#'
#' @export
plot.x_from_power <- function(x,
                              what = c("ci", "power_curve", "final_x", "final_power", "target_power",
                                       switch(x$x, n = "sig_area", es = NULL)),
                              text_what = c("final_x", "final_power",
                                            switch(x$x, n = "sig_area", es = NULL)),
                              digits = 3,
                              main = paste0("Power Curve ",
                                            "(Target Power: ",
                                            formatC(x$target_power, digits = digits, format = "f"),
                                            ")"),
                              xlab = NULL,
                              ylab = "Estimated Power",
                              ci_level = .95,
                              pars_ci = list(),
                              pars_power_curve = list(),
                              pars_ci_final_x = list(lwd = 2,
                                                               length = .2,
                                                               col = "blue"),
                              pars_target_power = list(lty = "dashed",
                                                       lwd = 2,
                                                       col = "black"),
                              pars_final_x = list(lty = "dotted"),
                              pars_final_power = list(lty = "dotted", col = "blue"),
                              pars_text_final_x = list(y = 0, pos = 3, cex = 1),
                              pars_text_final_power = list(pos = 3, cex = 1),
                              pars_sig_area = list(col = adjustcolor("lightblue",
                                                   alpha.f = .1)),
                              pars_text_sig_area = list(cex = 1),
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

    if (("sig_area" %in% what) &&
        (x$goal == "close_enough")) {
      if (x$what == "lb") {
        tmp_args <- utils::modifyList(pars_sig_area,
                                      list(
                                        xleft = x$x_final,
                                        xright = par("usr")[2],
                                        ybottom = par("usr")[3],
                                        ytop = par("usr")[4],
                                        border = NULL)
                                      )
        do.call(rect,
                tmp_args)
      }
      if (x$what == "ub") {
        tmp_args <- utils::modifyList(pars_sig_area,
                                      list(
                                        xleft = par("usr")[1],
                                        xright = x$x_final,
                                        ybottom = par("usr")[3],
                                        ytop = par("usr")[4],
                                        border = NULL)
                                      )
        do.call(rect,
                tmp_args)
      }
    }

    if (("sig_area" %in% text_what) &&
        (x$goal == "close_enough")) {
      if (x$what == "lb") {
        tmp_str <- paste0("Power sig. >\n",
                           formatC(x$target_power,
                                   digits,
                                   format = "f"))
        tmp_args <- utils::modifyList(pars_text_sig_area,
                                      list(x = mean(c(x$x_final,
                                                      par("usr")[2])),
                                           y = mean(c(x$target_power,
                                                      par("usr")[3])),
                                           labels = tmp_str))
        do.call(text,
                tmp_args)
      }
      if (x$what == "ub") {
        tmp_str <- paste0("Power sig. <\n",
                           formatC(x$target_power,
                                   digits,
                                   format = "f"))
        tmp_args <- utils::modifyList(pars_text_sig_area,
                                      list(x = mean(c(x$x_final,
                                                      par("usr")[1])),
                                           y = mean(c(x$target_power,
                                                      par("usr")[3])),
                                           labels = tmp_str))
        do.call(text,
                tmp_args)
      }
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
                         ci_level = .95,
                         ...) {
  # No need for other arguments. They should be stored in object.
  reject0 <- rejection_rates_add_ci(object,
                                    level = ci_level)
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
                            ci_level = .95,
                            ...) {
  # No need for other arguments. They should be stored in object.
  reject0 <- rejection_rates_add_ci(object,
                                    level = ci_level)
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
                               ci_level = .95,
                               ...) {
  # power_x_fit is a power_curve object
  # No need for other arguments. They should be stored in object.
  reject0 <- rejection_rates_add_ci(object,
                                    level = ci_level)
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

#' @rdname plot.x_from_power
#'
#' @param pars_text_final_x_lower,pars_text_final_x_upper
#' If two values of the predictor are to
#' be printed, these are the named list
#' of the arguments to be passed to [text()]
#' when adding the labels for these two
#' values.
#'
#' @details
#' The `plot`-method for
#' `n_region_from_power` objects is
#' a modified version of the `plot`-method
#' for `x_from_power`. It plots the
#' results of two runs of [n_from_power()]
#' in one plot. It is otherwise similar
#' to the `plot`-method for `x_from_power`.
#'
#' @return
#' The `plot`-method of `n_region_from_power`
#' returns `x` invisibly.
#' It is called for its side effect.
#'
#' @export
plot.n_region_from_power <- function(x,
                              what = c("ci", "power_curve", "final_x", "final_power", "target_power",
                                       "sig_area"),
                              text_what = c("final_x", "final_power",
                                            "sig_area"),
                              digits = 3,
                              main = paste0("Power Curve ",
                                            "(Target Power: ",
                                            formatC(x$below$target_power, digits = digits, format = "f"),
                                            ")"),
                              xlab = NULL,
                              ylab = "Estimated Power",
                              ci_level = .95,
                              pars_ci = list(),
                              pars_power_curve = list(),
                              pars_ci_final_x = list(lwd = 2,
                                                     length = .2,
                                                     col = "blue"),
                              pars_target_power = list(lty = "dashed",
                                                       lwd = 2,
                                                       col = "black"),
                              pars_final_x = list(lty = "dotted"),
                              pars_final_power = list(lty = "dotted", col = "blue"),
                              pars_text_final_x = list(pos = 3, cex = 1),
                              pars_text_final_x_lower = pars_text_final_x,
                              pars_text_final_x_upper = pars_text_final_x,
                              pars_text_final_power = list(cex = 1),
                              pars_sig_area = list(col = adjustcolor("lightblue",
                                                   alpha.f = .1)),
                              pars_text_sig_area = list(cex = 1),
                              ...) {

  what <- match.arg(what, several.ok = TRUE)
  text_what <- match.arg(text_what, several.ok = TRUE)
  predictor <- "n"

  # Set xlab

  xlab <- "Sample Size"

  # Was a solution found?
  solution_found_below <- isFALSE(identical(NA, x$below$x_final))
  solution_found_above <- isFALSE(identical(NA, x$above$x_final))
  solution_found <- solution_found_below || solution_found_above

  # === Draw the base plot: Power vs. Predictor

  # It is intended *not* to use plot.power_curve().
  # It is possible that the fit failed.

  # No need for other arguments. They should be stored in object.
  a1 <- rejection_rates(
              x$below$power4test_trials,
              all_columns = TRUE
            )
  a2 <- rejection_rates(
              x$above$power4test_trials,
              all_columns = TRUE
            )
  a <- rbind(a1, a2)
  i <- order(a$n)
  a <- a[i, ]
  a <- a[!duplicated(a$n), ]

  n_min <- min(a$n)
  n_max <- max(a$n)
  n_width <- n_max - n_min

  x_min <- n_min
  if (solution_found_below) {
    tmp <- (x$below$x_final - n_min) / n_width
    if (tmp < .20) {
      tmp2 <- (x$below$x_final - .20 * n_max) /
              (1 - .20)
      x_min <- floor(tmp2)
    }
  }

  x_max <- n_max
  if (solution_found_above) {
    tmp <- (x$above$x_final - n_min) / n_width
    if (tmp > .80) {
      tmp2 <- (x$above$x_final - n_min + .80 * n_min) / .80
      x_max <- ceiling(tmp2)
    }
  }

  args0 <- list(...)
  args1 <- utils::modifyList(
               args0,
               list(object = a,
                    predictor = predictor,
                    main = main,
                    xlab = xlab,
                    ylab = ylab)
              )
  if (is.null(args0$xlim)) {
    args1 <- utils::modifyList(args1,
                               list(xlim = c(x_min, x_max)))
  }

  do.call(plot_power_x,
          args1)

  # === Add CIs?

  if ("ci" %in% what) {

    a_for_ci <- a

    if (solution_found &&
        ("final_x" %in% what)) {

      # A solution was found and final_x line is to be drawn.
      # Draw the final_x CI separately

      # Draw the CI for the final x

      if (solution_found_below) {
        tmp <- x$below$power4test_trials[x$below$i_final]
        class(tmp) <- class(x$below$power4test_trials)
        tmp_args <- utils::modifyList(pars_ci_final_x,
                                      list(object = tmp,
                                          predictor = predictor))
        do.call(plot_power_x_ci,
                tmp_args)

        a_for_ci <- a_for_ci[a_for_ci$n != x$below$x_final, ]

      }

      if (solution_found_above) {
        tmp <- x$above$power4test_trials[x$above$i_final]
        class(tmp) <- class(x$above$power4test_trials)
        tmp_args <- utils::modifyList(pars_ci_final_x,
                                      list(object = tmp,
                                          predictor = predictor))
        do.call(plot_power_x_ci,
                tmp_args)

        a_for_ci <- a_for_ci[a_for_ci$n != x$above$x_final, ]

      }

    }

    # Draw the other CIs
    tmp_args <- utils::modifyList(pars_ci,
                                  list(object = a_for_ci,
                                       predictor = predictor))
    do.call(plot_power_x_ci,
            tmp_args)
  }

  # === Draw the power curve?

  # It is intentional *not* to use the power curve plot method.

  if ("power_curve" %in% what) {
    tmp1 <- x$below$power4test_trials
    tmp2 <- x$above$power4test_trials
    i <- setdiff(names(tmp2), names(tmp1))
    if (length(i) > 0) {
      # No need to check because the models must be the same
      tmp_trials <- c(tmp1, tmp2[i], skip_checking_models = TRUE)
    } else {
      tmp_trials <- tmp1
    }
    fit <- power_curve(tmp_trials)
    tmp_args <- utils::modifyList(pars_power_curve,
                                  list(object = a,
                                       predictor = "n",
                                       power_x_fit = fit))
    do.call(plot_power_curve_x,
            tmp_args)
  }

  # === Draw a horizontal line for the target power?

  if ("target_power" %in% what) {
    tmp_args <- utils::modifyList(pars_target_power,
                                  list(h = x$below$target_power))
    do.call(abline,
            tmp_args)
  }

  if (solution_found) {

    # === Draw a vertical line for the final N?

    if ("final_x" %in% what) {
      if (solution_found_below) {
        tmp_args <- utils::modifyList(pars_final_x,
                                      list(v = x$below$x_final))
        do.call(abline,
                tmp_args)
      }
      if (solution_found_above) {
        tmp_args <- utils::modifyList(pars_final_x,
                                      list(v = x$above$x_final))
        do.call(abline,
                tmp_args)
      }
    }

    # === Draw a horizontal line for the final power?

    if ("final_power" %in% what) {
      if (solution_found_below) {
        tmp_args <- utils::modifyList(pars_final_power,
                                      list(h = x$below$power_final))
        do.call(abline,
                tmp_args)
      }
      if (solution_found_above) {
        tmp_args <- utils::modifyList(pars_final_power,
                                      list(h = x$above$power_final))
        do.call(abline,
                tmp_args)
      }
    }

    # === Add a label for the final x?

    if ("final_x" %in% text_what) {
      if (solution_found_below) {
        x_final_str <- formatC(x$below$x_final,
                              digits = switch(predictor,
                                              n = 0,
                                              es = digits),
                              format = "f")
        tmp_args <- utils::modifyList(list(y = par("usr")[3] + .05 * (par("usr")[4] - par("usr")[3])),
                                      pars_text_final_x_lower)
        tmp_args <- utils::modifyList(tmp_args,
                                      list(x = x$below$x_final,
                                          labels = x_final_str))
        do.call(text,
                tmp_args)
      }
      if (solution_found_above) {
        x_final_str <- formatC(x$above$x_final,
                              digits = switch(predictor,
                                              n = 0,
                                              es = digits),
                              format = "f")
        tmp_args <- utils::modifyList(list(y = par("usr")[3] + .10 * (par("usr")[4] - par("usr")[3])),
                                      pars_text_final_x_upper)
        tmp_args <- utils::modifyList(tmp_args,
                                      list(x = x$above$x_final,
                                          labels = x_final_str))
        do.call(text,
                tmp_args)
      }
    }

    if ("sig_area" %in% what) {
      if (solution_found_below) {
        tmp_args <- utils::modifyList(pars_sig_area,
                                      list(
                                        xleft = par("usr")[1],
                                        xright = x$below$x_final,
                                        ybottom = par("usr")[3],
                                        ytop = par("usr")[4],
                                        border = NULL)
                                      )
        do.call(rect,
                tmp_args)
      }
      if (solution_found_above) {
        tmp_args <- utils::modifyList(pars_sig_area,
                                      list(
                                        xleft = x$above$x_final,
                                        xright = par("usr")[2],
                                        ybottom = par("usr")[3],
                                        ytop = par("usr")[4],
                                        border = NULL)
                                      )
        do.call(rect,
                tmp_args)
      }
    }

    if ("sig_area" %in% text_what) {
      if (solution_found_below) {
        tmp_str <- paste0("Power sig. <\n",
                           formatC(x$below$target_power,
                                   digits,
                                   format = "f"))
        tmp_args <- utils::modifyList(pars_text_sig_area,
                                      list(x = mean(c(x$below$x_final,
                                                      par("usr")[1])),
                                           y = mean(c(x$below$target_power,
                                                      par("usr")[3])),
                                           labels = tmp_str))
        do.call(text,
                tmp_args)
      }
      if (solution_found_above) {
        tmp_str <- paste0("Power sig. >\n",
                           formatC(x$above$target_power,
                                   digits,
                                   format = "f"))
        tmp_args <- utils::modifyList(pars_text_sig_area,
                                      list(x = mean(c(x$above$x_final,
                                                      par("usr")[2])),
                                           y = mean(c(x$above$target_power,
                                                      par("usr")[3])),
                                           labels = tmp_str))
        do.call(text,
                tmp_args)
      }
    }

    # === Add a label for the final power?

    if ("final_power" %in% text_what) {
      if (solution_found_below) {
        tmp <- par("usr")
        tmp_args <- utils::modifyList(pars_text_final_power,
                                      list(y = x$below$power_final,
                                          x = tmp[1] + (tmp[2] - tmp[1]) * .05,
                                          pos = 1,
                                          labels = formatC(x$below$power_final,
                                                            digits = digits,
                                                            format = "f")))
        if (!is.null(pars_text_final_power$x)) {
          tmp_args <- utils::modifyList(tmp_args,
                                        list(x = pars_text_final_power$x))
        }
        do.call(text,
                tmp_args)
      }
      if (solution_found_above) {
        tmp <- par("usr")
        tmp_args <- utils::modifyList(pars_text_final_power,
                                      list(y = x$above$power_final,
                                          x = tmp[1] + (tmp[2] - tmp[1]) * .05,
                                          pos = 3,
                                          labels = formatC(x$above$power_final,
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

  }

  invisible(x)
}
