#' @title Plot a Power Curve
#'
#' @description It plots the results
#' in a 'power_curve' object, such as the
#' estimated power against sample size.
#'
#' @details
#' It currently plots the relation
#' between estimated power and
#' the predictor. Other elements
#' can be requested (see the argument
#' `what`), and they can be customized
#' individually.
#'
#' @return
#' The `plot`-method of `power_curve`
#' objects
#' is called for its side effect.
#'
#' @param x A `n_from_power` object,
#' the output of [n_from_power()].
#'
#' @param what A character vector of
#' what to include in the
#' plot. Possible values are
#' `"ci"` (confidence intervals
#' for the estimated sample size) and
#' `"power_curve"` (the crude power
#' curve, if available).
#' By default, all these elements will
#' be plotted.
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
#' @param type An argument of the
#' default plot method [plot.default()].
#' Default is `"l"`. See [plot.default()]
#' for other options.
#'
#' @param ylim A two-element numeric
#' vector of the range of the vertical
#' axis.
#'
#' @param ci_level The level of
#' confidence of the confidence intervals,
#' if requested. Default is `.95`, denoting
#' 95%.
#'
#' @param ... Optional arguments.
#' Passed to [plot()] when drawing
#' the base plot.
#'
#' @seealso [power_curve()]
#'
#' @examples
#'
#' # TODO:
#' # - To revise
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
plot.power_curve <- function(x,
                             what = c("ci", "power_curve"),
                             main = paste0("Power Curve ",
                                           "(Predictor: ",
                                           switch(x$predictor, n = "Sample Size", es = "Effect Size"),
                                           ")"),
                             xlab = switch(x$predictor, n = "Sample Size", es = "Effect Size"),
                             ylab = "Estimated Power",
                             pars_ci = list(),
                             type = "l",
                             ylim = c(0, 1),
                             ci_level = .95,
                             ...) {

  what <- match.arg(what, several.ok = TRUE)

  reject_df <- rejection_rates_add_ci(x$reject_df,
                                      level = ci_level)

  # === Draw the base plot: Power vs. N

  predictor <- x$predictor

  plot(x = reject_df$x,
       y = reject_df$reject,
       main = main,
       xlab = xlab,
       ylab = ylab,
       type = type,
       ylim = ylim,
       ...)

  # === Add CIs?

  if ("ci" %in% what) {

    tmp_args <- utils::modifyList(pars_ci,
                                  list(object = reject_df))
    do.call(plot_power_curve_x_ci,
            tmp_args)
  }

  # === Draw the power curve?

  if ("power_curve" %in% what) {
    tmp_args <- utils::modifyList(pars_ci,
                                  list(x = x))
    do.call(plot_power_curve_x_curve,
            tmp_args)
  }

  invisible(x)
}

#' @noRd
plot_power_curve_x_ci <- function(reject_df,
                                  length = .1,
                                  angle = 90,
                                  code = 3,
                                  col = "grey50",
                                  ...) {
  # object should be a data frame with CIs already computed
  # Some CIs may be of zero width
  i <- !(reject_df$reject_ci_lo == reject_df$reject_ci_hi)
  if (any(i)) {
    arrows(x0 = reject_df$n[i],
           y0 = reject_df$reject_ci_lo[i],
           x1 = reject_df$n[i],
           y1 = reject_df$reject_ci_hi[i],
           length = length,
           angle = angle,
           code = code,
           col = col,
           ...)
  }
}

#' @noRd

plot_power_curve_x_curve <- function(x,
                                     type = "l",
                                     lwd = 2,
                                     col = "red",
                                     curve_points = 20,
                                     ...) {
  # x is a power_curve object
  reject_df <- x$reject_df
  x_new <- seq(min(reject_df$n),
               max(reject_df$n),
               length.out = curve_points)
  if (inherits(power_n_fit, "nls") || inherits(power_n_fit, "lm")) {
    # TODO:
    # - Write a predict method for power_curve objects
    y_new <- predict(x,
                     newdata = list(n = x_new))
    points(x = x_new,
           y = y_new,
           type = type,
           lwd = lwd,
           col = col,
           ...)
  }
}
