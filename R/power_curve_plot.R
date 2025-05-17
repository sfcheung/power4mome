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
#' @param x A `power_curve` object,
#' the output of [power_curve()].
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
#' model_simple_med <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' model_simple_med_es <- c("y ~ m" = "l",
#'                          "m ~ x" = "m",
#'                          "y ~ x" = "s")
#'
#' sim_only <- power4test(nrep = 10,
#'                        model = model_simple_med,
#'                        pop_es = model_simple_med_es,
#'                        n = 50,
#'                        fit_model_args = list(fit_function = "lm"),
#'                        do_the_test = FALSE,
#'                        iseed = 1234,
#'                        parallel = FALSE,
#'                        progress = FALSE)
#'
#' # By n
#'
#' out1 <- power4test_by_n(sim_only,
#'                         nrep = 10,
#'                         test_fun = test_parameters,
#'                         test_args = list(par = "y~x"),
#'                         n = c(25, 100, 200, 1000),
#'                         by_seed = 1234,
#'                         parallel = FALSE,
#'                         progress = FALSE)
#'
#' pout1 <- power_curve(out1)
#' model_simple_med <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' model_simple_med_es <- c("y ~ m" = "l",
#'                          "m ~ x" = "m",
#'                          "y ~ x" = "s")
#'
#' sim_only <- power4test(nrep = 10,
#'                        model = model_simple_med,
#'                        pop_es = model_simple_med_es,
#'                        n = 50,
#'                        fit_model_args = list(fit_function = "lm"),
#'                        do_the_test = FALSE,
#'                        iseed = 1234,
#'                        parallel = FALSE,
#'                        progress = FALSE)
#'
#' # By n
#'
#' out1 <- power4test_by_n(sim_only,
#'                         nrep = 10,
#'                         test_fun = test_parameters,
#'                         test_args = list(par = "y~x"),
#'                         n = c(25, 100, 200, 1000),
#'                         by_seed = 1234,
#'                         parallel = FALSE,
#'                         progress = FALSE)
#'
#' pout1 <- power_curve(out1)
#' pout1
#' plot(pout1)
#'
#' # By pop_es
#'
#' out2 <- power4test_by_pop_es(sim_only,
#'                              nrep = 10,
#'                              test_fun = test_parameters,
#'                              test_args = list(par = "y~x"),
#'                              pop_es_name = "y ~ x",
#'                              pop_es_values = seq(0, .7, .15),
#'                              by_seed = 1234,
#'                              parallel = FALSE,
#'                              progress = FALSE)
#'
#' pout2 <- power_curve(out2)
#' plot(pout2)
#'
#' # By pop_es
#'
#' out2 <- power4test_by_pop_es(sim_only,
#'                              nrep = 10,
#'                              test_fun = test_parameters,
#'                              test_args = list(par = "y~x"),
#'                              pop_es_name = "y ~ x",
#'                              pop_es_values = seq(0, .7, .15),
#'                              by_seed = 1234,
#'                              parallel = FALSE,
#'                              progress = FALSE)
#'
#' pout2 <- power_curve(out2)
#' plot(pout2)
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
                                  list(reject_df = reject_df))
    do.call(plot_power_curve_ci,
            tmp_args)
  }

  # === Draw the power curve?

  if ("power_curve" %in% what) {
    tmp_args <- utils::modifyList(pars_ci,
                                  list(x = x))
    do.call(plot_power_curve_curve,
            tmp_args)
  }

  invisible(x)
}

#' @noRd
plot_power_curve_ci <- function(reject_df,
                                length = .1,
                                angle = 90,
                                code = 3,
                                col = "grey50",
                                ...) {
  # object should be a data frame with CIs already computed
  # Some CIs may be of zero width
  i <- !(reject_df$reject_ci_lo == reject_df$reject_ci_hi)
  if (any(i)) {
    arrows(x0 = reject_df$x[i],
           y0 = reject_df$reject_ci_lo[i],
           x1 = reject_df$x[i],
           y1 = reject_df$reject_ci_hi[i],
           length = length,
           angle = angle,
           code = code,
           col = col,
           ...)
  }
}

#' @noRd

plot_power_curve_curve <- function(x,
                                   type = "l",
                                   lwd = 2,
                                   col = "red",
                                   curve_points = 20,
                                   ...) {
  # x is a power_curve object
  reject_df <- x$reject_df
  x_new <- seq(min(reject_df$x),
               max(reject_df$x),
               length.out = curve_points)
  fit <- x$fit
  if (inherits(fit, "nls") || inherits(fit, "lm")) {
    y_new <- stats::predict(x,
                            newdata = list(x = x_new))
    points(x = x_new,
           y = y_new,
           type = type,
           lwd = lwd,
           col = col,
           ...)
  }
}
