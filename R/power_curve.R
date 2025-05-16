#' @title Power Curve
#'
#' @description Estimate the relation
#' between power and a characteristic,
#' such as sample size or population
#' effect size.
#'
#' @details This is a general function,
#' to be used by wrappers such as
#' [power_curve_by_n()] and
#' [power_curve_by_es()].
#'
#' @return
#' TODO: Specify what are returned.
#' It returns a list which is
#' a `power_curve` object, with the
#' following elements:
#'
#' @param object An object of the class
#' `power4test_by_n` or `power4test_by_pop_es`.
#'
#' @param formula A formula of the model
#' for [stats::nls()]. It can also be
#' a list of formulas, and the models
#' will be fitted successively by
#' [stats::nls()], with the first
#' model fitted successfully adopted.
#' The response variable must be named
#' `reject`, and the predictor named
#' `x`. Whether `x` represents `n` or
#' `es` depends on the class of `object`.
#'
#' @param start Either a named vector
#' of the start value(s) of parameter(s)
#' in `formula`, or a list of named
#' vectors of the starting value(s)
#' of the list of formula(s).
#'
#' @param lower_bound Either a named vector
#' of the lower bound(s) of parameter(s)
#' in `formula`, or a list of named
#' vectors of the lower bound(s)
#' for the list of formula(s). They will
#' be passed to `lower` of [stats::nls()].
#'
#' @param upper_bound Either a named vector
#' of the upper bound(s) of parameter(s)
#' in `formula`, or a list of named
#' vectors of the upper bound(s)
#' for the list of formula(s). They will
#' be passed to `upper` of [stats::nls()].
#'
#' @param nls_args A named list of
#' arguments to be used when calling
#' [stats::nls()]. Used to override
#' internal default, such as the
#' algorithm (default is `"port"`).
#' Used with cautions.
#'
#' @param nls_control A named list of
#' arguments to be passed the `control`
#' argument of [stats::nls()] when
#' estimating the relation between
#' power and sample size. The values will
#' override internal default values,
#' and also override `nls_args`.
#'
#' @param verbose Logical. Whether
#' the messages will be printed when
#' trying different models.
#'
#' @param
#'
#' @seealso [power4test_by_n()] and [power4test_by_es()]
#'
#' @examples
#' x <- 1
#' # TODO: Add some examples.
#' \donttest{
#' }
#'
#' @export
power_curve_x <- function(object,
                          formula = NULL,
                          start = c(b = 2, c0 = 100, e = 1),
                          lower_bound = c(b = 0, c0 = 0, e = 1),
                          upper_bound = c(b = Inf, c0 = Inf, e = Inf),
                          nls_args = list(),
                          nls_control = list(),
                          verbose = TRUE) {

  # reject ~ I((x - c0)^e) / (b + I((x - c0)^e))
  # The formula used depends on the nature of the predictors
  # The predictor should always be named 'x' in the formula.
  # The outcome is always 'reject'
  # `formula` can be a list of possible NLS models.

  class0 <- class(object)[1]
  if (!(class0 %in% c("power4test_by_n",
                      "power4test_by_pop_es"))) {
    stop("'object' is neither a power4test_by_n or power4test_by_n object.")
  }

  reject0 <- switch(class0,
                    power4test_by_n = get_rejection_rates_by_n(object,
                                                               all_columns = TRUE),
                    power4test_by_pop_es = get_rejection_rates_by_pop_es(object,
                                                                          all_columns = TRUE))

  predictor <- switch(class0,
                      power4test_by_n = "n",
                      power4test_by_pop_es = "es")

  #reject0$power <- reject0$reject
  reject0$x <- reject0[predictor]

  model_found <- FALSE

  if (nrow(reject0) >= 4) {

    # === nls ===

    nls_args_fixed <- fix_nls_args(formula = formula,
                                   start = start,
                                   lower_bound = lower_bound,
                                   upper_bound = upper_bound,
                                   nsl_args = nls_args,
                                   nls_control = nls_control)

    # Try each formula
    fit <- NA
    fit_deviance <- Inf
    for (i in seq_along(formula)) {

      nls_args1 <- utils::modifyList(nls_args_fixed$nls_args,
                                     list(formula = nls_args_fixed$formula[[i]],
                                          data = reject0,
                                          start = nls_args_fixed$start[[i]],
                                          lower = nls_args_fixed$lower_bound[[i]],
                                          upper = nls_args_fixed$upper_bound[[i]],
                                          control = nls_args_fixed$nls_contorl1,
                                          nrep = reject0$nrep))
      # Do nls
      fit_i <- tryCatch(suppressWarnings(do.call(do_nls,
                                                 nls_args1)),
                        error = function(e) e)
      if (inherits(fit, "nls")) {
        fit_i_d <- stats::deviance(fit)
        if (fit_i_d < fit_deviance) {
          fit <- fit_i
          fit_deviance <- fit_i_d
        }
      }
    }
  } else {
    if (verbose) {
      message("- 'nls()' estimation skipped when less than 4 values of predictor examined.")
    }
  }

  if (inherits(fit, "nls")) {
    model_found <- TRUE
  } else {
    if (verbose) {
      message("- 'nls()' estimation failed. Switch to logistic regression.")
    }
  }

  # === Logistic ===

  # Do logistic
  fit <- do_logistic(reject_df = reject0)

  if (inherits(fit, "glm")) {
    model_found <- TRUE
  } else {
    if (verbose) {
      message("- Logistic regression failed. Switch to linear regression.")
    }
  }

  # === OLS Regression ===

  # Last resort: OLS regression
  fit <- do_lm(reject_df = reject0,
               weights = reject0$nrep)

  if (inherits(fit, "lm")) {
    model_found <- TRUE
  } else {
    if (verbose) {
      message("- OLS regression failed. No power curve estimated.")
    }
  }

  # TODO:
  # - Consider using `splinefun()` as a last resort.

  # TODO:
  # - Create the power_curve object.

  out <- list(fit = fit,
              reject_df = reject0,
              predictor = predictor,
              call = match.call())
  class(out) <- c("power_curve", class(out))
  return(out)
}

#' @rdname power_curve_x
#' @export
power_curve_by_n <- function(object,
                             formula = reject ~ (x - c0)^e / (b + (x - c0)^e),
                             start = c(b = 2, c0 = 100, e = 1),
                             lower_bound = c(b = 0, c0 = 0, e = 1),
                             upper_bound = c(b = Inf, c0 = Inf, e = Inf),
                             nls_args = list(),
                             nls_control = list(),
                             verbose = TRUE) {
  if (!inherits(object, "power4test_by_n")) {
    stop("'object' is not a power4test_by_n object.")
  }
  power_curve_x(object = object,
                formula = formula,
                start = start,
                lower_bound = lower_bound,
                upper_bound = upper_bound,
                nls_args = nls_args,
                nls_control = nls_control,
                verbose = verbose)
}

#' @rdname power_curve_x
#' @export
power_curve_by_pop_es <- function(object,
                                  formula = list(reject ~ 1 - 1 / I((1 + (x / d)^a)^b),
                                                 reject ~ 1 - exp(x / a) / I((1  + exp(x / a))^b),
                                                 reject ~ 1 - 2 / (exp(x / d) + exp(-x / d)),
                                                 reject ~ 1 / (1 + a * exp(-b * x))),
                                  start = list(c(a = 2, b = 4, d = 4),
                                               c(a = 1, b = 2),
                                               c(d = 1),
                                               c(a = 1, b = 1)),
                                  lower_bound = NULL,
                                  upper_bound = NULL,
                                  nls_args = list(),
                                  nls_control = list(),
                                  verbose = TRUE) {
  if (!inherits(object, "power4test_by_pop_es")) {
    stop("'object' is not a power4test_by_pop_es object.")
  }
  power_curve_x(object = object,
                formula = formula,
                start = start,
                lower_bound = lower_bound,
                upper_bound = upper_bound,
                nls_args = nls_args,
                nls_control = nls_control,
                verbose = verbose)
}

#' @noRd
do_nls <- function(...,
                   nrep = NULL) {
  args <- match.args()
  # Try weights
  if (!is.null(nrep)) {
    args1 <- utils::modifyList(args,
                              list(data = data,
                                   weights = nrep))
    fit <- tryCatch(suppressWarnings(do.call(stats::nls,
                                     args1)),
                   error = function(e) e)
    if (inherits(fit, "nls")) {
      return(fit)
    }
  }

  # Do not use weights
  fit <- tryCatch(suppressWarnings(do.call(stats::nls,
                                   args)),
                  error = function(e) e)
  if (inherits(fit, "nls")) {
    return(fit)
  }
  # fit is an error. Return it
  return(fit)
}

#' @noRd
do_logistic <- function(reject_df) {
  # The predictor is always 'x'.

  # Expand to one row per replication
  reject1 <- reject_df[, c("x", "reject", "nrep")]
  reject1$sig <- round(reject1$reject * reject1$nrep)
  reject1$ns <- reject1$nrep - reject1$sig
  tmp <- mapply(function(x, y) {
                  c(rep(1, x), rep(0, y - x))
                },
                x = reject1$sig,
                y = reject1$nrep,
                SIMPLIFY = FALSE)
  tmp <- unlist(tmp)
  reject1 <- data.frame(n = rep(reject1$n,
                                times = reject1$nrep),
                        sig = tmp)
  # Rename sig to reject,
  # to be consistent with other mdoels
  reject1$reject <- reject1$sig

  # Do logistic regression
  fit <- tryCatch(stats::glm(reject ~ x,
                             data = reject1,
                             family = "binomial"),
                  error = function(e) e,
                  warning = function(w) w)
  # Also catch warning such as
  # - "fitted probabilities numerically 0 or 1 occurred>"

  # Always return the fit
  # Let the calling function to handle error
  return(fit)
}

#' @noRd
do_lm <- function(reject_df,
                  weights) {

  # Use weights
  fit <- tryCatch(stats::lm(reject ~ x,
                            data = reject_df,
                            weights = weights),
                  error = function(e) e)
  if (inherits(fit, "lm")) {
    return(fit)
  }
  # Do not use weights
  fit <- tryCatch(stats::lm(reject ~ x,
                            data = reject_df),
                  error = function(e) e)

  # Return fit, error or not
  return(fit)
}

#' @noRd

fix_nls_args <- function(formula,
                         start,
                         lower_bound,
                         upper_bound,
                         nls_args,
                         nls_control) {

  # Set up the per-formula arguments
  # - If not a list, coerce to a list:
  #   one element for each formula.

  if (!is.list(formula)) {
    formula <- list(formula)
  }
  if (!is.list(start)) {
    start <- list(start)
  }
  if (!is.list(lower_bound)) {
    lower_bound <- list(lower_bound)
  }
  if (!is.list(upper_bound)) {
    upper_bound <- list(upper_bound)
  }

  # Default argument values
  nls_contorl0 <- list(maxiter = 1000)
  nls_contorl1 <- utils::modifyList(nls_contorl0,
                                    nls_control)

  # Use "port" because zero residual cases are possible
  # But can be overridden
  nls_args0 <- list(algorithm = "port")
  nls_args1 <- utils::modifyList(nls_args0,
                                  nls_args)

  out <- list(formula = formula,
              start = start,
              lower_bound = lower_bound,
              upper_bound = upper_bound,
              nls_args = nls_args,
              nls_control = nls_control)

  return(out)

}