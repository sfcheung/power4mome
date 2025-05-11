
set_n_range <- function(object,
                        target_power = .80,
                        k = 4,
                        n_max = 1000) {
  n0 <- attr(object, "args")$n
  reject0 <- get_rejection_rates(object)
  power0 <- reject0$reject[1]
  if (n0 >= n_max) {
    stop("Initial sample size (",
          n0,
          ") is equal to or greater than 'n_max' (",
          n_max,
          "). Please increase 'n_max'.")
  }
  if (power0 < target_power) {
    b <- power0 / n0
    n_end <- min(round(target_power / b),
                 n_max)
    n_out <- seq(from = n0,
                 to = n_end,
                 length.out = k)
    n_out <- round(n_out)
    return(n_out)
  } else {
    b <- power0 / n0
    n_end <- min(round(target_power / b),
                 n_max)
    n_out <- seq(from = n_end,
                 to = n0,
                 length.out = k)
    n_out <- round(n_out)
    return(n_out)
  }
}

power_curve_n <- function(object,
                          formula = power ~ (n - c0)^e / (b + (n - c0)^e),
                          start = c(b = 2, c0 = 100, e = 1),
                          lower_bound = c(b = 0, c0 = 0, e = 1),
                          control = list()) {
  # TODO:
  # - Make the function more flexible and allow users
  #   to customize the search.
  reject0 <- get_rejection_rates_by_n(object,
                                      all_columns = TRUE)
  reject0$power <- reject0$sig
  nls_contorl0 <- list(maxiter = 5000)
  nls_contorl1 <- utils::modifyList(nls_contorl0,
                                    control)

  # Do nls
  fit <- tryCatch(suppressWarnings(stats::nls(formula = formula,
                    data = reject0,
                    start = start,
                    algorithm = "port",
                    lower = lower_bound,
                    control = nls_contorl1)),
                  error = function(e) e)
  if (inherits(fit, "nls")) {
    return(fit)
  }

  # Do logistic
  reject1 <- reject0[, c("n", "power", "nrep")]
  reject1$sig <- round(reject1$power * reject1$nrep)
  reject1$ns <- reject1$nrep - reject1$sig
  tmp <- mapply(function(x, y) {
                  c(rep(1, x), rep(0, y - x))
                },
                x = reject1$sig,
                y = reject1$nrep,
                SIMPLIFY = FALSE)
  tmp <- unlist(tmp)
  reject1 <- data.frame(n = rep(reject1$n, times = reject1$nrep),
                        sig = tmp)
  fit <- tryCatch(stats::glm(sig ~ n,
                              data = reject1,
                              family = "binomial"),
                  error = function(e) e,
                  warning = function(w) w)
  # Also catch warning such as
  # - "fitted probabilities numerically 0 or 1 occurred>"
  if (inherits(fit, "glm")) {
    return(fit)
  }

  # Last resort: OLS regression
  fit <- tryCatch(stats::lm(power ~ n,
                            data = reject0),
                  error = function(e) e)
  if (inherits(fit, "lm")) {
    return(fit)
  }
  return(NA)
}

plot_power_curve <- function(object,
                             power_n_fit,
                             ...) {
  # reject0 <- get_rejection_rates_by_n(object)
  reject0 <- rejection_rates_add_ci(object)
  reject0$power <- reject0$reject
  plot(power ~ n,
       data = reject0,
       type = "l",
       lwd = 2,
       ylim = c(0, 1))
  # Some CIs may be of zero width
  i <- !(reject0$reject_ci_lo == reject0$reject_ci_hi)
  if (any(i)) {
    arrows(x0 = reject0$n[i],
          y0 = reject0$reject_ci_lo[i],
          x1 = reject0$n[i],
          y1 = reject0$reject_ci_hi[i],
          length = .05,
          angle = 90,
          code = 3,
          col = "grey50")
  }
  x_new <- seq(min(reject0$n),
               max(reject0$n),
               length.out = 20)
  if (inherits(power_n_fit, "nls") || inherits(power_n_fit, "lm")) {
    y_new <- predict_fit(power_n_fit,
                         newdata = list(n = x_new))
    points(x = x_new,
          y = y_new,
          type = "l",
          lwd = 2,
          col = "red")
  }
}

estimate_n <- function(power_n_fit,
                       target_power = .80,
                       interval = c(50, 2000),
                       extendInt = "no") {
  f <- function(n) {
    # stats::predict(power_n_fit,
    #                newdata = list(n = n)) - target_power
    predict_fit(power_n_fit,
                newdata = list(n = n)) - target_power
  }
  n_target <- tryCatch(stats::uniroot(f,
                             interval = interval,
                             extendInt = extendInt),
                       error = function(e) e)
  if (inherits(n_target, "error")) {
    # Return NA if error occurred. E.g.,
    # - Root not in the interval.
    return(NA)
  }
  n_target <- round(n_target$root)
  # Negative sample size should be handled by the
  # calling function, e.g,, estimate_n_range().
  return(n_target)
}

estimate_n_range <- function(power_n_fit,
                             target_power = .80,
                             k = 5,
                             tolerance = .20,
                             power_min = .01,
                             power_max = .99,
                             interval = c(50, 2000),
                             extendInt = "upX",
                             n_to_exclude = NULL) {
  power_j <- seq(from = max(target_power - tolerance, power_min),
                 to = min(target_power + tolerance, power_max),
                 length.out = k)
  if (isFALSE(target_power %in% power_j)) {
    power_j <- sort(c(power_j, target_power))
  }
  out <- sapply(power_j,
                function(x) {
                  estimate_n(power_n_fit = power_n_fit,
                             target_power = x,
                             interval = interval,
                             extendInt = extendInt)
                })
  out <- ceiling(out)
  # Check invalid Ns
  i <- rep(FALSE, length(out))
  if (isFALSE(extendInt %in% c("yes", "upX"))) {
    i <- out > interval[2]
  }
  if (isFALSE(extendInt %in% c("yes", "downX"))) {
    i <- out < interval[1]
  }
  i[is.na(i)] <- TRUE
  if (isFALSE(any(i))) {
    return(out)
  }
  # Replace invalid Ns by random Ns
  n_pool <- setdiff(seq(interval[1], interval[2]),
                    c(n_to_exclude, out[!i]))
  n_new <- sample(n_pool, size = sum(i))
  out[i] <- n_new
  return(out)
}

predict_fit <- function(object,
                        newdata) {
  if (inherits(object, "glm")) {
    out <- stats::predict(object = object,
                          newdata = newdata,
                          type = "response")
  } else {
    out <- stats::predict(object = object,
                          newdata = newdata)
  }
  return(out)
}

nrep_from_power <- function(power_j,
                            target_power,
                            tolerance,
                            nrep_min,
                            nrep_max) {
  a <- abs(power_j - target_power)
  a <- pmin(a, tolerance)
  b <- 1 - a / tolerance
  d <- nrep_max - nrep_min
  out <- round(nrep_min + b * d)
  return(out)
}

rejection_rates_add_ci <- function(object,
                                   level = .95) {
  df1 <- get_rejection_rates_by_n(object,
                                  all_columns = TRUE)
  df1$reject <- df1$sig
  df1$reject_se <- sqrt(df1$reject * (1 - df1$reject) / df1$nvalid)
  a <- stats::qnorm(1 - (1 - level) / 2)
  df1$reject_ci_lo <- df1$reject - a * df1$reject_se
  df1$reject_ci_hi <- df1$reject + a * df1$reject_se
  df1
}
