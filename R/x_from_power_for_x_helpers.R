
set_x_range <- function(object,
                        x,
                        pop_es_name,
                        target_power = .80,
                        k = 4,
                        x_max = switch(x, n = 1000, es = .7),
                        x_min = switch(x, n = 10, es = .0)) {
  if (x == "n") {
    out <- set_n_range(object = object,
                       target_power = target_power,
                       k = k,
                       n_max = x_max)
    return(out)
  }
  if (x == "es") {
    out <- set_es_range(object = object,
                        pop_es_name = pop_es_name,
                        target_power = target_power,
                        k = k,
                        es_max = x_max,
                        es_min = x_min)
    return(out)
  }
}

estimate_x <- function(power_n_fit,
                       target_power = .80,
                       interval = c(50, 2000),
                       extendInt = "no") {
  f <- function(n) {
    # stats::predict(power_n_fit,
    #                newdata = list(n = n)) - target_power
    stats::predict(power_n_fit,
                   newdata = list(x = n)) - target_power
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

estimate_x_range <- function(power_x_fit,
                             x,
                             target_power = .80,
                             k = 5,
                             tolerance = .20,
                             power_min = .01,
                             power_max = .99,
                             interval = switch(x,
                                               n = c(50, 2000),
                                               es = c(0, .7)),
                             extendInt = NULL,
                             x_to_exclude = NULL) {
  x <- match.arg(x,
                 c("n", "es"))
  out <- switch(x,
                n = estimate_n_range(power_n_fit = power_x_fit,
                                     target_power = target_power,
                                     k = k,
                                     tolerance = tolerance,
                                     power_min = power_min,
                                     power_max = power_max,
                                     interval = interval,
                                     extendInt = extendInt,
                                     n_to_exclude = x_to_exclude),
                es = estimate_es_range(power_es_fit = power_x_fit,
                                       target_power = target_power,
                                       k = k,
                                       tolerance = tolerance,
                                       power_min = power_min,
                                       power_max = power_max,
                                       interval = interval,
                                       extendInt = extendInt,
                                       es_to_exclude = x_to_exclude))
  out
}

check_x <- function(ns,
                    interval,
                    n_to_exclude,
                    extendInt) {
  i <- rep(FALSE, length(ns))
  if (isFALSE(extendInt %in% c("yes", "upX"))) {
    i[ns > interval[2]] <- TRUE
  }
  if (isFALSE(extendInt %in% c("yes", "downX"))) {
    i[ns < interval[1]] <- TRUE
  }

  # Ns used are considered invalid
  i[ns %in% n_to_exclude] <- TRUE

  # Duplicated are considered invalid
  i[duplicated(ns)] <- TRUE

  i[is.na(ns)] <- TRUE

  i
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

# TODO:
# - Check. Should write a general rejection_rates_add_ci()
#   that works for _by_n and _by_es.

#' @noRd
# Helper for printing
catwrap <- function(x,
                    width = 0.9 * getOption("width"),
                    indent = 0,
                    exdent = 0,
                    prefix = "",
                    simplify = TRUE,
                    initial = prefix,
                    sep = "\n",
                    fill = FALSE,
                    labels = NULL,
                    append = FALSE) {
  out <- strwrap(x,
                 width = width,
                 indent = indent,
                 exdent = exdent,
                 prefix = prefix,
                 simplify = simplify,
                 initial = initial)
  cat(out,
      sep = sep,
      fill = fill,
      labels = labels,
      append = append)
}
