
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


rejection_rates_add_ci <- function(object,
                                   level = .95,
                                   add_reject = TRUE,
                                   add_se = TRUE) {
  if (!is.data.frame(object)) {
    if (inherits(object, "power4test_by_es")) {
      df1 <- rejection_rates(object,
                             all_columns = TRUE)
    }
    if (inherits(object, "power4test_by_n")) {
      df1 <- rejection_rates(object,
                             all_columns = TRUE)
    }
  } else {
    # Assume it is already an output with rejection rates
    df1 <- object
  }
  # It works on any data frame with these two columns:
  # - `sig` or `reject`
  # - `nvalid`
  # It adds these columns:
  # - `reject` (if add_reject is TRUE)
  # - `reject_se`
  # - `reject_ci_lo`
  # - `reject_ci_hi`

  tmp <- match(c("sig", "reject"), colnames(df1))
  if (all(is.na(tmp))) {
    stop("CI cannot be computed because 'reject' or 'sig' column not found.")
  }
  tmp <- tmp[!is.na(tmp)][1]
  reject <- df1[, tmp, drop = TRUE]
  if (add_reject && isFALSE("reject" %in% colnames(df1))) {
    df1$reject <- df1$sig
  }
  df1$reject_se <- sqrt(reject * (1 - reject) / df1$nvalid)
  a <- stats::qnorm(1 - (1 - level) / 2)
  df1$reject_ci_lo <- reject - a * df1$reject_se
  df1$reject_ci_hi <- reject + a * df1$reject_se
  if (!add_se) {
    df1$reject_se <- NULL
  }
  df1
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

#' @noRd
find_ci_hit <- function(object,
                        ci_level = .95,
                        target_power = .80,
                        final_nrep = 400,
                        closest_ok = FALSE) {
  # If no hit, return NULL
  # If hit, always return one number

  by_x_ci <- rejection_rates_add_ci(object,
                                    level = ci_level)
  i0 <- (by_x_ci$reject_ci_lo < target_power) &
        (by_x_ci$reject_ci_hi > target_power)

  if (isFALSE(any(i0)) && !closest_ok) {
    return(NULL)
  } else {
    i0 <- which.min(abs(by_x_ci$reject - target_power))
  }

  # If only one CI hits, always keep it.
  if (sum(i0) > 1) {
    # Find the value with CI hitting the target power
    # and has the smallest SE.
    i1 <- rank(by_x_ci$reject_se)
    # Do not consider those with nrep < final_nrep
    i1[by_x_ci$nrep < final_nrep] <- Inf
    # If ties, the smallest value will be used
    i2 <- which(i1 == min(i1[i0]))[1]
  } else {
    i2 <- which(i0)
  }
  return(i2)
}
