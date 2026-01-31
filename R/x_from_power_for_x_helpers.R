set_x_range_by_x <- function(object,
                        x,
                        pop_es_name,
                        target_power = .80,
                        k = 4,
                        x_max = switch(x, n = 1000, es = .7),
                        x_min = switch(x, n = 10, es = .0),
                        object_by_org = NULL,
                        what = NULL,
                        goal = NULL,
                        tol = NULL,
                        ci_level = .95) {
  if (x == "n") {
    out <- set_n_range_by_x(
                        object = object,
                        target_power = target_power,
                        k = k,
                        n_max = x_max,
                        object_by_org = object_by_org,
                        what = what,
                        goal = goal,
                        tol = tol,
                        ci_level = ci_level
                      )
    return(out)
  }
  if (x == "es") {
    out <- set_es_range_by_x(
                        object = object,
                        pop_es_name = pop_es_name,
                        target_power = target_power,
                        k = k,
                        es_max = x_max,
                        es_min = x_min,
                        object_by_org = object_by_org,
                        what = what,
                        goal = goal,
                        tol = tol,
                        ci_level = ci_level
                      )
    return(out)
  }
}

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

#' @noRd
# Output:
# - k values of x with levels of power predicted by power_x_fit
#   The width determined by tolerance.
# If k == 1 and tolerance == 0,
# - yield the x with the predicted target_power.
# WARNING: If k == 1 but tolerance != 0, unexpected results may occur.
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
  # a <- stats::qnorm(1 - (1 - level) / 2)
  # df1$reject_ci_lo <- reject - a * df1$reject_se
  # df1$reject_ci_hi <- reject + a * df1$reject_se
  ci_i <- reject_ci(
            nreject = round(reject * df1$nvalid),
            nvalid = df1$nvalid,
            level = level,
            method = getOption("power4mome.ci_method", default = "wilson"))
  df1$reject_ci_lo <- as.vector(ci_i[, 1])
  df1$reject_ci_hi <- as.vector(ci_i[, 2])
  if (!add_se) {
    df1$reject_se <- NULL
  }
  df1
}

#' @noRd
# No longer needed because power_curve object
# has a predict method
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
# Check whether any value of x tried already
# meet the ci_hit criterion
find_ci_hit <- function(object,
                        ci_level = .95,
                        target_power = .80,
                        final_nrep = 400,
                        closest_ok = FALSE,
                        if_ties = c("min", "max")) {
  if_ties <- match.arg(if_ties)
  # If no hit, return NULL
  # If hit, always return one number
  # If closest_ok, accept a trial with closest power level
  by_x_ci <- rejection_rates_add_ci(object,
                                    level = ci_level)
  i0 <- (by_x_ci$reject_ci_lo < target_power) &
        (by_x_ci$reject_ci_hi > target_power)

  if (isFALSE(any(i0))) {
    if (closest_ok) {
      tmp <- which.min(abs(by_x_ci$reject - target_power))
      i0 <- rep(FALSE, nrow(by_x_ci))
      i0[tmp] <- TRUE
    } else {
      return(NULL)
    }
  }
  if (sum(i0) > 1) {
    # Find the value with CI hitting the target power
    # and has the smallest SE.
    i1 <- rank(by_x_ci$reject_se)
    i1[!i0] <- NA
    # Do not consider those with nrep < final_nrep
    i1[by_x_ci$nrep < final_nrep] <- NA
    # If ties, the smallest value will be used
    if (all(is.na(i1))) {
      return(NULL)
    }
    i2 <- switch(if_ties,
                 min = which(i1 == min(i1[i0], na.rm = TRUE))[1],
                 max = which(i1 == max(i1[i0], na.rm = TRUE))[1])
  } else {
    # Still check nrep
    # To ignore nrep, set nrep to 0.
    if (by_x_ci$nrep[i0] < final_nrep) {
      return(NULL)
    }
    i2 <- which(i0)
  }
  return(i2)
}

#' @noRd
# Find the solution
# based on goal and what
find_solution <- function(object,
                          target_power = .80,
                          ci_level = .95,
                          what = c("point", "ub", "lb"),
                          tol = 1e-2,
                          goal = c("ci_hit", "close_enough"),
                          final_nrep = 400,
                          closest_ok = FALSE,
                          if_ties = c("min", "max"),
                          weight_by = c("nrep", "ci_width", "se", "none"),
                          debug = FALSE) {

  what <- match.arg(what)
  goal <- match.arg(goal)
  weight_by <- match.arg(weight_by)

  out <- switch(goal,
                ci_hit = find_ci_hit(
                           object = object,
                           ci_level = ci_level,
                           target_power = target_power,
                           final_nrep = final_nrep,
                           closest_ok = closest_ok,
                           if_ties = if_ties),
                close_enough = find_close_enough(
                           object = object,
                           ci_level = ci_level,
                           target_power = target_power,
                           final_nrep = final_nrep,
                           tol = tol,
                           what = what,
                           closest_ok = closest_ok,
                           if_ties = if_ties,
                           weight_by = weight_by,
                           debug = debug))
  out
}

#' @noRd
# Find the solution
# based on goal and what
find_close_enough <- function(
  object,
  target_power = .80,
  ci_level = .95,
  what = c("point", "ub", "lb"),
  tol = 1e-2,
  final_nrep = 400,
  closest_ok = FALSE,
  if_ties = c("min", "max"),
  weight_by = c("nrep", "ci_width", "se", "none"),
  debug = FALSE) {
  # if (debug) browser()

  # If no solution, return NULL
  # If solution, always return one number
  # If closest_ok, accept the closest trial
  what <- match.arg(what)
  if_ties <- match.arg(if_ties)
  weight_by <- match.arg(weight_by)

  by_x_ci <- rejection_rates_add_ci(object,
                                    level = ci_level,
                                    add_se = TRUE)
  var_all <- switch(
                weight_by,
                se = by_x_ci$reject_se ^ 2,
                ci_width = (by_x_ci$reject_ci_hi - by_x_ci$reject_ci_lo),
                none = rep(1, nrow(by_x_ci)),
                nrep = max(by_x_ci$nvalid) / by_x_ci$nvalid
              )
  r_all <- switch(what,
                  point = by_x_ci$reject,
                  ub = by_x_ci$reject_ci_hi,
                  lb = by_x_ci$reject_ci_lo)
  r_all0 <- r_all - target_power
  r_all1 <- abs(r_all0)
  i0 <- r_all1 < tol

  if (isFALSE(any(i0))) {
    if (closest_ok) {
      tmp <- which.min(r_all1 * var_all)
      i0 <- rep(FALSE, nrow(by_x_ci))
      i0[tmp] <- TRUE
    } else {
      return(NULL)
    }
  }
  if (sum(i0) > 1) {
    # Find the value with CI hitting the target power
    # and has the smallest SE.
    i1 <- rank(by_x_ci$reject_se)
    i1[!i0] <- NA
    # Do not consider those with nrep < final_nrep
    i1[by_x_ci$nrep < final_nrep] <- NA
    # If ties, the smallest value will be used
    if (all(is.na(i1))) {
      return(NULL)
    }
    i2 <- switch(if_ties,
                 min = which(i1 == min(i1[i0], na.rm = TRUE))[1],
                 max = which(i1 == max(i1[i0], na.rm = TRUE))[1])
  } else {
    # Still check nrep
    # To ignore nrep, set nrep to 0.
    if (by_x_ci$nrep[i0] < final_nrep) {
      return(NULL)
    }
    i2 <- which(i0)
  }
  return(i2)
}

#' @noRd
# Check whether the x_from_power object
# has x and (optionally) pop_es_name identical
# to those requested.
check_x_from_power_as_input <- function(object,
                                        x,
                                        pop_es_name,
                                        final_nrep,
                                        ci_level) {
  if (!identical(x, object$x)) {
    stop("object's x is ", object$x, " but ",
         "requested x is ", x)
  }
  if (x == "es") {
    if (!identical(pop_es_name, object$pop_es_name)) {
      stop("object's pop_es_name is ", object$pop_es_name, " but ",
          "requested pop_es_name is ", pop_es_name)
    }
  }
  if (object$arg$final_nrep != final_nrep) {
    stop("object's final_nrep (",
         object$final_nrep,
         ") is different from the requested final_nrep (",
         final_nrep,
         ").")
  }
  if (object$ci_level != ci_level) {
    stop("object's ci_level (",
         object$ci_level,
         ") is different from the requested ci_level (",
         ci_level,
         ").")
  }
  return(TRUE)
}

#' @noRd
# Get the vector of x values already tried
get_x_tried <- function(object,
                        x) {
  tmp <- rejection_rates_add_ci(object)
  out <- switch(x,
                n = tmp$n,
                es = tmp$es)
  out
}

#' @noRd
# Check whether a value of x has already been tried
in_x_tried <- function(test_x,
                       object,
                       x) {
  # If yes, return the index
  # Otherwise, return NA
  x_tried <- get_x_tried(object = object,
                         x = x)
  match(test_x, x_tried)
}

#' @noRd
force_new_x <- function(
                    x0,
                    x_tried,
                    x_interval,
                    x_type = c("es", "n"),
                    step = switch(x_type,
                                  n = 1,
                                  es = .01),
                    post_process = switch(x_type,
                                          es = function(x) x,
                                          n = ceiling),
                    k = 20
                  ) {
  k0 <- 0
  xi <- x0
  step_all <- as.vector(rbind(seq_len(k), -seq_len(k)))
  while ((k0 <= k * 2) &&
         ((xi %in% x_tried) ||
          (xi < min(x_interval)) ||
          (xi > max(x_interval)))) {
    k0 <- k0 + 1
    xi <- x0 + step_all[k0] * step
    xi <- post_process(xi)
  }
  xi
}

#' @noRd
# Determine the probable range of valid values
# for a parameter
fix_es_interval <- function(object,
                            x,
                            pop_es_name,
                            x_interval,
                            progress = TRUE,
                            step = .10,
                            es_min = -.90,
                            es_max = .90) {
  if ((x == "es") &&
      is.null(x_interval)) {
    if (progress) {
      cat("\n--- Interval for x (es) ---\n\n")
      cat("Determining the valid interval of values for '",
          pop_es_name,
          "' ...\n",
          sep = "")
    }
    es_tmp <- pop_es(object,
                     pop_es_name = pop_es_name)
    range_tmp <- tryCatch(check_valid_es_values(object,
                                                pop_es_name = pop_es_name,
                                                step = step,
                                                es_min = es_min,
                                                es_max = es_max),
                    error = function(e) e)
    if ((inherits(range_tmp, "error")) ||
        (all(is.na(range_tmp)))) {
      if (es_tmp < 0) {
        x_interval <- c(-.95, 0)
      } else {
        x_interval <- c(0, .95)
      }
      if (progress) {
        cat("Failed to find the valid range.\n")
        cat("This range will be used:",
            paste0(formatC(x_interval[1], digits = 3, format = "f"),
                  " to ",
                  formatC(x_interval[2], digits = 3, format = "f")),
            "\n")
        cat("Set 'x_interval' manually if necessary.\n")
      }
    } else {
      x_interval <- range_tmp
      if ((es_tmp < min(x_interval)) ||
          (es_tmp > max(x_interval))) {
        # TODO:
        # - Do we need this check? This should rarely happen.
      }
      # If es0 is in the interval, then
      if (es_tmp <= 0) {
        x_interval[x_interval >= 0] <- 0
      } else {
        x_interval[x_interval <= 0] <- 0
      }
      if (progress) {
        cat("The probable valid range, of the same sign of object's value, is:",
            paste0(formatC(x_interval[1], digits = 3, format = "f"),
                  " to ",
                  formatC(x_interval[2], digits = 3, format = "f")),
            "\n")
      }
    }
  }
  return(x_interval)
}

#' @noRd
# Find the x-intersection,
# as in Regula Falsi
x_from_y <- function(x1,
                     y1,
                     x2,
                     y2,
                     target = 0) {
  (target - y1) * (x2 - x1) / (y2 - y1) + x1
}

#' @noRd
# Can be used for most algorithms
# Input:
# - f_i: The power to be tested
# - target: The target power
# - nrep: The number of replications
# - final_nrep: The required nrep. Always not a solution if nrep != final_nrep
# - ci_level: The level of confidence of the CI
# - which:
#    - point: Check against the target power
#    - ub: Check the upper bound of the CI against the target power
#    - lb: Check the lower bound of the CI against the target power
#    Ignored if goal == "ci_hit"
# - tol:
#    - The tolerance used in "close_enough"
# - goal:
#    - ci_hit: f_i is a solution if its CI include target_power
#    - close_enough: f_i is as solution if
#        abs(what - target) < tol
check_solution <- function(f_i,
                           target_power,
                           nrep,
                           final_nrep,
                           ci_level = .95,
                           what = c("point", "ub", "lb"),
                           tol = 1e-2,
                           goal = c("ci_hit", "close_enough")) {
  if (nrep != final_nrep) {
    return(FALSE)
  }
  goal <- match.arg(goal)
  # a <- abs(stats::qnorm((1 - ci_level) / 2))
  se_i <- sqrt(f_i * (1 - f_i) / nrep)
  # cilb <- f_i - a * se_i
  # ciub <- f_i + a * se_i
  ci_i <- reject_ci(
            nreject = round(f_i * nrep),
            nvalid = nrep,
            level = ci_level,
            method = getOption("power4mome.ci_method", default = "wilson"))
  cilb <- as.vector(ci_i[, 1])
  ciub <- as.vector(ci_i[, 2])
  if (goal == "ci_hit") {
    # Ignore what
    if ((cilb < target_power) && (ciub > target_power)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  # goal == "close_enough"
  chk_point <- switch(what,
                      point = f_i,
                      ub = ciub,
                      lb = cilb)
  out <- abs(chk_point - target_power) < tol
  out
}

#' @noRd
# Find the root by the Muller's method
# For reference. Not used directly.
muller <- function(
  f,
  ...,
  start,
  maxiter = 100,
  tol = 1e-5
) {
  x_all <- rep(NA, maxiter)
  y_all <- rep(NA, maxiter)
  if (length(start) != 3) {
    stop("'start' must be a vector of three numbers")
  }
  i <- 1
  start <- sort(start)
  xm2 <- start[1]
  xm1 <- start[2]
  x0i <- start[3]
  while (i <= maxiter) {
    x_all[i] <- x0i
    y_all[i] <- f(x0i, ...)
    xp1 <- root_muller_i(
      f = f,
      xm2 = xm2,
      xm1 = xm1,
      x0i = x0i,
      ...
    )
    fp1 <- f(xp1, ...)
    cat("----\n")
    cat("i =", i, "\n")
    cat("xp1 =", xp1, "\n")
    cat("fp1 =", fp1, "\n")
    if (abs(fp1) < tol) {
      break
    }
    xm2 <- xm1
    xm1 <- x0i
    x0i <- xp1
    i <- i + 1
  }
  list(x = x_all[seq_len(i)],
       y = y_all[seq_len(i)])
}

#' @noRd
# Find the next value based on the Muller's method
root_muller_i <- function(
  f,
  xm2,
  xm1,
  x0i,
  ym2 = NULL,
  ym1 = NULL,
  y0i = NULL,
  ...
) {
  if (is.null(ym2)) {
    ym2 <- f(xm2, ...)
  }
  if (is.null(ym1)) {
    ym1 <- f(xm1, ...)
  }
  if (is.null(y0i)) {
    y0i <- f(x0i, ...)
  }
  A <- ((xm1 - x0i) * (ym2 - y0i) - (xm2 - x0i) * (ym1 - y0i)) /
       ((x0i - xm1) * (xm1 - xm2) * (xm2 - x0i))
  B <- ((xm2 - x0i)^2 * (ym1 - y0i) - (xm1 - x0i)^2 * (ym2 - y0i)) /
       ((x0i - xm1) * (xm1 - xm2) * (xm2 - x0i))
  C <- y0i

  xp1a <- suppressWarnings(
            x0i + (-2 * C) /
            (B - sqrt(B^2 - 4 * A * C))
          )
  xp1b <- suppressWarnings(
            x0i + (-2 * C) /
            (B + sqrt(B^2 - 4 * A * C))
          )
  if (is.nan(xp1a)) {
    xp1a <- NA
  }
  if (is.nan(xp1b)) {
    xp1b <- NA
  }

  xp1 <- max(xp1a, xp1b, na.rm = TRUE)
  xp1
}

#' @noRd
check_solution_in_by_x <- function(
  object,
  target_power,
  final_nrep,
  ci_level = .95,
  what = c("point", "ub", "lb"),
  tol = 1e-2,
  goal = c("ci_hit", "close_enough")
) {
  what <- match.arg(what)
  goal <- match.arg(goal)
  if (inherits(object, "rejection_rates_df")) {
    reject_df <- object
  } else {
    reject_df <- rejection_rates(object,
                                ci_level = ci_level,
                                all_columns = TRUE,
                                se = TRUE)
  }
  reject_all <- reject_df$reject
  nrep_all <- reject_df$nrep
  chK_all <- mapply(
               check_solution,
               f_i = reject_all,
               nrep = nrep_all,
               MoreArgs = list(target_power = target_power,
                               final_nrep = final_nrep,
                               ci_level = ci_level,
                               what = what,
                               tol = tol,
                               goal = goal)
             )
  chK_all
}


#' @noRd

check_changes <- function(
    x_history,
    delta_tol = .01,
    last_k = 3) {
  x <- x_history[!is.na(x_history)]
  p <- length(x)
  if (p < last_k) return(TRUE)
  x_test <- rev(x_history[!is.na(x_history)])
  x_range <- range(x_test,
                   na.rm = TRUE)
  if (abs(x_range[2] - x_range[1]) < delta_tol) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @noRd

check_rate <- function(
    x_history,
    delta_slope_tol = -.1,
    last_k = 3) {
  x <- x_history[!is.na(x_history)]
  p <- length(x)
  if (p < last_k) return(TRUE)
  x <- rev(rev(x)[1:last_k])
  trend <- stats::lm.fit(
          y = matrix(abs(x), ncol = 1),
          x = cbind(1, matrix(1:last_k, ncol = 1)))$coefficients["x2"]
  if (trend > delta_slope_tol) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @noRd
# - Return the adjusted power
#   based on what and goal
target_power_adjusted <- function(
  target_power = .80,
  goal = c("ci_hit", "close_enough"),
  what = c("point", "ub", "lb"),
  tolerance = .02,
  nrep = 100,
  level = .95
) {
  if (goal == "close_enough") {
    if (what == "point") {
      return(target_power)
    }
    if (what %in% c("ub", "lb")) {
      # Find the power with ub close enough to target
      b <- switch(what,
                  ub = "cihi",
                  lb = "cilo")
      f <- function(x, adj) {
        reject_ci_wilson(nreject = x,
                         nvalid = nrep,
                         level = level)[, b] -
        target_power + adj
      }
      tmp1 <- stats::uniroot(
              f,
              adj = tolerance,
              interval = c(0, nrep)
            )
      out1 <- tmp1$root / nrep
      tmp2 <- stats::uniroot(
              f,
              adj = -tolerance,
              interval = c(0, nrep)
            )
      out2 <- tmp2$root / nrep
      return(mean(c(out1, out2)))
    }
  }
  if (goal == "ci_hit") {
    return(target_power)
  }
}
