set_n_range_by_x <- function(
                        object,
                        target_power = .80,
                        k = 4,
                        n_max = 1000,
                        object_by_org = NULL,
                        what = NULL,
                        goal = NULL,
                        tol = NULL,
                        ci_level = .95) {
  # No need for other arguments because they will be retrieved from object_by_org
  reject0by <- rejection_rates(object_by_org,
                               level = ci_level,
                               add_se = TRUE,
                               all_columns = TRUE)
  # Always return a value because closest_ok is TRUE
  i0 <- find_solution(
                object_by_org,
                target_power = target_power,
                ci_level = ci_level,
                what = what,
                tol = tol,
                goal = goal,
                final_nrep = 0,
                closest_ok = TRUE,
                weight_by = "nrep",
                debug = TRUE
              )
  n0 <- reject0by$n[i0]
  what0 <- switch(
              what,
              point = "reject",
              lb = "reject_ci_lo",
              ub = "reject_ci_hi"
            )
  est0 <- reject0by[i0, what0, drop = TRUE]
  # Display est0 a little bit
  if (est0 == target_power) {
    est0 <- target_power * .95
  }
  if (est0 < target_power) {
    i1a <- reject0by[, what0, drop = TRUE] > target_power
    i1b <- reject0by$n > n0
    i1 <- i1a & i1b
  } else {
    i1a <- reject0by[, what0, drop = TRUE] < target_power
    i1b <- reject0by$n < n0
    i1 <- i1a & i1b
  }
  if (any(i1)) {
    i1 <- which(i1)[1]
    n1 <- reject0by$n[i1]
  } else {
    n1 <- n0 * target_power / est0
  }
  n1 <- ceiling(n1)
  n0 <- min(n0, n_max)
  n1 <- min(n1, n_max)
  # Displace n0 and n1
  n_out <- range(n0, n1)
  n_out
}

set_n_range <- function(object,
                        target_power = .80,
                        k = 4,
                        n_max = 1000) {
  n0 <- attr(object, "args")$n
  # No need for other arguments because only `reject` is used
  reject0 <- rejection_rates(object)
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
                 n_max,
                 n0 * 2)
    n_out <- seq(from = n0,
                 to = n_end,
                 length.out = k)
    n_out <- round(n_out)
    return(n_out)
  } else {
    # If power0 == target_power,
    # Be conservative and decrease power by a small amount,
    # if (power0 == target_power) {
    #   power0 <- target_power * .80
    # }
    b <- power0 / n0
    n_end <- min(round(target_power / b),
                 n_max,
                 n0 / 2)
    n_out <- seq(from = n_end,
                 to = n0,
                 length.out = k)
    n_out <- round(n_out)
    return(n_out)
  }
}

estimate_n <- function(power_n_fit,
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

  # If NA, have to do random sampling
  i <- is.na(out)
  if (any(i)) {
    # Duplication is OK because it will be fixed later
    out[i] <- sample(seq(interval[1],
                         interval[2]),
                     size = sum(i),
                     replace = FALSE)
  }

  # Check invalid Ns

  i <- check_n(ns = out,
               interval = interval,
               n_to_exclude = n_to_exclude,
               extendInt = extendInt)

  if (isFALSE(any(i))) {
    return(out)
  }
  # Replace invalid Ns by random Ns
  # Do not use the full interval
  # But can include Ns already considered
  new_interval1 <- min(n_to_exclude, interval)
  new_interval2 <- max(ceiling(max(n_to_exclude) * 1.25),
                       interval[2])
  n_pool <- setdiff(seq(new_interval1, new_interval2),
                    c(n_to_exclude, out[!i]))
  for (q in 1:10) {
    out[i] <- out[i] + sample(c(seq(1, q),
                                -seq(1, q)),
                              size = sum(i),
                              replace = TRUE)
    i <- check_n(out,
                 interval = c(new_interval1, new_interval2),
                 n_to_exclude = n_to_exclude,
                 extendInt = extendInt)
    if (isFALSE(any(i))) {
      # All ns OK
      break
    }
  }
  if (any(i)) {
    # Have to do random sample
    n_pool <- setdiff(seq(new_interval1, new_interval2),
                      c(n_to_exclude, out[!i]))
    n_new <- sample(n_pool, size = sum(i))
    out[i] <- n_new
  }
  return(out)
}

check_n <- function(ns,
                    interval,
                    n_to_exclude,
                    extendInt,
                    hard_min = 5) {
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

  i[ns < hard_min] <- TRUE

  i
}
