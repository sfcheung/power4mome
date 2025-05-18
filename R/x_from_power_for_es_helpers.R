set_es_range <- function(object,
                         pop_es_name,
                         target_power = .80,
                         k = 4,
                         es_max = .7,
                         es_min = 0) {
  # TODO:
  # - Write a helper to retrieve the current population value
  # - Support single-group model for now.
  es0 <- pop_es(object,
                pop_es_name = pop_es_name)
  es0_sign <- sign(es0)
  es0_abs <- abs(es0)
  reject0 <- get_rejection_rates(object)
  power0 <- reject0$reject[1]
  if (es0 >= es_max) {
    # Use x_max because es_max and es_min are internal arguments
    stop("Initial population value (",
          es0,
          ") is equal to or greater than 'x_max' (",
          es_max,
          "). Please increase 'x_max'.")
  }
  if (es0 <= es_min) {
    # Use x_max because es_max and es_min are internal arguments
    stop("Initial population value (",
          es0,
          ") is equal to or less than 'x_min' (",
          es_min,
          "). Please increase 'x_min'.")
  }

  if (power0 == target_power) {
    # If power0 == target_power,
    # Be conservative and decrease power by a small amount
    power0 <- target_power * .80
  }

  b <- power0 / es0_abs
  es_bound <- ifelse(es0_sign >= 0,
                      abs(es_max),
                      abs(es_min))
  es_end <- min(target_power / b,
                es_bound)
  es_out <- seq(from = es0_abs,
                to = es_end,
                length.out = k)
  if (es0_sign >=0) {
    return(es_out)
  } else {
    es_out <- sort(-es_out)
    return(es_out)
  }

}

#' @noRd
pop_es <- function(object,
                   pop_es_name) {
  # TODO:
  # - Support multigroup models
  ptable <- object$sim_all[[1]]$ptable
  ngroups <- max(ptable$group)
  if (ngroups != 1) {
    stop("Do not support multigroup models for now.")
  }
  ptable$lavlabel <- lavaan::lav_partable_labels(ptable)
  ptable$lavlabel2 <- paste0(ptable$lhs,
                             ptable$op,
                             ptable$rhs)
  pop_es_name0 <- gsub(" ", "", pop_es_name)
  i0 <- match(pop_es_name0, ptable$lavlabel)
  i1 <- match(pop_es_name0, ptable$lavlabel2)
  i <- c(i0, i1)
  i <- unique(i)
  i <- i[!is.na(i)]
  if (length(i) == 0) {
    stop("'pop_es_name' not a valid name of a parameter in the model.")
  }
  out <- ptable$start[i]
  out
}

#' @noRd
estimate_es <- function(power_es_fit,
                        target_power = .80,
                        interval = c(0, .70),
                        extendInt = "no") {
  f <- function(es) {
    stats::predict(power_es_fit,
                   newdata = list(x = es)) - target_power
  }
  es_target <- tryCatch(stats::uniroot(f,
                             interval = interval,
                             extendInt = extendInt),
                       error = function(e) e)
  if (inherits(es_target, "error")) {
    # Return NA if error occurred. E.g.,
    # - Root not in the interval.
    return(NA)
  }
  es_target <- es_target$root
  return(es_target)
}

#' @noRd
estimate_es_range <- function(power_es_fit,
                              target_power = .80,
                              k = 5,
                              tolerance = .20,
                              power_min = .01,
                              power_max = .99,
                              interval = c(0, .70),
                              extendInt = "upX",
                              es_to_exclude = NULL) {
  power_j <- seq(from = max(target_power - tolerance, power_min),
                 to = min(target_power + tolerance, power_max),
                 length.out = k)
  if (isFALSE(target_power %in% power_j)) {
    power_j <- sort(c(power_j, target_power))
  }
  out <- sapply(power_j,
                function(x) {
                  estimate_es(power_es_fit = power_es_fit,
                              target_power = x,
                              interval = interval,
                              extendInt = extendInt)
                })
  out <- ceiling(out)

  # If NA, have to do random sampling
  i <- is.na(out)
  if (any(i)) {
    # Duplication is OK because it will be fixed later
    out[i] <- stats::runif(n = sum(i),
                           min = interval[1],
                           max = interval[2])
  }

  # Check invalid es values

  i <- check_es(ess = out,
                interval = interval,
                es_to_exclude = es_to_exclude,
                extendInt = extendInt)

  if (isFALSE(any(i))) {
    return(out)
  }
  # Replace invalid ESs by random ESs
  # Do not use the full interval
  # But can include ESs already considered
  new_interval1 <- min(es_to_exclude, interval)
  new_interval2 <- max(max(es_to_exclude),
                       interval[2])
  interval_width <- (new_interval2 - new_interval1)
  es_pool <- setdiff(seq(new_interval1,
                         new_interval2,
                         length = 500),
                     c(es_to_exclude, out[!i]))
  for (q in 1:10) {
    out[i] <- out[i] + stats::runif(n = sum(i),
                                    min = -interval_width / 100,
                                    max = interval_width / 100)
    i <- check_es(out,
                  interval = c(new_interval1, new_interval2),
                  es_to_exclude = es_to_exclude,
                  extendInt = extendInt)
    if (isFALSE(any(i))) {
      # All ns OK
      break
    }
  }
  if (any(i)) {
    # Have to do random sample
    es_pool <- setdiff(seq(new_interval1,
                           new_interval2,
                           length = 500),
                       c(es_to_exclude, out[!i]))
    es_new <- sample(es_pool, size = sum(i))
    out[i] <- es_new
  }
  return(out)
}

#' @noRd
check_es <- function(ess,
                     interval,
                     es_to_exclude,
                     extendInt) {
  i <- rep(FALSE, length(ess))
  if (isFALSE(extendInt %in% c("yes", "upX"))) {
    i[ess > interval[2]] <- TRUE
  }
  if (isFALSE(extendInt %in% c("yes", "downX"))) {
    i[ess < interval[1]] <- TRUE
  }

  # ESs used are considered invalid
  i[ess %in% es_to_exclude] <- TRUE

  # Duplicated are considered invalid
  i[duplicated(ess)] <- TRUE

  i[is.na(ess)] <- TRUE

  i
}
