#' @noRd
power_algorithm_bisection <- function(object,
                                      x,
                                      pop_es_name = NULL,
                                      target_power = .80,
                                      ci_level = .95,
                                      x_interval = switch(x, n = c(100, 1000),
                                                             es = c(.10, .50)),
                                      extendInt = c("no", "yes", "downX", "upX"),
                                      progress = TRUE,
                                      simulation_progress = TRUE,
                                      max_trials = 10,
                                      final_nrep = 400,
                                      R = NULL,
                                      start = mean(x_interval),
                                      by_x_1 = by_x_1,
                                      fit_1 = NULL,
                                      ci_hit = NULL,
                                      save_sim_all = FALSE,
                                      solution_found = FALSE,
                                      digits = 3,
                                      lower_hard = switch(x, n = 10, es = 0),
                                      upper_hard = switch(x, n = 10000, es = .999),
                                      power_model = NULL,
                                      power_curve_start = NULL,
                                      lower_bound = NULL,
                                      upper_bound = NULL,
                                      nls_control = list(),
                                      nls_args = list(),
                                      extend_maxiter = 3,
                                      what = c("point", "ub", "lb")) {
  extendInt <- match.arg(extendInt)
  what <- match.arg(what)
  x_type <- x

  a <- abs(stats::qnorm((1 - ci_level) / 2))
  power_tolerance_in_interval <- a * sqrt(target_power * (1 - target_power) / final_nrep)
  power_tolerance_in_final <- a * sqrt(target_power * (1 - target_power) / final_nrep)

  # Create the objective function
  f <- gen_objective(object = object,
                     x = x,
                     pop_es_name = pop_es_name,
                     target_power = target_power,
                     ci_level = ci_level,
                     progress = progress,
                     digits = digits,
                     nrep = final_nrep,
                     R = R,
                     what = "point",
                     simulation_progress = simulation_progress,
                     save_sim_all = save_sim_all,
                     store_output = TRUE)

  # Find f.lower and f.upper
  by_x_ci <- rejection_rates_add_ci(by_x_1,
                                    level = ci_level)
  x_tried <- switch(x,
                    n = by_x_1$n,
                    es = by_x_1$es)
  reject_tried <- by_x_1$reject
  lower <- min(x_interval)
  upper <- max(x_interval)
  tmp <- match(lower, x_tried)
  if (!is.na(tmp)) {
    f.lower <- reject_tried[tmp]
  } else {
    f.lower <- f(x_i = lower,
                 x = x,
                 pop_es_name = pop_es_name,
                 target_power = target_power,
                 ci_level = ci_level,
                 progress = progress,
                 digits = digits,
                 nrep = final_nrep,
                 R = R,
                 what = what,
                 simulation_progress = simulation_progress,
                 save_sim_all = save_sim_all,
                 store_output = TRUE)
    by_x_1 <- c(by_x_1, attr(f.lower, "output"))
  }
  tmp <- match(upper, x_tried)
  if (!is.na(tmp)) {
    f.upper <- reject_tried[tmp]
  } else {
    f.upper <- f(x_i = upper,
                 x = x,
                 pop_es_name = pop_es_name,
                 target_power = target_power,
                 ci_level = ci_level,
                 progress = progress,
                 digits = digits,
                 nrep = final_nrep,
                 R = R,
                 what = what,
                 simulation_progress = simulation_progress,
                 save_sim_all = save_sim_all,
                 store_output = TRUE)
    by_x_1 <- c(by_x_1, attr(f.upper, "output"))
  }

  # TODO:
  # - Check whether lower or upper is already a solution

  do_search <- TRUE

  # Fix the interval
  interval_updated <- extend_interval(f = f,
                                      x = x,
                                      pop_es_name = pop_es_name,
                                      target_power = target_power,
                                      ci_level = ci_level,
                                      nrep = final_nrep,
                                      R = R,
                                      what = what,
                                      simulation_progress = simulation_progress,
                                      save_sim_all = save_sim_all,
                                      progress = progress,
                                      x_type = x_type,
                                      lower = lower,
                                      upper = upper,
                                      f.lower = f.lower,
                                      f.upper = f.upper,
                                      lower_hard = lower_hard,
                                      upper_hard = upper_hard,
                                      extendInt = extendInt,
                                      extend_maxiter = extend_maxiter,
                                      trace = as.numeric(progress),
                                      digits = digits,
                                      store_output = TRUE)

  if (interval_updated$extend_status == 0) {
    if (lower != interval_updated$lower) {
      by_x_1 <- c(by_x_1, attr(interval_updated$f.lower, "output"))
    }
    if (upper != interval_updated$upper) {
      by_x_1 <- c(by_x_1, attr(interval_updated$f.upper, "output"))
    }
    lower <- interval_updated$lower
    upper <- interval_updated$upper
    f.lower <- interval_updated$f.lower
    f.upper <- interval_updated$f.upper
    if ((start <= lower) || (start >= upper)) {
      start <- mean(c(lower, upper))
    }
  } else {
    # Interval not OK. Prepare to quit
    do_search <- FALSE
  }

  # TODO:
  # - Check whether lower or upper is already a solution

  if (do_search) {
    x_i <- start
    if (x_type == "n") {
      x_i <- ceiling(x_i)
    }
    f.lower_i <- f.lower
    f.upper_i <- f.upper
    lower_i <- lower
    upper_i <- upper
    status <- 0

    i <- 1
    while (i <= max_trials) {
      if (progress) {
        cat("\nIteration #", i, "\n\n")
      }
      out_i <- f(x_i = x_i,
                 x = x,
                 pop_es_name = pop_es_name,
                 target_power = target_power,
                 ci_level = ci_level,
                 progress = progress,
                 digits = digits,
                 nrep = final_nrep,
                 R = R,
                 what = what,
                 simulation_progress = simulation_progress,
                 save_sim_all = save_sim_all,
                 store_output = TRUE)
      output_i <- attr(out_i, "output")
      by_x_1 <- c(by_x_1, output_i)
      out_i <- as.numeric(out_i)
      # TODO:
      # - Check NA, error, etc.
      # Convergence?
      by_ci_i <- rejection_rates_add_ci(output_i,
                                        level = ci_level)
      ci_i <- unlist(by_ci_i[1, c("reject_ci_lo", "reject_ci_hi")])
      ci_lower_i <- ci_i[1]
      ci_upper_i <- ci_i[2]
      if ((ci_lower_i < target_power) &&
          (ci_upper_i > target_power)) {
        ci_hit <- TRUE
        solution_found <- TRUE
        break
      }
      # Update interval
      if (x_type == "n") {
        x_i <- ceiling(x_i)
      }
      if (sign(out_i) == sign(f.lower_i)) {
        lower_i <- x_i
        f.lower_i <- out_i
      } else {
        upper_i <- x_i
        f.upper_i <- out_i
      }
      x_i <- mean(c(lower_i, upper_i))
      if (progress) {
        print_interval(lower = lower_i,
                       upper = upper_i,
                       digits = digits,
                       x_type = x_type)
      }
      i <- i + 1
    }
  } else {
    # No iteration
  }

  # Prepare the output

  # ** by_x_1 **
  # The collection of all values tried and their results
  # to be updated when new value is tried.
  # Used after the end of the loop.

  # ** fit_1 **
  # The latest power curve
  # To be updated whenever by_x_1 is updated.
  # Used after the end of the loop.

  # ** x_tried **
  # The sample sizes or population values
  # examined.

  # ** x_out **
  # The output in the last trial.

  # ** power_out **
  # The power in the last trial.

  # ** nrep_out **
  # The nrep in the last trial.

  # ** ci_out **
  # The ci in the last trial.

  # ** by_x_out **
  # The by_* output in the last trial

  # ** i2 **
  # The row number of the last trial
  # in by_x_1.

  # ** solution_found **
  # TRUE is an acceptable solution is found

  fit_1 <- power_curve(by_x_1,
                       formula = power_model,
                       start = power_curve_start,
                       lower_bound = lower_bound,
                       upper_bound = upper_bound,
                       nls_control = nls_control,
                       nls_args = nls_args,
                       verbose = progress)

  x_tried <- switch(x,
                    n = as.numeric(names(by_x_1)),
                    es = sapply(by_x_1,
                                \(x) {attr(x, "pop_es_value")},
                                USE.NAMES = FALSE))
  i2 <- match(x_i, x_tried)
  by_x_ci <- rejection_rates_add_ci(by_x_1,
                                    level = ci_level)
  x_out <- x_i
  power_out <- unlist(by_ci_i[1, "reject"])
  nrep_out <- unlist(by_ci_i[1, "nrep"])
  ci_out <- ci_i
  by_x_out <- by_ci_i

  # The final x_i are always returned.
  # ci_hit is used to decide whether
  # final x_i is a solution.
  out <- list(by_x_1 = by_x_1,
              fit_1 = fit_1,
              ci_hit = ci_hit,
              x_tried = x_tried,
              x_out = x_out,
              power_out = power_out,
              nrep_out = nrep_out,
              ci_out = ci_out,
              by_x_out = by_x_out,
              i2 = i2,
              solution_found = solution_found)
  out

}

#' @noRd
extend_interval <- function(f,
                            ...,
                            x_type,
                            lower,
                            upper,
                            f.lower = f(lower, ...),
                            f.upper = f(upper, ...),
                            lower_hard,
                            upper_hard,
                            extendInt = c("no", "yes", "downX", "upX"),
                            extend_maxiter = 3,
                            trace = 0,
                            digits = 3,
                            by_x_1 = NULL,
                            overshoot = .5) {
  status_msg <- c("Interval OK" = 0,
                  "Interval not OK but extendInd is no" = 1,
                  "Interval above the solution but extendInd is not yes or downX" = 2,
                  "Interval below the solution but extendInd is not yes or upX" = 3,
                  "Interval above the solution but the lower bound hits lower_hard" = 4,
                  "Interval above the solution but the upper bound hits upper_hard" = 5,
                  "Interval not OK but extend_maxiter reached" = 6)
  extend_status <- NA
  args <- list(...)
  if (sign(f.lower) != sign(f.upper)) {
    # No need to extend
    return(list(lower = lower,
                upper = upper,
                f.lower = f.lower,
                f.upper = f.upper,
                interval_ok = TRUE,
                extend_status = status_msg[status_msg == 0],
                extendInt = extendInt))
  }
  if (extendInt == "no") {
    return(list(lower = lower,
                upper = upper,
                f.lower = f.lower,
                f.upper = f.upper,
                interval_ok = FALSE,
                extend_status = status_msg[status_msg == 1],
                extendInt = extendInt))
  }
  slope <- (f.upper - f.lower) / (upper - lower)
  intercept <- -slope * lower +  f.lower
  # TODO:
  # - Need to optimize the code to reduce duplications
  extend_up <- ((slope > 0) && (f.upper < 0)) ||
                ((slope < 0) && (f.upper > 0))
  extend_down <- ((slope > 0) && (f.upper > 0)) ||
                  ((slope < 0) && (f.upper < 0))
  if (extend_down && (!(extendInt %in% c("yes", "downX")))) {
    return(list(lower = lower,
                upper = upper,
                f.lower = f.lower,
                f.upper = f.upper,
                interval_ok = FALSE,
                extend_status = status_msg[status_msg == 2],
                extendInt = extendInt))
  }
  if (extend_up && (!(extendInt %in% c("yes", "upX")))) {
    return(list(lower = lower,
                upper = upper,
                f.lower = f.lower,
                f.upper = f.upper,
                interval_ok = FALSE,
                extend_status = status_msg[status_msg == 3],
                extendInt = extendInt))
  }
  # TODO:
  # - Need to optimize the code to reduce duplications
  interval_ok <- FALSE
  if ((extendInt %in% c("yes", "downX")) && extend_down) {
    # Extend lower by simple linear extrapolation
    if (trace) {
      cat("Interval above the solution. Extend the lower bound ...\n")
    }
    i <- 1
    while ((i <= extend_maxiter) &&
            (sign(f.lower) == sign(f.upper))) {
      if (lower == lower_hard) {
        # Hit the hard lower limit
        interval_ok <- FALSE
        extend_status <- status_msg[status_msg == 4]
        if (trace) {
          cat(names(extend_status), ".\n", sep = "")
        }
        break
      } else {
        slope <- (f.upper - f.lower) / (upper - lower)
        intercept <- -slope * upper +  f.upper
        upper <- lower
        f.upper <- f.lower
        lower <- overshoot * -intercept / slope
        if (x_type == "n") {
          lower <- ceiling(lower)
        }
        lower <- max(lower, lower_hard)
        if (trace) {
          cat("Iteration:", i, "\n")
          print_interval(lower = lower,
                         upper = upper,
                         digits = digits,
                         x_type = x_type)
        }
        f.lower <- do.call(f,
                           c(list(x_i = lower),
                             list(x = x_type),
                             args))
        i <- i + 1
      }
    }
  }
  # Should have exhausted all possibilities
  if ((extendInt %in% c("yes", "upX")) && extend_up) {
    # Extend upper by simple linear extrapolation
    if (trace) {
      cat("Interval below the solution. Extend the upper bound ...\n")
    }
    i <- 1
    while ((i <= extend_maxiter) &&
            (sign(f.lower) == sign(f.upper))) {
      if (upper == upper_hard) {
        # Hit the hard upper limit
        interval_ok <- FALSE
        extend_status <- status_msg[status_msg == 5]
        if (trace) {
          cat(names(extend_status), ".\n", sep = "")
        }
        break
      } else {
        slope <- (f.upper - f.lower) / (upper - lower)
        intercept <- -slope * upper +  f.upper
        lower <- upper
        f.lower <- f.upper
        upper <- (1 + overshoot) * -intercept / slope
        if (x_type == "n") {
          upper <- ceiling(upper)
        }
        upper <- min(upper, upper_hard)
        if (trace) {
          cat("Iteration:", i, "\n")
          print_interval(lower = lower,
                         upper = upper,
                         digits = digits,
                         x_type = x_type)
        }
        f.upper <- do.call(f,
                           c(list(x_i = upper),
                             list(x = x_type),
                             args))
        i <- i + 1
      }
    }
  }
  interval_ok <- sign(f.lower) != sign(f.upper)
  if (!interval_ok) {
    if (is.na(extend_status)) {
      # extend_maxiter reached
      extend_status <- status_msg[status_msg == 6]
      if (trace) {
        cat(names(extend_status), ".\n", sep = "")
      }
    }
  } else {
    extend_status <- status_msg[status_msg == 0]
    if (trace) {
      cat(names(extend_status), ".\n", sep = "")
    }
  }
  return(list(lower = lower,
              upper = upper,
              f.lower = f.lower,
              f.upper = f.upper,
              interval_ok = interval_ok,
              extend_status = extend_status,
              extendInt = extendInt))
}

#' @noRd
print_interval <- function(lower,
                           upper,
                           digits,
                           x_type,
                           prefix = "New interval:") {
  tmp <- formatC(c(lower, upper),
                  digits = switch(x_type,
                                  n = 0,
                                  es = digits),
                  format = "f")
  tmp <- paste0("[",paste(tmp, collapse = ", "), "]")
  cat(prefix, tmp, "\n")
}

#' @noRd
gen_objective <- function(object,
                          x,
                          pop_es_name,
                          target_power,
                          ci_level,
                          progress,
                          digits,
                          nrep,
                          R,
                          what = c("point", "ub", "lb"),
                          simulation_progress,
                          save_sim_all,
                          store_output) {
  what <- match.arg(what)
  # Create the objective function
  f <- function(x_i,
                x = x,
                pop_es_name = NULL,
                target_power = .80,
                ci_level = .95,
                progress = TRUE,
                digits = 3,
                nrep = 10,
                R = NULL,
                what = "point",
                simulation_progress = TRUE,
                save_sim_all = FALSE,
                store_output = TRUE) {
    if (x == "n") {
      x_i <- ceiling(x_i)
    }
    # TODO:
    # - Adaptive nrep?
    # Static nrep
    if (progress) {
      tmp <- switch(x,
                    n = as.character(x_i),
                    es = formatC(x_i,
                                 digits = digits,
                                 format = "f"))
      cat("\nTrying x =", tmp, "\n\n")
    }
    out_i <- switch(x,
                    n = power4test_by_n(object,
                                        n = x_i,
                                        R = R,
                                        progress = simulation_progress,
                                        by_nrep = nrep,
                                        save_sim_all = save_sim_all),
                    es = power4test_by_es(object,
                                          pop_es_name = pop_es_name,
                                          pop_es_values = x_i,
                                          R = R,
                                          progress = simulation_progress,
                                          by_nrep = nrep,
                                          save_sim_all = save_sim_all))

    power_i <- rejection_rates(out_i)$reject
    a <- abs(stats::qnorm((1 - ci_level) / 2))
    se_i <- sqrt(power_i * (1 - power_i) / nrep)
    ci_i <- power_i + c(-a, a) * se_i

    if (progress) {
      tmp1 <- formatC(power_i, digits = digits, format = "f")
      tmp2 <- paste0("[",
                     paste0(formatC(ci_i, digits = digits, format = "f"),
                            collapse = ","),
                     "]")
      cat("\nEstimated power at ", x, ": ",
          tmp1,
          ", ", formatC(ci_level*100,
                       digits = max(0, digits - 2),
                       format = "f"), "% confidence interval: ",
          tmp2,
          "\n",
          sep = "")
    }

    out2_i <- switch(what,
                     point = power_i - target_power,
                     upper = ci_i[2] - target_power,
                     lower = ci_i[1] - target_power)
    if (store_output) {
      attr(out2_i, "output") <- out_i
    }
    out2_i
  }

  # Set default values
  # TODO:
  # - There should be a better way to do this
  formals(f)$x <- x
  if (!is.null(pop_es_name)) {
    formals(f)$pop_es_name <- pop_es_name
  }
  formals(f)$target_power <- target_power
  formals(f)$ci_level <- ci_level
  formals(f)$progress <- progress
  formals(f)$digits <- digits
  formals(f)$nrep <- nrep
  if (!is.null(R)) {
    formals(f)$R <- R
  }
  formals(f)$what <- what
  formals(f)$simulation_progress <- simulation_progress
  formals(f)$save_sim_all <- save_sim_all
  formals(f)$store_output <- store_output

  f
}