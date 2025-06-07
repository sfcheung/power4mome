#' @noRd
power_algorithm_bisection <- function(object,
                                      x,
                                      pop_es_name = NULL,
                                      ...,
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
                                      power_model = NULL,
                                      power_curve_start = NULL,
                                      lower_bound = NULL,
                                      upper_bound = NULL,
                                      nls_control = list(),
                                      nls_args = list(),
                                      save_sim_all = FALSE,
                                      start = mean(x_interval),
                                      by_x_1 = by_x_1,
                                      fit_1 = NULL,
                                      ci_hit = NULL,
                                      is_by_x = FALSE,
                                      solution_found = FALSE,
                                      digits = 3,
                                      lower_hard = switch(x, n = 10, es = 0),
                                      upper_hard = switch(x, n = 10000, es = .999),
                                      extend_maxiter = 3,
                                      what = c("point", "ub", "lb"),
                                      goal = c("ci_hit", "close_enough"),
                                      tol = .02) {
  extendInt <- match.arg(extendInt)
  what <- match.arg(what)
  goal <- match.arg(goal)

  x_type <- x

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
                     what = what,
                     simulation_progress = simulation_progress,
                     save_sim_all = save_sim_all,
                     store_output = TRUE)

  # Find f.lower and f.upper
  by_x_ci <- rejection_rates_add_ci(by_x_1,
                                    level = ci_level)
  x_tried <- get_x_tried(by_x_ci,
                         x = x)
  reject_tried <- by_x_ci$reject
  lower <- min(x_interval)
  upper <- max(x_interval)

  if (progress) {
    cat("\n")
    print_interval(lower = lower,
                   upper = upper,
                   digits = digits,
                   x_type = x_type,
                   prefix = "Initial interval:")
    cat("\n")
  }


  tmp <- in_x_tried(lower,
                    object = by_x_ci,
                    x = x)
  if (!is.na(tmp)) {
    output_tmp <- by_x_1[tmp]
    class(output_tmp) <- class(by_x_1)
    f.lower <- f(x_i = lower,
                 power_i = reject_tried[tmp],
                 progress = FALSE)
    attr(f.lower, "output") <- output_tmp
  } else {
    if (progress) {
      cat("\nDo the simulation for the lower bound:\n")
    }
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
    by_x_1 <- c(by_x_1, attr(f.lower, "output"),
                skip_checking_models = TRUE)
  }

  tmp <- in_x_tried(upper,
                    object = by_x_ci,
                    x = x)
  if (!is.na(tmp)) {
    output_tmp <- by_x_1[tmp]
    class(output_tmp) <- class(by_x_1)
    f.upper <- f(x_i = upper,
                 power_i = reject_tried[tmp],
                 progress = FALSE)
    attr(f.upper, "output") <- output_tmp
  } else {
    if (progress) {
      cat("\nDo the simulation for the upper bound:\n")
    }
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
    by_x_1 <- c(by_x_1, attr(f.upper, "output"),
                skip_checking_models = TRUE)
  }

  if (progress) {
    cat("\n")
    print_interval(lower = lower,
                   upper = upper,
                   digits = digits,
                   x_type = x_type,
                   prefix = "Initial interval:")
    cat("\n")
    cat("- Rejection Rates:\n")
    tmp <- rejection_rates(by_x_1)
    print(tmp, annotation = FALSE)
    cat("\n")
  }

  do_search <- TRUE

  # - Check whether lower or upper is already a solution
  output_lower <- attr(f.lower, "output")
  reject_lower <- rejection_rates(output_lower)$reject
  ok_lower <- bisection_check_solution(f_i = reject_lower,
                                       target_power = target_power,
                                       nrep = final_nrep,
                                       ci_level = ci_level,
                                       what = what,
                                       goal = goal,
                                       tol = tol)
  output_upper <- attr(f.upper, "output")
  reject_upper <- rejection_rates(output_upper)$reject
  ok_upper <- bisection_check_solution(f_i = reject_upper,
                                       target_power = target_power,
                                       nrep = final_nrep,
                                       ci_level = ci_level,
                                       what = what,
                                       goal = goal,
                                       tol = tol)

  if (ok_lower || ok_upper) {
    # One of them is a solution. No need to check the interval,
    # even if the interval is below or above the root.
    do_search <- FALSE
  } else {
    # Fix the interval
    # The original interval is returned if it is OK
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
                                        store_output = TRUE,
                                        overshoot = switch(x,
                                                           n = .5,
                                                           es = .05))

    # if (interval_updated$extend_status == 0) {
    lower <- interval_updated$lower
    upper <- interval_updated$upper
    # The outputs of lower and upper always in by_x_1
    f.lower <- interval_updated$f.lower
    if (is.null(attr(f.lower, "output"))) {
      tmp <- in_x_tried(test_x = lower,
                        object = by_x_1,
                        x = x)
      tmp <- by_x_1[tmp]
      class(tmp) <- class(by_x_1)
      attr(f.lower, "output") <- tmp
    }
    f.upper <- interval_updated$f.upper
    if (is.null(attr(f.upper, "output"))) {
      tmp <- in_x_tried(test_x = upper,
                        object = by_x_1,
                        x = x)
      tmp <- by_x_1[tmp]
      class(tmp) <- class(by_x_1)
      attr(f.upper, "output") <- tmp
    }
    if ((start <= lower) || (start >= upper)) {
      start <- mean(c(lower, upper))
    }

    tmp <- in_x_tried(test_x = lower,
                      object = by_x_1,
                      x = x)
    if (is.na(tmp)) {
      by_x_1 <- c(by_x_1, attr(interval_updated$f.lower, "output"),
                  skip_checking_models = TRUE)
    }

    tmp <- in_x_tried(test_x = upper,
                      object = by_x_1,
                      x = x)
    if (is.na(tmp)) {
      by_x_1 <- c(by_x_1, attr(interval_updated$f.upper, "output"),
                  skip_checking_models = TRUE)
    }

    # } else {
    #   # Interval not OK.
    #   # However, it is possible that
    #   # one of the bounds is a solution.
    # }

    # - Check whether the updated lower or upper is already a solution
    output_lower <- attr(f.lower, "output")
    reject_lower <- rejection_rates(output_lower)$reject
    ok_lower <- bisection_check_solution(f_i = reject_lower,
                                        target_power = target_power,
                                        nrep = final_nrep,
                                        ci_level = ci_level,
                                        what = what,
                                        goal = goal,
                                        tol = tol)
    output_upper <- attr(f.upper, "output")
    reject_upper <- rejection_rates(output_upper)$reject
    ok_upper <- bisection_check_solution(f_i = reject_upper,
                                        target_power = target_power,
                                        nrep = final_nrep,
                                        ci_level = ci_level,
                                        what = what,
                                        goal = goal,
                                        tol = tol)

    if ((interval_updated$extend_status != 0) &&
        (!ok_lower && !ok_upper)) {
      # Interval not OK and no bounds are the solution
      if (progress) {
        cat("\n")
        cat(names(interval_updated$extend_status), ".\n")
        cat("None of the bounds are solution.\n")
        cat("Try another interval.\n\n")
        # Should quit
        do_search <- FALSE
      }
    }

  }

  if (ok_lower || ok_upper) {
    if (progress) {
      cat("One of the bounds in the interval is already a solution.\n\n")
    }
    do_search <- FALSE
    ci_hit <- switch(goal,
                     ci_hit = TRUE,
                     close_enough = NA)
    solution_found <- TRUE
    if (ok_lower) {
      x_i <- lower
      out_i <- f.lower
      output_i <- output_lower
      reject_i <- reject_lower
    } else {
      x_i <- upper
      out_i <- f.upper
      output_i <- output_upper
      reject_i <- reject_upper
    }
    # by_x_1 <- c(by_x_1, output_i)
    # No need. lower and upper always in by_x_1
  }

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
        cat("\nIteration #", i, "\n")
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
      by_x_1 <- c(by_x_1, output_i,
                  skip_checking_models = TRUE)
      reject_i <- rejection_rates(output_i)$reject

      if (progress) {
        cat("- Rejection Rates:\n")
        tmp <- rejection_rates(by_x_1)
        print(tmp, annotation = FALSE)
        cat("\n")
      }

      # TODO:
      # - Check NA, error, etc.
      # Convergence?
      ok <- bisection_check_solution(f_i = reject_i,
                                     target_power = target_power,
                                     nrep = final_nrep,
                                     ci_level = ci_level,
                                     what = what,
                                     goal = goal,
                                     tol = tol)
      if (ok) {
        ci_hit <- switch(goal,
                         ci_hit = TRUE,
                         close_enough = NA)
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
      if (x_type == "n") {
        x_i <- ceiling(x_i)
      }
      if (progress) {
        print_interval(lower = lower_i,
                       upper = upper_i,
                       digits = digits,
                       x_type = x_type)
        cat("Updated x:", x_i, "\n")
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
  # TRUE if an acceptable solution is found

  if (!do_search &&
      (!ok_lower && !ok_upper)) {
    x_out <- NA
    power_out <- NA
    nrep_out <- NA
    ci_out <- NA
    by_x_out <- NA
    i2 <- NA
  } else {
    # Store the last results,
    # regardless of solution
    out_i <- as.numeric(out_i)
    by_ci_i <- rejection_rates_add_ci(output_i,
                                      level = ci_level)
    ci_i <- unlist(by_ci_i[1, c("reject_ci_lo", "reject_ci_hi")])
    x_tried <- get_x_tried(by_x_1,
                           x = x)
    i2 <- match(x_i, x_tried)
    by_x_ci <- rejection_rates_add_ci(by_x_1,
                                      level = ci_level)
    x_out <- x_i
    power_out <- unlist(by_ci_i[1, "reject"])
    nrep_out <- unlist(by_ci_i[1, "nrep"])
    ci_out <- ci_i
    by_x_out <- by_ci_i
  }

  # Available regardless of do_search

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

  if (progress) {
    if (solution_found) {
      cat("Solution found.\n")
    } else {
      cat("Solution not found.\n")
    }
  }

  # The final x_i are always returned if available
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
              solution_found = solution_found,
              tol = tol)
  out

}

#' @noRd
extend_interval <- function(f,
                            ...,
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
  if (trace) {
    cat("\n\n== Enter extending interval ...\n\n")
  }
  status_msg <- c("Interval OK" = 0,
                  "Interval not OK but extendInd is no" = 1,
                  "Interval above the solution but extendInd is not yes or downX" = 2,
                  "Interval below the solution but extendInd is not yes or upX" = 3,
                  "Interval above the solution but the lower bound hits lower_hard" = 4,
                  "Interval above the solution but the upper bound hits upper_hard" = 5,
                  "Interval not OK but extend_maxiter reached" = 6)
  extend_status <- NA
  args <- list(...)
  # x is always supplied
  x_type <- args$x
  if (sign(f.lower) != sign(f.upper)) {
    # No need to extend
    if (trace) {
      cat("\n\n== Exist extending interval ...\n\n")
    }
    return(list(lower = lower,
                upper = upper,
                f.lower = f.lower,
                f.upper = f.upper,
                interval_ok = TRUE,
                extend_status = status_msg[status_msg == 0],
                extendInt = extendInt))
  }
  if (extendInt == "no") {
    if (trace) {
      cat("\n\n== Exist extending interval ...\n\n")
    }
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
    if (trace) {
      cat("\n\n== Exist extending interval ...\n\n")
    }
    return(list(lower = lower,
                upper = upper,
                f.lower = f.lower,
                f.upper = f.upper,
                interval_ok = FALSE,
                extend_status = status_msg[status_msg == 2],
                extendInt = extendInt))
  }
  if (extend_up && (!(extendInt %in% c("yes", "upX")))) {
    if (trace) {
      cat("\n\n== Exist extending interval ...\n\n")
    }
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
          cat("\n\n(Extending the interval) Iteration:", i, "\n\n")
          print_interval(lower = lower,
                         upper = upper,
                         digits = digits,
                         x_type = x_type)
        }
        f.lower <- do.call(f,
                           c(list(x_i = lower),
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
          cat(names(extend_status), ".\n\n", sep = "")
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
          cat("\n\n(Extending the interval) Iteration:", i, "\n")
          print_interval(lower = lower,
                         upper = upper,
                         digits = digits,
                         x_type = x_type)
        }
        f.upper <- do.call(f,
                           c(list(x_i = upper),
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

  if (trace) {
    print_interval(lower = lower,
                   upper = upper,
                   digits = digits,
                   x_type = x_type,
                   prefix = "Final extended interval:")
  }

  if (trace) {
    cat("\n\n== Exist extending interval ...\n\n")
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
                ...,
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
                store_output = TRUE,
                out_i = NULL,
                power_i = NULL) {
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
      cat("\nTry x =", tmp, "\n")
    }

    # If out_i is supplied
    # it will be used and many arguments will be ignored.
    # If power_i is supplied, even out_i will be ignored
    # Convenient for using f() to compute the function value
    # But be careful of conflicting arguments.

    if (is.null(out_i) && is.null(power_i)) {
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
    } else {
      if (is.null(power_i)) {
        # power_i is supplied. Ignore out_i
        power_i <- rejection_rates(out_i)$reject
        out_i <- NULL
      } else {
        # out_i is supplied
        # power_i will be used
      }
    }

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
                     ub = ci_i[2] - target_power,
                     lb = ci_i[1] - target_power)
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

#' @noRd
bisection_check_solution <- function(f_i,
                                     target_power = .80,
                                     nrep = 10,
                                     ci_level = .95,
                                     what = c("point", "ub", "lb"),
                                     tol = 1e-2,
                                     goal = c("ci_hit", "close_enough")) {
  goal <- match.arg(goal)
  a <- abs(stats::qnorm((1 - ci_level) / 2))
  se_i <- sqrt(f_i * (1 - f_i) / nrep)
  cilb <- f_i - a * se_i
  ciub <- f_i + a * se_i

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
power_algorithm_bisection_pre_i <- function(object,
                                            x,
                                            pop_es_name,
                                            target_power,
                                            xs_per_trial,
                                            x_max,
                                            x_min,
                                            nrep0,
                                            R0,
                                            progress,
                                            x_include_interval,
                                            x_interval,
                                            simulation_progress,
                                            save_sim_all,
                                            is_by_x,
                                            object_by_org,
                                            power_model,
                                            start,
                                            lower_bound,
                                            upper_bound,
                                            nls_control,
                                            nls_args,
                                            final_nrep,
                                            nrep_steps,
                                            final_R) {
  # TODO:
  # - Make use of by_* object's results.
  # This method only needs an initial interval
  x_i <- set_x_range(object,
                    x = x,
                    pop_es_name = pop_es_name,
                    target_power = target_power,
                    k = 2,
                    x_max = x_max,
                    x_min = x_min)
  # For bisection, no need to exclude the value in the input objects
  # Exclude the value in the input object
  x0 <- switch(x,
              n = attr(object, "args")$n,
              es = pop_es(object,
                          pop_es_name = pop_es_name))
  # x_i <- setdiff(x_i, x0)

  # For bisection, only two initial values are needed

  if (x_include_interval) {
    # For bisecition, should exclude them initially,
    # and lest extend_interval() to do the job
    x_i <- sort(c(x_interval, x_i))
  }
  x_i <- sort(unique(x_i))
  if (length(x_i) == 1) {
    x_interval_min <- min(x_interval)
    x_interval_max <- max(x_interval)
    if ((x_i < x_interval_min) ||
        (x_i > x_interval_max)) {
      x_i <- x_interval
    } else {
      tmp <- mean(x_interval)
      if (x_i < tmp) {
        x_i <- c(x_i, x_interval_max)
      } else if (x_i > tmp) {
        x_i <- c(x_i, x_interval_min)
      } else {
        x_i <- c(mean(c(x_i, x_interval_min)),
                 mean(c(x_i, x_interval_max)))
        x_i <- range(x_i)
      }
    }
  } else {
    x_i <- range(x_i)
  }

  # if (progress) {
  #   x_i_str <- formatC(x_i,
  #                     digits = switch(x, n = 0, es = 4),
  #                     format = "f")
  #   cat("- Value(s) to try: ",
  #       paste0(x_i_str, collapse = ", "),
  #       "\n")
  # }

  # Let the algorithm to do the simulation for the bounds
  # ** by_x_i **
  # The current (i-th) set of values examined,
  # along with their results.
  # by_x_i <- switch(x,
  #                 n = power4test_by_n(object,
  #                                     n = x_i,
  #                                     nrep = nrep0,
  #                                     R = R0,
  #                                     progress = simulation_progress,
  #                                     save_sim_all = save_sim_all),
  #                 es = power4test_by_es(object,
  #                                       pop_es_name = pop_es_name,
  #                                       pop_es_values = x_i,
  #                                       nrep = nrep0,
  #                                       R = R0,
  #                                       progress = simulation_progress,
  #                                       save_sim_all = save_sim_all))

  # Add the input object to the list
  # if (is_by_x) {
  #   # Object is an output of *_by_n() or *_by_es()
  #   by_x_i <- c(by_x_i,
  #               object_by_org,
  #               skip_checking_models = TRUE)
  # } else {
  by_x_i <- list(object)
  # TODO:
  # - Write an ax.....by_* function.
  if (x == "n") {
    class(by_x_i) <- c("power4test_by_n", class(by_x_i))
    names(by_x_i) <- as.character(x0)
  }
  if (x == "es") {
    class(by_x_i) <- c("power4test_by_es", class(by_x_i))
    names(by_x_i) <- paste0(pop_es_name,
                        " = ",
                          as.character(x0))
    attr(by_x_i[[1]], "pop_es_name") <- pop_es_name
    attr(by_x_i[[1]], "pop_es_value") <- x0
  }
  # by_x_i <- c(by_x_i, by_x_i,
  #             skip_checking_models = TRUE)
  # }

  # if (progress) {
  #   cat("- Rejection Rates:\n")
  #   tmp <- rejection_rates(by_x_i)
  #   print(tmp)
  #   cat("\n")
  # }

  # ** fit_i **
  # The current power curve, based on by_x_i
  # fit_i <- power_curve(by_x_i,
  #                     formula = power_model,
  #                     start = start,
  #                     lower_bound = lower_bound,
  #                     upper_bound = upper_bound,
  #                     nls_control = nls_control,
  #                     nls_args = nls_args,
  #                     verbose = progress)

  # if (progress) {
  #   cat("- Power Curve:\n")
  #   # Can use the print method of power_curve objects
  #   print(fit_i)
  #   cat("\n")
  # }

  # ** by_x_1 **
  # The collection of all values tried and their results
  # to be updated when new value is tried.
  # Used after the end of the loop.
  by_x_1 <- by_x_i

  # ** fit_1 **
  # The latest power curve
  # To be updated whenever by_x_1 is updated.
  # Used after the end of the loop.
  # fit_1 <- fit_i
  fit_1 <- NULL

  # TODO:
  # - Adaptive nrep for bisection

  # === Initialize the Sequences ===
  # The sequence will be updated when nrep_step is initiated,
  # to successively increase precision and speed by
  # - increasing the number of replication,
  # - increasing the number of resampling, and
  # - decreasing the number of values to try.

  # The sequence of the numbers of replication
  # new_nrep <- rejection_rates(by_x_1,
  #                             all_columns = TRUE)$nrep

  # new_nrep <- ceiling(mean(new_nrep))
  # new_nrep <- nrep0
  # nrep_seq <- ceiling(seq(from = new_nrep,
  #                         to = final_nrep,
  #                         length.out = nrep_steps + 1))
  # final_nrep_seq <- ceiling(seq(from = ceiling(mean(c(new_nrep, final_nrep))),
  #                               to = final_nrep,
  #                               length.out = nrep_steps + 1))

  # The sequence of the Rs (for boot and MC CI)
  # R0 <- attr(object, "args")$R
  # if (!is.null(R0)) {
  #   R_seq <- ceiling(seq(from = R0,
  #                       to = final_R,
  #                       length.out = nrep_steps + 1))
  # } else {
  #   R_seq <- NULL
  # }

  # # The sequence of the numbers of values per trial
  # xs_per_trial_seq <- ceiling(seq(from = xs_per_trial,
  #                                 to = 2,
  #                                 length.out = nrep_steps + 1))

  out <- list(x_i = NULL,
              by_x_i = NULL,
              fit_i = NULL,
              by_x_1 = by_x_1,
              fit_1 = fit_1,
              nrep_seq = NULL,
              final_nrep_seq = NULL,
              R_seq = NULL,
              xs_per_trial_seq = NULL,
              x_interval_updated = x_i)

  return(out)
}