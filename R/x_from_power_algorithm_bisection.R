#' @noRd

alg_bisection <- function(
    object,
    x,
    pop_es_name,
    ...,
    target_power = .80,
    x_max,
    x_min,
    progress,
    x_include_interval,
    x_interval = switch(
                  x,
                  n = c(100, 1000),
                  es = c(.10, .50)),
    simulation_progress,
    save_sim_all,
    is_by_x,
    object_by_org,
    final_nrep,
    final_R,
    ci_level = .95,
    extendInt = c("no", "yes", "downX", "upX"),
    max_trials = 10,
    R = NULL,
    digits = 3,
    lower_hard = switch(x, n = 10, es = 0),
    upper_hard = switch(x, n = 10000, es = .999),
    extend_maxiter = 3,
    what = c("point", "ub", "lb"),
    goal = c("ci_hit", "close_enough"),
    tol = .02,
    delta_tol = switch(x,
                    n = 1,
                    es = .001),
    last_k = 3,
    variants = list()
) {

  # ==== Pre-search setup ====

  a_out <- power_algorithm_bisection_pre_i(
    object = object,
    x = x,
    pop_es_name = pop_es_name,
    target_power = target_power,
    x_max = x_max,
    x_min = x_min,
    progress = progress,
    x_include_interval = x_include_interval,
    x_interval = x_interval,
    simulation_progress = simulation_progress,
    save_sim_all = save_sim_all,
    is_by_x = is_by_x,
    object_by_org = object_by_org,
    final_nrep = final_nrep,
    final_R = final_R,
    what = what,
    goal = goal,
    tol = tol,
    ci_level = ci_level
  )

  # ==== Process output ====

  x_interval_updated <- a_out$x_interval_updated
  by_x_1 <- a_out$by_x_1
  fit_1 <- a_out$fit_1

  # TODO:
  # - Check whether it works
  if (is_by_x) {
    tmp <- setdiff(names(object_by_org), names(by_x_1))
    by_x_1 <- c(by_x_1,
                object_by_org[tmp])
  }

  rm(a_out)

  lower_hard <- min(x_interval)
  upper_hard <- max(x_interval)

  # ==== Start the search ====

  a_out <- power_algorithm_bisection(
    object = object,
    x = x,
    pop_es_name = pop_es_name,
    target_power = target_power,
    ci_level = ci_level,
    x_interval = x_interval_updated,
    extendInt = extendInt,
    progress = progress,
    simulation_progress = simulation_progress,
    max_trials = max_trials,
    final_nrep = final_nrep,
    R = R,
    save_sim_all = save_sim_all,
    by_x_1 = by_x_1,
    fit_1 = fit_1,
    is_by_x = is_by_x,
    digits = digits,
    lower_hard = lower_hard,
    upper_hard = upper_hard,
    what = what,
    goal = goal,
    tol = tol,
    delta_tol = delta_tol,
    last_k = last_k,
    variants = variants
  )

  # ==== Return the output ====

  a_out
}

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
                                      by_x_1 = NULL,
                                      fit_1 = NULL,
                                      is_by_x = FALSE,
                                      digits = 3,
                                      lower_hard = switch(x, n = 10, es = 0),
                                      upper_hard = switch(x, n = 10000, es = .999),
                                      extend_maxiter = 3,
                                      what = c("point", "ub", "lb"),
                                      goal = c("ci_hit", "close_enough"),
                                      tol = .02,
                                      delta_tol = switch(x,
                                                      n = 1,
                                                      es = .001),
                                      last_k = 3,
                                      variants = list()) {
  extendInt <- match.arg(extendInt)
  what <- match.arg(what)
  goal <- match.arg(goal)
  status <- NULL
  changes_ok <- TRUE
  f_history <- vector("numeric", max_trials)
  f_history[] <- NA
  x_history <- vector("numeric", max_trials)
  x_history[] <- NA
  x_interval_history <- matrix(NA,
                               nrow = max_trials,
                               ncol = 2)
  colnames(x_interval_history) <- c("lower", "upper")
  f_interval_history <- matrix(NA,
                               nrow = max_trials,
                               ncol = 2)
  colnames(f_interval_history) <- c("lower", "upper")
  reject_history <- vector("numeric", max_trials)
  reject_history[] <- NA

  i <- NA

  # what: The value to be examined.
  # goal:
  # - ci_hit: Only relevant for what == "point"
  # - close_enough: Can be used for all what.

  x_type <- x

  # Create the objective function

  # Tbe output is:
  # - a scalar,
  # - with the attribute "output" storing the full by_* output.
  # - The scalar depends on 'what'

  # ==== Generate the objective function ====

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

  # ==== Get the initial interval ====

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

  # Compute f.lower and f.upper if not available

  # f.lower

  # ==== Compute f for the lower bound  ====

  tmp <- in_x_tried(lower,
                    object = by_x_ci,
                    x = x)
  if (!is.na(tmp)) {

    # ==== Extract from by_x_1  ====

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

    # ==== Estimate f for the lower bound  ====

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

  # f.upper

  # ==== Compute f for the upper bound  ====

  tmp <- in_x_tried(upper,
                    object = by_x_ci,
                    x = x)
  if (!is.na(tmp)) {

    # ==== Extract from by_x_1  ====

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

    # ==== Estimate f for the upper bound  ====

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

  # ==== Is one of the bound a solution?  ====

  # Check whether lower or upper is already a solution

  output_lower <- attr(f.lower, "output")
  tmp <- rejection_rates(
            output_lower,
            all_columns = TRUE
          )
  reject_lower <- tmp$reject
  nrep_lower <- tmp$nrep
  ok_lower <- check_solution(
                f_i = reject_lower,
                target_power = target_power,
                nrep = nrep_lower,
                ci_level = ci_level,
                final_nrep = final_nrep,
                what = what,
                goal = goal,
                tol = tol
              )
  output_upper <- attr(f.upper, "output")
  tmp <- rejection_rates(
            output_upper,
            all_columns = TRUE
          )
  reject_upper <- tmp$reject
  nrep_upper <- tmp$nrep
  reject_upper <- rejection_rates(output_upper)$reject
  ok_upper <- check_solution(
                f_i = reject_upper,
                target_power = target_power,
                nrep = nrep_upper,
                final_nrep = final_nrep,
                ci_level = ci_level,
                what = what,
                goal = goal,
                tol = tol
              )

  if (ok_lower || ok_upper) {

    # ==== Solution in the interval. Skip the search ====

    # One of them is a solution. No need to check the interval,
    # even if the interval is below or above the root.

    do_search <- FALSE

  } else {

    # ==== Extend the interval if necessary ====

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
                                        by_x_1 = by_x_1,
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
    by_x_1 <- interval_updated$by_x_1 %||% by_x_1
    ci_hit <- FALSE
    solution_found <- FALSE

    lower <- interval_updated$lower
    upper <- interval_updated$upper

    # ==== Update bounds' outputs, if necessary ====

    # The outputs of lower and upper are always in by_x_1

    # Store the output back into f.lower, if not yet stored

    f.lower <- interval_updated$f.lower
    if (is.null(attr(f.lower, "output"))) {
      tmp <- in_x_tried(test_x = lower,
                        object = by_x_1,
                        x = x)
      tmp <- by_x_1[tmp]
      class(tmp) <- class(by_x_1)
      attr(f.lower, "output") <- tmp
    }

    # Store the output back into f.upper, if not yet stored

    f.upper <- interval_updated$f.upper
    if (is.null(attr(f.upper, "output"))) {
      tmp <- in_x_tried(test_x = upper,
                        object = by_x_1,
                        x = x)
      tmp <- by_x_1[tmp]
      class(tmp) <- class(by_x_1)
      attr(f.upper, "output") <- tmp
    }

    # ==== Fix the start value, if necessary ====

    # Fix the start value if it is outside the new interval

    if ((start <= lower) || (start >= upper)) {
      start <- mean(c(lower, upper))
    }

    # ==== Store output(s) to by_x_1, if necessary ====

    # Store the output of f.lower to by_x_1, if not yet stored

    tmp <- in_x_tried(test_x = lower,
                      object = by_x_1,
                      x = x)
    if (is.na(tmp)) {
      by_x_1 <- c(by_x_1, attr(interval_updated$f.lower, "output"),
                  skip_checking_models = TRUE)
    }

    # Store the output of f.upper to by_x_1, if not yet stored

    tmp <- in_x_tried(test_x = upper,
                      object = by_x_1,
                      x = x)
    if (is.na(tmp)) {
      by_x_1 <- c(by_x_1, attr(interval_updated$f.upper, "output"),
                  skip_checking_models = TRUE)
    }

    # ==== Does the updated interval has a solution?  ====

    # Check whether the updated lower or upper is already a solution

    output_lower <- attr(f.lower, "output")
    tmp <- rejection_rates(
             output_lower,
             all_columns = TRUE
           )
    reject_lower <- tmp$reject
    nrep_lower <- tmp$nrep
    ok_lower <- check_solution(
                  f_i = reject_lower,
                  target_power = target_power,
                  nrep = final_nrep,
                  final_nrep = final_nrep,
                  ci_level = ci_level,
                  what = what,
                  goal = goal,
                  tol = tol
                )
    output_upper <- attr(f.upper, "output")
    tmp <- rejection_rates(
             output_upper,
             all_columns = TRUE
           )
    reject_upper <- tmp$reject
    nrep_upper <- tmp$nrep
    ok_upper <- check_solution(
                  f_i = reject_upper,
                  target_power = target_power,
                  nrep = nrep_upper,
                  final_nrep = final_nrep,
                  ci_level = ci_level,
                  what = what,
                  goal = goal,
                  tol = tol
                )

    if ((interval_updated$extend_status != 0) &&
        (!ok_lower && !ok_upper)) {

      # ==== No solution and interval invalid. Skip the search ====

      # Interval not OK and no bounds are the solution

      if (progress) {
        cat("\n")
        cat(names(interval_updated$extend_status), ".\n")
        cat("... and none of the bounds are solution.\n")
        cat("Try another interval.\n\n")

        # Should quit

        do_search <- FALSE
      }

    }

  }

  # ==== Is one of the bounds a solution? ====

  if (ok_lower || ok_upper) {

    # ==== Yes. Skip the search ====

    if (progress) {
      cat("One of the bounds in the interval is already a solution.\n\n")
    }

    do_search <- FALSE
    ci_hit <- switch(goal,
                     ci_hit = TRUE,
                     close_enough = NA)
    solution_found <- TRUE

    status <- bisection_status_message(0, status)

    # The lower bound take precedence

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

  # ==== Do the search ====

  if (do_search) {

    # Do the bisection search

    x_i <- start
    if (x_type == "n") {
      x_i <- ceiling(x_i)
    }
    f.lower_i <- f.lower
    f.upper_i <- f.upper
    lower_i <- lower
    upper_i <- upper
    status <- 0

    # ==== Start the loop ====

    i <- 1
    while (i <= max_trials) {

      if (progress) {
        cat("\nIteration #", i, "\n")
      }

      # ==== Compute f(x) ====

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
      # final_nrep and nrep are always the same
      # for now for bisection

      # ==== Is x a solution? ====

      ok <- check_solution(
              f_i = reject_i,
              target_power = target_power,
              nrep = final_nrep,
              final_nrep = final_nrep,
              ci_level = ci_level,
              what = what,
              goal = goal,
              tol = tol
            )

      if (ok) {

        # ==== Yes. Solution found ====

        # Solution found

        ci_hit <- switch(goal,
                         ci_hit = TRUE,
                         close_enough = NA)
        solution_found <- TRUE

        status <- bisection_status_message(0, status)

        break

      }

      f_history[i] <- as.numeric(out_i)
      x_history[i] <- x_i
      x_interval_history[i, ] <- c(lower_i, upper_i)
      f_interval_history[i, ] <- c(f.lower_i, f.upper_i)
      reject_history[i] <- reject_i

      # ==== Check changes ====

      changes_ok <- check_changes(
              x_history = x_history,
              delta_tol = delta_tol,
              last_k = last_k
            )

      if (!changes_ok) {

        status <- bisection_status_message(2, status)
        break

      }

      # ==== No solution. Update the interval ====

      # TODO:
      # - Handle nonnegative rate

      # == Check rate ==

      # rate_ok <- check_rate(f_history,
      #                       delta_slope_tol = -.1,
      #                       last_k = last_k)

      # == Rate OK. Update as usual ==

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

      if (isTRUE(variants$muller) &&
          (i >= 3)) {

        # ==== Use Muller's Method ====

        # NOTE: Usable but do not use.
        # This method was found to fail to converge
        # even in some simple cases.

        tmp1 <- x_history[(i - 3 + 1):i]
        tmp2 <- f_history[(i - 3 + 1):i]
        x_muller <- tryCatch(
            root_muller_i(f = f,
                          xm2 = tmp1[1],
                          xm1 = tmp1[2],
                          x0i = tmp1[3],
                          ym2 = tmp2[1],
                          ym1 = tmp2[2],
                          y0i = tmp2[3]
                        ),
                      error = function(e) e)
        if (!inherits(x_muller, "error")) {
          x_i <- x_muller
        } else {
          x_i <- mean(c(lower_i, upper_i))
        }
      } else {

        # ==== Mean as the next start ====

        x_i <- mean(c(lower_i, upper_i))
      }

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

    # ==== End the loop ====

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

  # ==== Prepare the output ====

  if (!do_search &&
      (!ok_lower && !ok_upper)) {

    # ==== No solution. Set _out to NA ====

    status <- bisection_status_message(1, status)

    # No solution for whatever reason

    x_out <- NA
    power_out <- NA
    nrep_out <- NA
    ci_out <- NA
    by_x_out <- NA
    i2 <- NA

  } else {

    # ==== Solution found. Store it ====

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

  # ==== Update the power curve ====

  fit_1 <- power_curve(by_x_1,
                      formula = power_model,
                      start = power_curve_start,
                      lower_bound = lower_bound,
                      upper_bound = upper_bound,
                      nls_control = nls_control,
                      nls_args = nls_args,
                      verbose = progress)
  x_tried <- get_x_tried(by_x_1,
                         x = x)

  if (progress) {
    if (solution_found) {
      cat("Solution found.\n")
    } else {
      cat("Solution not found.\n")
    }
  }

  # ==== Return the output ====

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
              status = status,
              iteration = i,
              x_history = x_history[!is.na(x_history)],
              x_interval_history = x_interval_history[stats::complete.cases(x_interval_history), ],
              f_interval_history = f_interval_history[stats::complete.cases(f_interval_history), ],
              reject_history = reject_history[!is.na(reject_history)],
              f_history = f_history[!is.na(f_history)],
              tol = tol,
              delta_tol = delta_tol,
              last_k = last_k,
              what = what,
              goal = goal,
              ci_level = ci_level)
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
  # by_x_1 is always supplied

  # ==== Interval already valid? ====

  if (sign(f.lower) != sign(f.upper)) {

    # ==== Yes. Exit ====

    # No need to extend
    if (trace) {
      cat("\n\n== Exit extending interval ...\n\n")
    }
    return(list(lower = lower,
                upper = upper,
                f.lower = f.lower,
                f.upper = f.upper,
                interval_ok = TRUE,
                extend_status = status_msg[status_msg == 0],
                extendInt = extendInt))
  }

  # ==== Interval invalid ====

  if (extendInt == "no") {

    # ==== "No". Exit ====

    if (trace) {
      cat("\n\n== Exit extending interval ...\n\n")
    }
    return(list(lower = lower,
                upper = upper,
                f.lower = f.lower,
                f.upper = f.upper,
                interval_ok = FALSE,
                extend_status = status_msg[status_msg == 1],
                extendInt = extendInt))
  }

  # ==== Handle bounds with equal function values ====

  rate_i <- .9
  rate_tmp <- 1
  max_tmp <- 5
  while (isTRUE(all.equal(
                  as.numeric(f.lower),
                  as.numeric(f.upper)
                )
              ) ||
          (max_tmp == 0)
          ) {
    max_tmp <- max_tmp - 1
    # To handle bounds with equal f values.
    # If f.upper == f.lower,
    if ((upper > 0) && (lower > 0)) {
      # Only extend lower
      tmp <- (args$target_power - as.numeric(f.lower)) /
             (args$target_power + as.numeric(f.lower))
      tmp <- tmp * rate_tmp
      lower <- max(lower_hard,
                   ifelse(x_type == "n",
                          floor(lower * tmp),
                          lower * tmp))
      lower <- force_new_x(
                    lower,
                    x_tried = get_x_tried(object = by_x_1,
                                          x = x_type),
                    x_interval = range(lower_hard, upper_hard),
                    x_type = x_type
                  )
      f.lower <- f(lower, ...)
      by_x_1 <- c(by_x_1, attr(f.lower, "output"),
                  skip_checking_models = TRUE)
    } else if (((upper < 0 && (lower < 0)))) {
      # Only extend upper
      tmp <- (args$target_power - as.numeric(f.upper)) /
             (args$target_power + as.numeric(f.upper))
      tmp <- tmp * 1 / rate_tmp
      upper <- min(upper_hard,
                   ifelse(x_type == "n",
                          floor(upper * tmp),
                          upper * tmp))
      upper <- force_new_x(
                    upper,
                    x_tried = get_x_tried(object = by_x_1,
                                          x = x_type),
                    x_interval = range(lower_hard, upper_hard),
                    x_type = x_type
                  )
      f.upper <- f(upper, ...)
      by_x_1 <- c(by_x_1, attr(f.upper, "output"),
                  skip_checking_models = TRUE)
    } else {
      # This possibility should not happen. Just in case.
      # widen the interval slightly
      tmp_upper <- tmp * 1 / rate_tmp
      tmp_lower <- tmp * rate_tmp

      upper <- min(upper_hard,
                   ifelse(x_type == "n",
                          floor(upper * tmp_upper),
                          upper * tmp_upper))
      lower <- max(lower_hard,
                   ifelse(x_type == "n",
                          floor(lower * tmp_lower),
                          lower * tmp_lower))
      upper <- force_new_x(
                    upper,
                    x_tried = get_x_tried(object = by_x_1,
                                          x = x_type),
                    x_interval = range(lower_hard, upper_hard),
                    x_type = x_type
                  )
      lower <- force_new_x(
                    lower,
                    x_tried = get_x_tried(object = by_x_1,
                                          x = x_type),
                    x_interval = range(lower_hard, upper_hard),
                    x_type = x_type
                  )
      f.upper <- f(upper, ...)
      f.lower <- f(lower, ...)
      by_x_1 <- c(by_x_1, attr(f.upper, "output"),
                  skip_checking_models = TRUE)
      by_x_1 <- c(by_x_1, attr(f.lower, "output"),
                  skip_checking_models = TRUE)
    }
    rate_tmp <- rate_tmp * rate_i
  }

  # ==== How should the interval be extended? ====

  extend_which <- check_extend_x(
                    upper = upper,
                    lower = lower,
                    f.upper = f.upper,
                    f.lower = f.lower
                  )
  extend_up <- (extend_which == "extend_up")
  extend_down <- (extend_which == "extend_down")

  if (extend_down && (!(extendInt %in% c("yes", "downX")))) {

    # ==== Should extend down but not requested. Exist ====

    if (trace) {
      cat("\n\n== Exit extending interval ...\n\n")
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

    # ==== Should extend up but not requested. Exist ====

    if (trace) {
      cat("\n\n== Exit extending interval ...\n\n")
    }
    return(list(lower = lower,
                upper = upper,
                f.lower = f.lower,
                f.upper = f.upper,
                interval_ok = FALSE,
                extend_status = status_msg[status_msg == 3],
                extendInt = extendInt))
  }
  interval_ok <- FALSE

  # ==== Extend the interval ====

  if (((extendInt %in% c("yes", "downX")) && extend_down) ||
      ((extendInt %in% c("yes", "upX")) && extend_up)) {
    # Extend simple linear extrapolation
    if (trace) {
      cat(
        switch(extend_which,
               extend_down =
                "Interval above the solution. Extend the lower bound ...\n",
               extend_up =
                "Interval below the solution. Extend the upper bound ...\n")
      )
    }

    # ==== Loop for extension ====
    i <- 1
    while ((i <= extend_maxiter) &&
            (sign(f.lower) == sign(f.upper))) {
        out_i <- extend_i(
            f = f,
            args = args,
            i = i,
            lower = lower,
            upper = upper,
            f.lower = f.lower,
            f.upper = f.upper,
            interval_ok = interval_ok,
            extend_status = extend_status,
            trace = trace,
            status_msg = status_msg,
            lower_hard = lower_hard,
            upper_hard = upper_hard,
            x_type = x_type,
            digits = digits,
            overshoot = overshoot,
            which = switch(extend_which,
                           extend_down = "lower",
                           extend_up = "upper"),
            by_x_1 = by_x_1
          )
        lower <- out_i$lower
        upper <- out_i$upper
        f.lower <- out_i$f.lower
        f.upper <- out_i$f.upper
        by_x_1 <- c(by_x_1, attr(f.lower, "output"),
                    skip_checking_models = TRUE)
        by_x_1 <- c(by_x_1, attr(f.upper, "output"),
                    skip_checking_models = TRUE)
        interval_ok <- out_i$interval_ok
        extend_status <- out_i$extend_status
        extend_which <- check_extend_x(
                          upper = upper,
                          lower = lower,
                          f.upper = f.upper,
                          f.lower = f.lower
                        )
        extend_up <- (extend_which == "extend_up")
        extend_down <- (extend_which == "extend_down")
        i <- i + 1
      }
    }

  # ==== Check the extended interval ====

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
    cat("\n\n== Exit extending interval ...\n\n")
  }

  # ==== Return the extended interval ====

  return(list(lower = lower,
              upper = upper,
              f.lower = f.lower,
              f.upper = f.upper,
              interval_ok = interval_ok,
              extend_status = extend_status,
              extendInt = extendInt,
              by_x_1 = by_x_1))
}

#' @noRd
extend_i <- function(
                  f,
                  args,
                  i,
                  lower,
                  upper,
                  f.lower,
                  f.upper,
                  interval_ok,
                  extend_status,
                  trace,
                  status_msg,
                  lower_hard,
                  upper_hard,
                  x_type,
                  digits,
                  overshoot,
                  which = c("lower", "upper"),
                  by_x_1 = NULL) {
  which <- match.arg(which)
  if (((which == "lower") && (lower == lower_hard)) ||
      ((which == "upper") && (upper == upper_hard))) {
    # Hit the hard limit
    interval_ok <- FALSE
    tmp <- switch(which,
                  lower = 4,
                  upper = 5)
    extend_status <- status_msg[status_msg == tmp]
    if (trace) {
      cat(names(extend_status), ".\n", sep = "")
    }
  } else {
    slope <- (f.upper - f.lower) / (upper - lower)
    intercept <- -slope * upper +  f.upper
    if (which == "lower") {
      upper <- lower
      f.upper <- f.lower
      lower <- overshoot * -intercept / slope
      if (lower > upper) {
        lower <- mean(c(lower_hard, upper))
      }
      if (x_type == "n") {
        lower <- ceiling(lower)
      }
      lower <- max(lower, lower_hard)
      if (!is.null(by_x_1)) {
        lower <- force_new_x(
                      lower,
                      x_tried = get_x_tried(object = by_x_1,
                                            x = x_type),
                      x_interval = range(lower_hard, upper_hard),
                      x_type = x_type
                    )
      }
      f.lower <- do.call(f,
                          c(list(x_i = lower),
                           args))
    } else {
      lower <- upper
      f.lower <- f.upper
      upper <- (1 + overshoot) * -intercept / slope
      if (upper < lower) {
        upper <- mean(c(lower, upper_hard))
      }
      if (x_type == "n") {
        upper <- ceiling(upper)
      }
      upper <- min(upper, upper_hard)
      if (!is.null(by_x_1)) {
        upper <- force_new_x(
                      upper,
                      x_tried = get_x_tried(object = by_x_1,
                                            x = x_type),
                      x_interval = range(lower_hard, upper_hard),
                      x_type = x_type
                    )
      }
      f.upper <- do.call(f,
                         c(list(x_i = upper),
                           args))
    }
    # Fix the interval
    if (upper < lower) {
      tmp <- lower
      lower <- upper
      upper <- tmp
      tmp <- f.lower
      f.lower <- f.upper
      f.upper <- tmp
      rm(tmp)
    }
    if (trace) {
      cat("\n\n(Extending the interval) Iteration:", i, "\n\n")
      print_interval(lower = lower,
                      upper = upper,
                      digits = digits,
                      x_type = x_type)
    }
  }
  list(
    lower = lower,
    upper = upper,
    f.lower = f.lower,
    f.upper = f.upper,
    interval_ok = interval_ok,
    extend_status = extend_status
  )
}

#' @noRd

check_extend_x <- function(
                    upper,
                    lower,
                    f.upper,
                    f.lower) {
  slope <- (f.upper - f.lower) / (upper - lower)
  extend_up <- ((slope > 0) && (f.upper < 0)) ||
                ((slope < 0) && (f.upper > 0))
  extend_down <- ((slope > 0) && (f.upper > 0)) ||
                  ((slope < 0) && (f.upper < 0))
  if (extend_up) {
    out <- "extend_up"
  } else if (extend_down) {
    out <- "extend_down"
  } else {
    # If f.upper and f.lower are equal,
    # err on larger value
    out <- "extend_up"
  }
  out
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

    # ==== How to get power ====

    if (is.null(out_i) && is.null(power_i)) {

      # ==== Estimate Power (No out_i and no powre_i) ====

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

        # ==== Get Power (out_i supplied) ====

        # power_i is supplied. Ignore out_i
        power_i <- rejection_rates(out_i)$reject
        out_i <- NULL
      } else {

        # ==== power_i supplied ====

        # out_i is supplied
        # power_i will be used
      }
    }

    # ==== Compute CI ====

    # a <- abs(stats::qnorm((1 - ci_level) / 2))
    se_i <- sqrt(power_i * (1 - power_i) / nrep)
    # ci_i <- power_i + c(-a, a) * se_i
    ci_i <- reject_ci(
              nreject = round(power_i * nrep),
              nvalid = nrep,
              level = ci_level,
              method = getOption("power4mome.ci_method", default = "wilson"))
    ci_i <- as.vector(ci_i)

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

    # ==== Return function value ====

    out2_i <- switch(what,
                     point = power_i - target_power,
                     ub = ci_i[2] - target_power,
                     lb = ci_i[1] - target_power)
    if (store_output) {
      attr(out2_i, "output") <- out_i
    }
    out2_i
  }

  # ==== Set default values for the objective function ====

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
power_algorithm_bisection_pre_i <- function(object,
                                            x,
                                            pop_es_name,
                                            target_power,
                                            x_max,
                                            x_min,
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
                                            final_R,
                                            what,
                                            goal,
                                            tol,
                                            ci_level
                                            ) {

  # ==== Initial values ====

  # This method only needs an initial interval
  if (inherits(object_by_org, "power4test_by_n") ||
      inherits(object_by_org, "power4test_by_es")) {
    x_i <- set_x_range_by_x(
                      object,
                      x = x,
                      pop_es_name = pop_es_name,
                      target_power = target_power,
                      k = 2,
                      x_max = x_max,
                      x_min = x_min,
                      object_by_org = object_by_org,
                      what = what,
                      goal = goal,
                      tol = tol,
                      ci_level = ci_level)
  } else {
    x_i <- set_x_range(object,
                      x = x,
                      pop_es_name = pop_es_name,
                      target_power = target_power,
                      k = 2,
                      x_max = x_max,
                      x_min = x_min)
  }

  # For bisection, no need to exclude the value in the input objects
  # Exclude the value in the input object

  # For bisection, only two initial values are needed

  # ==== Set x_interval ====

  if (x_include_interval) {
    # For bisection, should exclude them initially,
    # and let extend_interval() to do the job
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

  # ==== Update by_x (by_x_i) ====

  by_x_i <- switch(x,
          n = as.power4test_by_n(object),
          es = as.power4test_by_es(object,
                                    pop_es_name = pop_es_name)
        )

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
  # - How about adaptive nrep for bisection?

  # ==== Return the output ====

  out <- list(x_i = NULL,
              by_x_i = NULL,
              fit_i = NULL,
              by_x_1 = by_x_1,
              fit_1 = fit_1,
              nrep_seq = NULL,
              final_nrep_seq = NULL,
              R_seq = NULL,
              x_interval_updated = x_i)

  return(out)
}

#' @noRd

bisection_status_message <- function(x,
                                       status_old) {
  # Do not override existing status
  if (!is.null(status_old)) {
    return(status_old)
  }
  status_msgs <- c(
        "Solution found." = 0,
        "Maximum iteration (max_trials) reached." = 1,
        "Changes in the two iterations less than 'delta_tol'." = 2
      )
  status_msgs[status_msgs == x]
}

#' @noRd

random_interval <- function(interval_i) {
  interval_i <- stats::na.omit(interval_i)
  interval_i0 <- interval_i[nrow(interval_i), ]
  tmp <- expand.grid(lower = interval_i[, "lower"],
                     upper = interval_i[, "upper"])
  tmp <- split(tmp,
               seq_len(nrow(tmp)))
  tmp2 <- sapply(tmp,
            function(xx) {
              all(xx == interval_i0)
            })
  if (all(tmp2)) return(NULL)
  tmp <- tmp[!tmp2]
  tmp_i <- sample.int(length(tmp),
                      size = 1)
  return(tmp[[tmp_i]])
}
