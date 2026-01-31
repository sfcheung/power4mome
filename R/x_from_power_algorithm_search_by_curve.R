# #' @param xs_per_trial The initial number
# #' of values (sample sizes or population
# #' values) to consider in each
# #' trial. Should be an integer at least
# #' 1. Rounded
# #' up if not an integer.

# #' @param power_min,power_max The minimum
# #' and maximum values, respectively,
# #' of power
# #' when determining the values to
# #' try in each trail. Default is .01.

# #' @param initial_nrep The initial
# #' number of replications. If set
# #' to `NULL`, the `nrep` used in
# #' `object` will be used. If higher
# #' than `final_nrep`, it will be
# #' converted to one-fourth of `final_nrep`.
# #' If lower than the `nrep` in `object`
# #' after the conversion,
# #' then set to `nrep` in the `object`.

# #' @param power_model The nonlinear
# #' model to be used when estimating
# #' the relation between power and
# #' `x`. Should be a formula
# #' acceptable by [stats::nls()],
# #' with `reject` on the left-hand side,
# #' and `x`
# #' on the right-hand
# #' side, with one or more parameters.
# #' Can also be set to a list of
# #' models.
# #' Users rarely need to change the
# #' default value. If `NULL`, the default,
# #' then the default model(s) will be
# #' determined by [power_curve()].
# #'
# #' @param start A named numeric vector
# #' of the starting values for `power_model`
# #' when fitted by [stats::nls()]. If
# #' `power_model` is a list, this should
# #' be a list of the same length.
# #' Users rarely need to change the
# #' default values.
# #'
# #' @param lower_bound A named numeric vector
# #' of the lower bounds for parameters
# #' in `power_model`
# #' when fitted by [stats::nls()]. If
# #' `power_model` is a list, this should
# #' be a list of the same length.
# #' Users rarely need to change the
# #' default values.
# #'
# #' @param upper_bound A named numeric vector
# #' of the upper bounds for parameters
# #' in `power_model`
# #' when fitted by [stats::nls()]. If
# #' `power_model` is a list, this should
# #' be a list of the same length.
# #' Users rarely need to change the
# #' default values.
# #'
# #' @param nls_args A named list of
# #' arguments to be used when calling
# #' [stats::nls()]. Used to override
# #' internal default, such as the
# #' algorithm (default is `"port"`).
# #' Use this argument with cautions.
# #'
# #' @param nls_control A named list of
# #' arguments to be passed the `control`
# #' argument of [stats::nls()] when
# #' estimating the relation between
# #' power and `x`. The values will
# #' override internal default values,
# #' and also override `nls_args`.
# #' Use this argument with cautions.

# #' @param initial_R The initial number of
# #' Monte Carlo simulation or
# #' bootstrapping samples. The `R` in calling
# #' [power4test()], [power4test_by_n()],
# #' or [power4test_by_es()]. If set to `NULL`,
# #' the `R` used in
# #' `object` will be used.
# #' If higher
# #' than `final_R`, it will be
# #' converted to one-fourth of `final_R`.
# #' If lower than the `R` in `object`
# #' after the conversion,
# #' then set to `R` in `object``.

# #' @param nrep_steps How many steps
# #' the number of replications will be
# #' increased to `final_nrep`, if the
# #' initial number of replications
# #' (`nrep` in [power4test()]) is
# #' less than `final_nrep`. The number
# #' of replications will be successively
# #' increased by this number of steps
# #' to increase the precision in estimating
# #' the power. Should be at least 1.
# #' Increasing this number will result
# #' in more trials and take longer to
# #' run, but will try more values.
# #' Rounded up if not an integer.

# #' @param final_xs_per_trial The final number
# #' of values (sample sizes or population
# #' values) to consider in the last
# #' trial or last few trials. Should be an integer at least
# #' 1. Rounded
# #' up if not an integer.

#' @noRd

alg_power_curve <- function(
  object,
  x,
  pop_es_name,
  ...,
  target_power,
  xs_per_trial = 3,
  x_max,
  x_min,
  nrep0 = 100,
  R0 = 250,
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
  nrep_steps = 1,
  final_R,
  final_xs_per_trial = 1,
  pre_i_xs = 5,
  pre_i_nrep = 50,
  pre_i_R = ifelse(is.null(R0),
                   NULL,
                   min(200, R0)),
  max_trials,
  ci_level,
  power_min = .01,
  power_max = .90,
  extendInt,
  power_tolerance_in_interval,
  power_tolerance_in_final,
  what = c("point", "ub", "lb"),
  goal = c("ci_hit", "close_enough"),
  tol = .02,
  delta_tol = switch(x,
                   n = 1,
                   es = .001),
  last_k = 3
) {

  what <- match.arg(what)
  goal <- match.arg(goal)

  # ==== Sanity check ====

  if (final_xs_per_trial < 1) {
    stop("'final_xs_per_trial' (",
         final_xs_per_trial,
         ") is less than 1.")
  }
  final_xs_per_trial <- ceiling(final_xs_per_trial)

  nrep_steps <- ceiling(nrep_steps)
  if (nrep_steps < 0) {
    stop("'nrep_steps' must be at least 1 (after rounding, if necessary).")
  }

  if (xs_per_trial < 1) {
    stop("'xs_per_trial' (",
         xs_per_trial,
         ") is less than 1.")
  }
  xs_per_trial <- ceiling(xs_per_trial)

  if (power_min <= 0 || power_max >= 1) {
    stop("'power_min' and 'power_max' must be between 0 and 1.")
  }

  if (power_max < target_power || power_min > target_power) {
    stop("'target_power' must be between 'power_min' and 'power_max'.")
  }

  # ==== Get nrep and R ====

  nrep_org <- attr(object, "args")$nrep
  if (nrep0 > final_nrep) {
    nrep0 <- ceiling(final_nrep / 4)
    if ((nrep0 < 100) && (nrep_org <= final_nrep)) {
      nrep0 <- nrep_org
    }
  }

  R_org <- attr(object, "args")$R

  if (is.null(R0)) {
    R0 <- R_org
  } else {
    if (R0 > final_R) {
      R0 <- ceiling(final_R / 4)
      if ((R0 < 100) && (R_org <= final_R)) {
        R0 <- R_org
      }
    }
    R0 <- ceiling(R0)
  }

  # ==== Pre-search setup ====

  a_out <- power_algorithm_search_by_curve_pre_i(
    object = object,
    x = x,
    pop_es_name = pop_es_name,
    target_power = target_power,
    xs_per_trial = xs_per_trial,
    x_max = x_max,
    x_min = x_min,
    nrep0 = nrep0,
    R0 = R0,
    progress = progress,
    x_include_interval = x_include_interval,
    x_interval = x_interval,
    simulation_progress = simulation_progress,
    save_sim_all = save_sim_all,
    is_by_x = is_by_x,
    object_by_org = object_by_org,
    power_model = power_model,
    start = start,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    nls_control = nls_control,
    nls_args = nls_args,
    final_nrep = final_nrep,
    nrep_steps = nrep_steps,
    final_R = final_R,
    final_xs_per_trial = final_xs_per_trial,
    pre_i_xs = pre_i_xs,
    pre_i_nrep = pre_i_nrep,
    pre_i_R = pre_i_R,
    what = what,
    goal = goal,
    ci_level = ci_level,
    tol = tol
  )

  # ==== Process output ====

  by_x_1 <- a_out$by_x_1
  fit_1 <- a_out$fit_1
  nrep_seq <- a_out$nrep_seq
  final_nrep_seq <- a_out$final_nrep_seq
  R_seq <- a_out$R_seq
  xs_per_trial_seq <- a_out$xs_per_trial_seq

  # TODO:
  # - Check whether it works
  if (is_by_x) {
    tmp <- setdiff(names(object_by_org), names(by_x_1))
    by_x_1 <- c(by_x_1,
                object_by_org[tmp],
                skip_checking_models = TRUE)
  }

  rm(a_out)

  # ==== Start the search ====

  a_out <- power_algorithm_search_by_curve(
    object = object,
    x = x,
    pop_es_name = pop_es_name,
    target_power = target_power,
    xs_per_trial_seq = xs_per_trial_seq,
    ci_level = ci_level,
    power_min = power_min,
    power_max = power_max,
    x_interval = x_interval,
    extendInt = extendInt,
    progress = progress,
    simulation_progress = simulation_progress,
    max_trials = max_trials,
    final_nrep = final_nrep,
    power_model = power_model,
    start = start,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    nls_control = nls_control,
    nls_args = nls_args,
    save_sim_all = save_sim_all,
    power_tolerance_in_interval = power_tolerance_in_interval,
    power_tolerance_in_final = power_tolerance_in_final,
    by_x_1 = by_x_1,
    fit_1 = fit_1,
    nrep_seq = nrep_seq,
    final_nrep_seq = final_nrep_seq,
    R_seq = R_seq,
    final_xs_per_trial = final_xs_per_trial,
    delta_tol = delta_tol,
    last_k = last_k,
    what = what,
    goal = goal,
    tol = tol)

  # ==== Return the output ====

  a_out
}

#' @noRd

power_algorithm_search_by_curve <- function(object,
                                            x,
                                            pop_es_name,
                                            target_power,
                                            xs_per_trial_seq,
                                            ci_level,
                                            power_min,
                                            power_max,
                                            x_interval,
                                            extendInt,
                                            progress = TRUE,
                                            simulation_progress,
                                            max_trials = 10,
                                            final_nrep,
                                            power_model,
                                            start,
                                            lower_bound,
                                            upper_bound,
                                            nls_control,
                                            nls_args,
                                            save_sim_all,
                                            power_tolerance_in_interval,
                                            power_tolerance_in_final,
                                            by_x_1,
                                            fit_1,
                                            nrep_seq,
                                            final_nrep_seq,
                                            R_seq,
                                            final_xs_per_trial,
                                            delta_tol = switch(x,
                                                               n = 1,
                                                               es = .001),
                                            last_k = 3,
                                            what = "point",
                                            goal = "ci_hit",
                                            tol = .02) {

  # goal: ci_hit
  #   what: point
  #     - No need to change
  # goal: close_enough
  #   what: point
  #     - TODO: Use distance as the goal
  #   what: ub, lb
  #     - TODO: Use adjusted_power from target_power_adjusted()

  # ci_hit does NOT denote solution found
  # TODO:
  # - solution_found refers to goal met
  ci_hit <- FALSE
  solution_found <- FALSE

  i2 <- NULL
  target_in_range <- FALSE
  status <- NULL
  changes_ok <- TRUE

  x_history <- vector("numeric", max_trials)
  x_history[] <- NA
  reject_history <- vector("numeric", max_trials)
  reject_history[] <- NA

  # ==== Start the Loop ====

  for (j in seq_len(max_trials)) {

    if (progress) {
      cat("--- Trial", j, "---\n\n")
      tmp <- format(Sys.time(), "%Y-%m-%d %X")
      cat("- Start at", tmp, "\n")
    }

    # ==== Determine values to try ====

    # ci_hit-point: OK
    # close_enough-point: WIP
    # close_enough-lb/ub: WIP

    # TODO:
    # - Update this part. Not necessary if goal is close_enough

    if (ci_hit) {
      # After the first trial,
      # Check whether at least one CI hit the target power.
      # If yes, reduce the range of power levels when
      # selecting the values to try.
      # Necessary because the number of replication may have
      # increased, requiring narrower CI.
      power_tolerance_in_interval <- max(abs(ci_out - power_out))
    }

    # ** x_j **
    # The vector of values to be tried in this trial
    # Determined using by latest power curve (fit_1)
    x_tried <- switch(x,
                      n = as.numeric(names(by_x_1)),
                      es = sapply(by_x_1,
                                  \(x) {attr(x, "pop_es_value")},
                                  USE.NAMES = FALSE))

    # ==== Update x-interval to try ====

    # ci_hit-point: OK
    # close_enough-point: WIP
    # close_enough-lb/ub: WIP

    # TODO:
    # - Make sure that adjusted power is used with close_enough-lb/ub
    # - Check tolerance: May depend on goal

    if (target_in_range) {
      # ==== Target power in range ====

      # ci_hit-point: OK
      # close_enough-point: WIP
      # close_enough-lb/ub: WIP

      # TODO:
      # - Check tolerance: May depend on goal

      # Always include the intersection, if target_in_range

      x_j <- estimate_x_range(power_x_fit = fit_1,
                              x = x,
                              target_power = target_power,
                              k = max(xs_per_trial_seq[1],
                                      1),
                              tolerance = power_tolerance_in_interval,
                              power_min = power_min,
                              power_max = power_max,
                              interval = x_interval,
                              extendInt = extendInt,
                              x_to_exclude = x_tried)
      tmp <- switch(x,
                    n = ceiling(x_between_i),
                    es = x_between_i)
      x_j <- c(x_j, tmp)
      x_j <- unique(x_j)
      if (length(x_j) < xs_per_trial_seq[1]) {
        # estimate_x_range generated x_between_i
        # Call it again to get all k values

        # ci_hit-point: OK
        # close_enough-point: WIP
        # close_enough-lb/ub: WIP

        # TODO:
        # - Check tolerance: May depend on goal

        x_j <- estimate_x_range(power_x_fit = fit_1,
                                x = x,
                                target_power = target_power,
                                k = xs_per_trial_seq[1],
                                tolerance = power_tolerance_in_interval,
                                power_min = power_min,
                                power_max = power_max,
                                interval = x_interval,
                                extendInt = extendInt,
                                x_to_exclude = x_tried)
      }
    } else {
      # ==== Target power not in range ====

      # ci_hit-point: OK
      # close_enough-point: WIP
      # close_enough-lb/ub: WIP

      # TODO:
      # - Check tolerance: May depend on goal

      x_j <- estimate_x_range(power_x_fit = fit_1,
                              x = x,
                              target_power = target_power,
                              k = xs_per_trial_seq[1],
                              tolerance = power_tolerance_in_interval,
                              power_min = power_min,
                              power_max = power_max,
                              interval = x_interval,
                              extendInt = extendInt,
                              x_to_exclude = x_tried)
    }

    # ==== Adjust nrep based on extrapolated power ====

    # ci_hit-point: OK
    # close_enough-point: WIP
    # close_enough-lb/ub: WIP

    # TODO:
    # - Check tolerance: May depend on goal
    # - Make sure that adjusted power is used with close_enough-lb/ub

    # Adjust the numbers of replication for each value.
    # A value with estimated power closer to the
    # target power will have a higher number of replication
    # power_j <- predict_fit(fit_1,
    #                        newdata = list(n = n_j))
    power_j <- stats::predict(fit_1,
                              newdata = list(x = x_j))
    nrep_j <- nrep_from_power(power_j = power_j,
                              target_power = target_power,
                              tolerance = power_tolerance_in_final,
                              nrep_min = nrep_seq[1],
                              nrep_max = final_nrep_seq[1])

    if (progress) {
      x_j_str <- formatC(x_j,
                         digits = switch(x, n = 0, es = 3),
                         format = "f")
      cat("- Value(s) to try:",
          paste0(x_j_str, collapse = ", "),
          "\n")
      cat("- Numbers of replications:",
          paste0(nrep_j, collapse = ", "),
          "\n")
    }

    # ==== Do the simulation for each value  ====

    # ci_hit-point: OK
    # close_enough-point: OK
    # close_enough-lb/ub: OK

    # ** by_x_j **
    # The results for this trial (based on n_j)
    by_x_j <- switch(x,
                     n = power4test_by_n(object,
                                         n = x_j,
                                         R = R_seq[1],
                                         progress = simulation_progress,
                                         by_nrep = nrep_j,
                                         save_sim_all = save_sim_all),
                     es = power4test_by_es(object,
                                           pop_es_name = pop_es_name,
                                           pop_es_values = x_j,
                                           R = R_seq[1],
                                           progress = simulation_progress,
                                           by_nrep = nrep_j,
                                           save_sim_all = save_sim_all))

    # Add the results to by_x_1
    by_x_1 <- c(by_x_1, by_x_j,
                skip_checking_models = TRUE)

    if (progress) {
      cat("\n- Rejection Rates:\n\n")
      tmp <- rejection_rates(by_x_1)
      print(tmp,
            annotation = FALSE)
      cat("\n")
    }

    # ==== Update the power curve ====

    # ci_hit-point: OK
    # close_enough-point: WIP
    # close_enough-lb/ub: WIP

    # TODO:
    # - Make sure that adjusted power is used with close_enough-lb/ub

    fit_i <- power_curve(by_x_1,
                         formula = power_model,
                         start = start,
                         lower_bound = lower_bound,
                         upper_bound = upper_bound,
                         nls_control = nls_control,
                         nls_args = nls_args,
                         verbose = progress,
                         models = c("glm", "lm"))

    # ==== Is target power in the range of current power levels? ====

    # ci_hit-point: OK
    # close_enough-point: WIP
    # close_enough-lb/ub: WIP

    # TODO:
    # - Make sure that adjusted power is used with close_enough-lb/ub

    # Get the rejection rates of all values tried.
    tmp1 <- rejection_rates_add_ci(by_x_1,
                                   level = ci_level)
    # tmp1$reject <- tmp1$sig
    tmp2 <- range(tmp1$reject)

    # Is the desired value likely already in the range
    # of values examined, based on the estimated power?
    target_in_range <- (target_power > tmp2[1]) &&
                       (target_power < tmp2[2])

    if (target_in_range) {

      # ==== Current solution: x with closest power ====

      # ci_hit-point: OK
      # close_enough-point: OK
      # close_enough-lb/ub: WIP

      # TODO:
      # - Make sure that adjusted power is used with close_enough-lb/ub

      # The desired value probably within the range examined

      tmp4 <- tmp1$reject - target_power
      tmp3 <- abs(tmp1$reject - target_power)

      # Closest and above
      tmp4a <- which(tmp4 > 0)
      x_above_i <- tmp4a[which.min(tmp3[tmp4a] * tmp1$reject_se[tmp4a]^2)]
      # Closest and below
      tmp4b <- which(tmp4 < 0)
      x_below_i <- tmp4b[which.min(tmp3[tmp4b] * tmp1$reject_se[tmp4b]^2)]

      x_out_above_i <- switch(x,
                              n = tmp1$n[x_above_i],
                              es = tmp1$es[x_above_i])
      x_out_below_i <- switch(x,
                              n = tmp1$n[x_below_i],
                              es = tmp1$es[x_below_i])
      x_reject_above_i <- tmp1$reject[x_above_i]
      x_reject_below_i <- tmp1$reject[x_below_i]

      x_between_i <- x_from_y(x1 = x_out_below_i,
                              y1 = x_reject_below_i,
                              x2 = x_out_above_i,
                              y2 = x_reject_above_i,
                              target = target_power)

      x_out_i <- which.min(tmp3)

      # If ties, the smallest value will be used

      # ** x_out, power_out, nrep_out, ci_out, by_x_out **
      # The results of the candidate solution,
      # - The value with power closest to the target power
      x_out <- switch(x,
                      n = tmp1$n[x_out_i],
                      es = tmp1$es[x_out_i])
      power_out <- tmp1$reject[x_out_i]
      nrep_out <- tmp1$nrep[x_out_i]
      by_x_ci <- rejection_rates_add_ci(by_x_1,
                                        level = ci_level)
      ci_out <- unlist(by_x_ci[x_out_i, c("reject_ci_lo", "reject_ci_hi")])
      by_x_out <- by_x_1[[x_out_i]]

      if (progress) {
        x_out_str <- formatC(x_out,
                             digits = switch(x, n = 0, es = 4),
                             format = "f")
        cat("- Value with closest power:", x_out_str, "\n")
        cat("- Estimated power for ",
            x_out_str,
            ": ",
            formatC(power_out, digits = 4, format = "f"),
            "\n",
            sep = "")
      }

    } else {

      # ==== Current solution: By power curve ====

      # ci_hit-point: OK
      # close_enough-point: OK
      # close_enough-lb/ub: WIP

      # TODO:
      # - Make sure that adjusted power is used with close_enough-lb/ub

      # The desired value may not be within the range examined
      # Use the latest power curve to estimate the desired value.
      # Inaccurate, but help approaching the target value.

      # ** x_out, power_out, nrep_out, ci_out, by_x_out **
      # Considered a candidate solution.
      x_tried <- switch(x,
                        n = as.numeric(names(by_x_1)),
                        es = sapply(by_x_1,
                                    \(x) {attr(x, "pop_es_value")},
                                    USE.NAMES = FALSE))
      x_out <- estimate_x_range(power_x_fit = fit_1,
                                x = x,
                                target_power = target_power,
                                k = 1,
                                tolerance = 0,
                                power_min = power_min,
                                power_max = power_max,
                                interval = x_interval,
                                extendInt = extendInt,
                                x_to_exclude = x_tried)
      by_x_out <- switch(x,
                         n = power4test_by_n(object,
                                             n = x_out,
                                             nrep = nrep_seq[1],
                                             R = R_seq[1],
                                             progress = simulation_progress,
                                             save_sim_all = save_sim_all),
                         es = power4test_by_es(object,
                                               pop_es_name = pop_es_name,
                                               pop_es_values = x_out,
                                               nrep = nrep_seq[1],
                                               R = R_seq[1],
                                               progress = simulation_progress,
                                               save_sim_all = save_sim_all))
      nrep_out <- nrep_seq[1]
      power_out <- rejection_rates(by_x_out)[1, "reject"]
      by_x_out_ci <- rejection_rates_add_ci(by_x_out,
                                            level = ci_level)
      ci_out <- unlist(by_x_out_ci[1, c("reject_ci_lo", "reject_ci_hi")])

      if (progress) {
        x_out_str <- formatC(x_out,
                            digits = switch(x, n = 0, es = 4),
                            format = "f")
        cat("- Extrapolated value:", x_out_str, "\n")
        cat("- Estimated power for ",
            x_out_str,
            ": ",
            formatC(power_out, digits = 4, format = "f"),
            "\n",
            sep = "")
      }

      # Add the results for the new value to
      # the collection of results
      by_x_1 <- c(by_x_1, by_x_out,
                  skip_checking_models = TRUE)

      # Update by_n_out
      by_x_out <- by_x_out[[1]]

      # Update the power curve
      fit_1 <- power_curve(by_x_1,
                           formula = power_model,
                           start = stats::coef(fit_1),
                           lower_bound = lower_bound,
                           upper_bound = upper_bound,
                           nls_control = nls_control,
                           nls_args = nls_args,
                           verbose = progress,
                           models = c("glm", "lm"))

    }

    if (progress) {
      cat("- Power Curve:\n")
      print(fit_i)
      cat("\n")
    }

    x_history[j] <- x_out
    reject_history[j] <- power_out

    # ==== Goal met? ====

    # ci_hit-point: OK
    # close_enough-point: WIP
    # close_enough-lb/ub: WIP

    if (goal == "ci_hit") {

      # ==== Any CI hits target power? ====

      # Check results accumulated so far

      by_x_ci <- rejection_rates_add_ci(by_x_1,
                                        level = ci_level)
      i0 <- (by_x_ci$reject_ci_lo < target_power) &
            (by_x_ci$reject_ci_hi > target_power)

      # Is there at least one CI hitting the target power?
      if (any(i0)) {

        # ==== At least one CI hits target power ====

        # At least one CI hits the target power

        ci_hit <- TRUE

        i2 <- find_ci_hit(by_x_1,
                          ci_level = ci_level,
                          target_power = target_power,
                          final_nrep = final_nrep)
        if (!is.na(i2) && !is.null(i2)) {

          # ==== Solution found ====

          # ci_hit && final_nrep reached

          solution_found <- TRUE

          # Updated *_out objects
          by_x_out <- by_x_1[[i2]]
          x_out <- switch(x,
                          n = by_x_ci$n[i2],
                          es = by_x_ci$es[i2])
          power_out <- by_x_ci$reject[i2]
          nrep_out <- by_x_ci$nrep[i2]
          ci_out <- unlist(by_x_ci[i2, c("reject_ci_lo", "reject_ci_hi")])
        } else {

          # ==== CI hits but final_nrep not reached ====

          # No CI with final_nrep hit
          # Get the closet solution

          i2 <- find_ci_hit(by_x_1,
                            ci_level = ci_level,
                            target_power = target_power,
                            final_nrep = 0)
          # Updated *_out objects
          by_x_out <- by_x_1[[i2]]
          x_out <- switch(x,
                          n = by_x_ci$n[i2],
                          es = by_x_ci$es[i2])
          power_out <- by_x_ci$reject[i2]
          nrep_out <- by_x_ci$nrep[i2]
          ci_out <- unlist(by_x_ci[i2, c("reject_ci_lo", "reject_ci_hi")])
        }

      } else {

        # ==== No CI hits target power ====

        # No CI hits the target power

        ci_hit <- FALSE
      }

      # ==== CI hits? ====

      if (ci_hit) {

        # Is the nrep of the candidate already equal to
        # target nrep for the final solution?

        if (solution_found) {

          # ==== CI hits and solution found. Exit the loop ====

          # Desired accuracy (based on nrep) achieved.
          # Exit the loop and finalize the results.

          if (progress) {
            cat("- Estimated power's CI include the target power (",
                formatC(target_power, digits = 4, format = "f"), "). ",
                "(CI: [", paste0(formatC(ci_out, digits = 4, format = "f"), collapse = ","), "])",
                "\n",
                sep = "")
          }

          status <- power_curve_status_message(0, status)

          break

        } else {

          # ==== CI hits but no solution. Next set of values in _seq ====

          # Move to the next step in the sequences
          # E.g., increase nrep.

          if (length(nrep_seq) > 1) {
            nrep_seq <- nrep_seq[-1]
            final_nrep_seq <- final_nrep_seq[-1]

            if (progress) {
              cat("- Minimum number of replications changed to",
                  nrep_seq[1], "\n\n")
            }

            R_seq <- R_seq[-1]
            xs_per_trial_seq <- xs_per_trial_seq[-1]
          }
        }
      } else {

        # ==== No CI hits ====

        # No value has CI hitting the target power.

        if (progress) {
          cat("- Estimated power's CI does not include the target power (",
              formatC(target_power, digits = 4, format = "f"), "). ",
              "(CI: [", paste0(formatC(ci_out, digits = 4, format = "f"), collapse = ","), "])",
              "\n",
              sep = "")
        }
      }

    }

    if (goal == "close_enough") {
      # TODO:
      # - Add a section for close_enough
    }

    # ==== Check changes ====

    # ci_hit-point: OK
    # close_enough-point: OK
    # close_enough-lb/ub: OK

    changes_ok <- check_changes(
            x_history = x_history,
            delta_tol = delta_tol,
            last_k = last_k
          )

    if (!changes_ok) {

      status <- power_curve_status_message(2, status)
      break

    }

  }

  # ==== End the Loop ====

  # ci_hit-point: OK
  # close_enough-point: OK
  # close_enough-lb/ub: OK

  if (!solution_found) {
    status <- power_curve_status_message(1, status)
  }

  # ==== Return the output ====

  # ci_hit-point: OK
  # close_enough-point: OK
  # close_enough-lb/ub: WIP

  # TODO:
  # - Check whether there will be a conflict
  #   between adjusted power and target power.

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
              iteration = j,
              x_history = x_history[!is.na(x_history)],
              reject_history = reject_history[!is.na(reject_history)],
              delta_tol = delta_tol,
              last_k = last_k,
              what = "point",
              goal = "ci_hit")
  out
}

#' @noRd

power_algorithm_search_by_curve_pre_i <- function(object,
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
                                                  final_R,
                                                  final_xs_per_trial,
                                                  pre_i_xs = 5,
                                                  pre_i_nrep = 50,
                                                  pre_i_R = ifelse(is.null(R0),
                                                                   NULL,
                                                                   min(200, R0)),
                                                  what = "point",
                                                  goal = "ci_hit",
                                                  ci_level = .95,
                                                  tol = .02) {

  # goal: ci_hit
  #   what: point
  #     - No need to change
  # goal: close_enough
  #   what: point
  #     - No need to change. The pre-search setup does not use tolerance.
  #   what: ub, lb
  #     - TODO: Use adjusted_power from target_power_adjusted()

  if (progress) {
    cat("\n--- Pre-iteration Crude Search ---\n\n")
    tmp <- format(Sys.time(), "%Y-%m-%d %X")
    cat("- Start at", tmp, "\n")
  }

  # ==== Initial values ====

  x_i <- set_x_range(object,
                    x = x,
                    pop_es_name = pop_es_name,
                    target_power = target_power,
                    k = pre_i_xs,
                    x_max = x_max,
                    x_min = x_min)

  # ==== Add x_interval (if x_include_interval) ====

  if (x_include_interval) {
    # Include the lowest and highest values in interval
    x_i <- sort(c(x_interval, x_i))
  }
  x_i <- sort(unique(x_i))

  # ==== Exclude existing values ====

  x0 <- switch(x,
              n = attr(object, "args")$n,
              es = pop_es(object,
                          pop_es_name = pop_es_name))
  x_i <- setdiff(x_i, x0)

  if (progress) {
    x_i_str <- formatC(x_i,
                      digits = switch(x, n = 0, es = 3),
                      format = "f")
    cat("- Value(s) to try: ",
        paste0(x_i_str, collapse = ", "),
        "\n")
    cat("- Crude search with",
        pre_i_nrep,
        "replications\n")
    cat("- R =",
        pre_i_R,
        "\n")
  }

  # ==== Estimate power ====

  # ** by_x_i **
  # The current (i-th) set of values examined,
  # along with their results.
  by_x_i <- switch(x,
                  n = power4test_by_n(object,
                                      n = x_i,
                                      nrep = pre_i_nrep,
                                      R = pre_i_R,
                                      progress = simulation_progress,
                                      save_sim_all = save_sim_all),
                  es = power4test_by_es(object,
                                        pop_es_name = pop_es_name,
                                        pop_es_values = x_i,
                                        nrep = pre_i_nrep,
                                        R = pre_i_R,
                                        progress = simulation_progress,
                                        save_sim_all = save_sim_all))

  # ==== Update by_x (by_x_i) ====

  # Add the input object to the list
  if (is_by_x) {
    # Object is an output of *_by_n() or *_by_es()
    by_x_i <- c(by_x_i,
                object_by_org,
                skip_checking_models = TRUE)
  } else {

    tmp <- switch(x,
            n = as.power4test_by_n(object),
            es = as.power4test_by_es(object,
                                     pop_es_name = pop_es_name)
          )
    by_x_i <- c(by_x_i, tmp,
                skip_checking_models = TRUE)
  }

  if (progress) {
    cat("\n- Rejection Rates:\n\n")
    tmp <- rejection_rates(by_x_i)
    print(tmp,
          annotation = FALSE)
    cat("\n")
  }

  # ==== Update power curve (fit_i) ====

  # ** fit_i **
  # The current power curve, based on by_x_i
  fit_i <- power_curve(by_x_i,
                      formula = power_model,
                      start = start,
                      lower_bound = lower_bound,
                      upper_bound = upper_bound,
                      nls_control = nls_control,
                      nls_args = nls_args,
                      verbose = progress,
                      models = c("glm", "lm"))

  if (progress) {
    cat("- Power Curve:\n")
    # Can use the print method of power_curve objects
    print(fit_i)
    cat("\n")
  }

  # ** by_x_1 **
  # The collection of all values tried and their results
  # to be updated when new value is tried.
  # Used after the end of the loop.
  by_x_1 <- by_x_i

  # ** fit_1 **
  # The latest power curve
  # To be updated whenever by_x_1 is updated.
  # Used after the end of the loop.
  fit_1 <- fit_i

  # === Initialize the Sequences ===
  # The sequence will be updated when nrep_step is initiated,
  # to successively increase precision and speed by
  # - increasing the number of replication,
  # - increasing the number of resampling, and
  # - decreasing the number of values to try.

  # The sequence of the numbers of replication
  # new_nrep <- rejection_rates(by_x_1,
  #                             all_columns = TRUE)$nrep

  # ==== Generate sequences of values ====

  # new_nrep <- ceiling(mean(new_nrep))
  new_nrep <- nrep0
  nrep_seq <- ceiling(seq(from = new_nrep,
                          to = final_nrep,
                          length.out = nrep_steps + 1))
  final_nrep_seq <- ceiling(seq(from = ceiling(mean(c(new_nrep, final_nrep))),
                                to = final_nrep,
                                length.out = nrep_steps + 1))

  # The sequence of the Rs (for boot and MC CI)
  # R0 <- attr(object, "args")$R
  if (!is.null(R0)) {
    R_seq <- ceiling(seq(from = R0,
                        to = final_R,
                        length.out = nrep_steps + 1))
  } else {
    R_seq <- NULL
  }

  # The sequence of the numbers of values per trial
  xs_per_trial_seq <- ceiling(seq(from = xs_per_trial,
                                  to = final_xs_per_trial,
                                  length.out = nrep_steps + 1))

  # ==== Return the output ====

  out <- list(x_i = x_i,
              by_x_i = by_x_i,
              fit_i = fit_i,
              by_x_1 = by_x_1,
              fit_1 = fit_1,
              nrep_seq = nrep_seq,
              final_nrep_seq = final_nrep_seq,
              R_seq = R_seq,
              xs_per_trial_seq = xs_per_trial_seq)

  return(out)
}

#' @noRd

power_curve_status_message <- function(x,
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
