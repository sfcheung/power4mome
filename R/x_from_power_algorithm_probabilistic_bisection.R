#' @noRd
# Probabilistic Bisection
alg_prob_bisection <- function(
    object,
    x,
    pop_es_name,
    ...,
    target_power = .80,
    x_max,
    x_min,
    progress,
    progress_type = c("cli", "cat"),
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
    extend_maxiter = 5,
    what = c("point", "ub", "lb"),
    goal = c("close_enough", "ci_hit"),
    tol = .005,
    delta_tol = switch(x,
                    n = 5,
                    es = .005),
    delta_tol_f = .01,
    last_k = 5,
    variants = list()
) {

  # Termination Criteria
  # - Number of trials: max_trials
  # - The range of x in the last_k trials is within delta_tol
  # - The range of f in the last_k trials is within delta_tol_f
  # TODO:
  # - Add time allowed?

  # Solution found
  # - Close enough (use tol)
  # - CI hits (use ci)

  progress_type <- match.arg(progress_type)

  # ==== Pre-search setup ====

  a_out <- power_algorithm_prob_bisection_pre_i(
    object = object,
    x = x,
    pop_es_name = pop_es_name,
    target_power = target_power,
    x_max = x_max,
    x_min = x_min,
    progress = progress,
    progress_type = progress_type,
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
                object_by_org[tmp],
                skip_checking_models = TRUE)
  }

  rm(a_out)

  lower_hard <- min(x_interval)
  upper_hard <- max(x_interval)

  # ==== Start the search ====

  a_out <- power_algorithm_prob_bisection(
    object = object,
    x = x,
    pop_es_name = pop_es_name,
    target_power = target_power,
    ci_level = ci_level,
    x_interval = x_interval_updated,
    extendInt = extendInt,
    progress = progress,
    progress_type = progress_type,
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
    delta_tol_f = delta_tol_f,
    last_k = last_k,
    variants = variants
  )

  # ==== Return the output ====

  a_out
}

#' @noRd
power_algorithm_prob_bisection <- function(
                                      object,
                                      x,
                                      pop_es_name = NULL,
                                      ...,
                                      target_power = .80,
                                      ci_level = .95,
                                      x_interval = switch(x, n = c(50, 2000),
                                                             es = c(.00, .70)),
                                      extendInt = c("no", "yes", "downX", "upX"),
                                      progress = TRUE,
                                      progress_type = c("cli", "cat"),
                                      simulation_progress = FALSE,
                                      max_trials = 100,
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
                                      goal = c("close_enough", "ci_hit"),
                                      tol = .005,
                                      delta_tol = -Inf,
                                      delta_tol_f = -Inf,
                                      last_k = 5,
                                      variants = list()) {
  extendInt <- match.arg(extendInt)
  what <- match.arg(what)
  goal <- match.arg(goal)
  progress_type <- match.arg(progress_type)
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

  dfun_history <- vector("list", max_trials)

  p_c_history <- vector("numeric", max_trials)
  p_c_history[] <- NA

  hdi_x_history <- vector("list", max_trials)
  hdi_power_history <- vector("list", max_trials)

  i <- NA

  # what: The value to be examined.
  # goal:
  # - ci_hit: Only relevant for what == "point"
  # - close_enough: Can be used for all what.

  x_type <- x

  # ==== Default for variants ====

  variants0 <- list(use_power_curve_assist = TRUE,
                    use_power_curve_min_points = 2,
                    power_curve_args = list(),
                    use_power_curve_hybrid = TRUE,
                    total_nrep = 5000,
                    initial_nrep = NULL,
                    nrep_step = 0,
                    step_up_factor = 4,
                    step_up_factor_f = 4,
                    trial_nrep = NULL,
                    npoints = 200,
                    p = .60,
                    use_estimated_p = TRUE,
                    adjust_p_c = .90,
                    hdi_prob = .80,
                    hdi_power_tol = .04)
  variants <- utils::modifyList(variants0,
                                variants)

  # ==== Set trial_nrep, initial_nrep, and nrep_step

  if (is.null(variants$trial_nrep)) {
    variants$trial_nrep <- ceiling(variants$total_nrep / max_trials)
  }
  if (is.null(variants$initial_nrep)) {
    variants$initial_nrep <- variants$trial_nrep
    # variants$nrep_step <- 0
  } else if (variants$initial_nrep > variants$trial_nrep) {
    variants$initial_nrep <- variants$trial_nrep
    # variants$nrep_step <- 0
  }
  if (is.null(variants$nrep_step)) {
    variants$nrep_step <- variants$initial_nrep
  }

  # ==== Set proxy_power for power_curve_assist ====

  proxy_power <- NULL
  if (variants$use_power_curve_assist) {
    # TODO:
    # - Can keep for now because we may use power_curve.
    proxy_power <- tryCatch(target_power_adjusted(
                      target_power = target_power,
                      goal = goal,
                      what = what,
                      tolerance = 0,
                      nrep = final_nrep,
                      level = ci_level
                    ),
                    error = function(e) e)
    if (inherits(proxy_power, "error")) {
      # Failed to find the proxy power. Do not use power_curve
      variants$use_power_curve_assist <- FALSE
    }
  }

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
                     nrep = variants$trial_nrep,
                     R = R,
                     what = what,
                     simulation_progress = simulation_progress,
                     save_sim_all = save_sim_all,
                     store_output = TRUE,
                     target_nrep = final_nrep,
                     progress_type = progress_type)

  # ==== Get the initial interval ====

  # Find f.lower and f.upper

  # Arguments for rejection rates should be retrieved from the object
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
      if (progress_type == "cat") {
        cat("\nDo the simulation for the lower bound:\n")
      }
      if (progress_type == "cli") {
        # TODO:
        # - Use cli step
        cat("Do the simulation for the lower bound ...\n")
      }
    }

    # ==== Estimate f for the lower bound  ====

    f.lower <- f(x_i = lower,
                 x = x,
                 pop_es_name = pop_es_name,
                 target_power = target_power,
                 ci_level = ci_level,
                 progress = ifelse(progress_type == "cat", progress, FALSE),
                 progress_type = "cat",
                 digits = digits,
                 nrep = variants$trial_nrep,
                 R = R,
                 what = what,
                 simulation_progress = simulation_progress,
                 save_sim_all = save_sim_all,
                 store_output = TRUE,
                 target_nrep = final_nrep)
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
      if (progress_type == "cat") {
        cat("\nDo the simulation for the upper bound:\n")
      }
      if (progress_type == "cli") {
        # TODO:
        # - Use cli step
        cat("Do the simulation for the upper bound ...\n")
      }
    }

    # ==== Estimate f for the upper bound  ====

    f.upper <- f(x_i = upper,
                 x = x,
                 pop_es_name = pop_es_name,
                 target_power = target_power,
                 ci_level = ci_level,
                 progress = ifelse(progress_type == "cat", progress, FALSE),
                 progress_type = "cat",
                 digits = digits,
                 nrep = variants$trial_nrep,
                 R = R,
                 what = what,
                 simulation_progress = simulation_progress,
                 save_sim_all = save_sim_all,
                 store_output = TRUE,
                 target_nrep = final_nrep)
    by_x_1 <- c(by_x_1, attr(f.upper, "output"),
                skip_checking_models = TRUE)
  }

  if (progress) {
    if (progress_type == "cat") {
      cat("\n")
      print_interval(lower = lower,
                     upper = upper,
                     digits = digits,
                     x_type = x_type,
                     prefix = "Initial interval:")
      cat("\n")
      cat("- Rejection Rates:\n")
      tmp <- rejection_rates(by_x_1,
                            level = ci_level)
      print(tmp, annotation = FALSE)
      cat("\n")
    }
  }

  do_search <- TRUE

  # ==== Is one of the bound a solution?  ====

  # Check whether lower or upper is already a solution

  output_lower <- attr(f.lower, "output")
  # Arguments for rejection rates should be retrieved from the object
  # No need for other arguments. Only `reject` and `nrep` are used.
  tmp <- rejection_rates(
            output_lower,
            all_columns = TRUE
          )
  reject_lower <- tmp$reject
  nrep_lower <- tmp$nrep

  output_upper <- attr(f.upper, "output")
  # Arguments for rejection rates should be retrieved from the object
  # No need for other arguments. Only `reject` and `nrep` are used.
  tmp <- rejection_rates(
            output_upper,
            all_columns = TRUE
          )
  reject_upper <- tmp$reject
  nrep_upper <- tmp$nrep

  tmp <- check_solution_bounds(
            f.lower = f.lower,
            f.upper = f.upper,
            target_power = target_power,
            final_nrep = final_nrep,
            ci_level = ci_level,
            what = what,
            goal = goal,
            tol = tol
          )

  ok_lower <- tmp["ok_lower"]
  ok_upper <- tmp["ok_upper"]

  if (ok_lower || ok_upper) {

    # ==== Solution in the interval. Skip the search ====

    # One of them is a solution. No need to check the interval,
    # even if the interval is below or above the root.

    do_search <- FALSE

  } else {

    # ==== Extend the interval if necessary ====

    # Fix the interval
    # The original interval is returned if it is OK

    if ((sign(f.lower) != sign(f.upper)) &&
        (abs(lower - upper) >= 0)) {
      # ==== The interval is valid ====
      extend_progress <- FALSE
    } else {
      # ==== Extend the interval ====
      extend_progress <- progress
    }

    interval_updated <- extend_interval(f = f,
                                        x = x,
                                        pop_es_name = pop_es_name,
                                        target_power = target_power,
                                        ci_level = ci_level,
                                        nrep = variants$trial_nrep,
                                        target_nrep = final_nrep,
                                        R = R,
                                        what = what,
                                        goal = goal,
                                        tol = tol,
                                        simulation_progress = simulation_progress,
                                        save_sim_all = save_sim_all,
                                        progress = extend_progress,
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
                                        trace = as.numeric(extend_progress),
                                        digits = digits,
                                        store_output = TRUE,
                                        overshoot = switch(x,
                                                           n = .5,
                                                           es = .05),
                                        variants = variants,
                                        proxy_power = proxy_power)
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
    # Arguments for rejection rates should be retrieved from the object
    # No need for other arguments. Only `reject` and `nrep` are used.
    tmp <- rejection_rates(
             output_lower,
             all_columns = TRUE
           )
    reject_lower <- tmp$reject
    nrep_lower <- tmp$nrep

    output_upper <- attr(f.upper, "output")
    # Arguments for rejection rates should be retrieved from the object
    # No need for other arguments. Only `reject` and `nrep` are used.
    tmp <- rejection_rates(
             output_upper,
             all_columns = TRUE
           )
    reject_upper <- tmp$reject
    nrep_upper <- tmp$nrep

    tmp <- check_solution_bounds(
              f.lower = f.lower,
              f.upper = f.upper,
              target_power = target_power,
              final_nrep = final_nrep,
              ci_level = ci_level,
              what = what,
              goal = goal,
              tol = tol
            )

    ok_lower <- tmp["ok_lower"]
    ok_upper <- tmp["ok_upper"]

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

    # ==== Set the prior ====

    f.lower_i <- f.lower
    f.upper_i <- f.upper
    lower_i <- lower
    upper_i <- upper
    status <- 0

    dfun_i <- gen_dfun(
                  interval = c(lower_i, upper_i),
                  npoints = variants$npoints
                )

    p <- variants$p

    x_i <- q_dfun(
                  dfun_i,
                  prob = .50
                )

    if (x_type == "n") {
      x_i <- ceiling(x_i)
    }

    # ==== Start the loop ====

    i <- 1
    nrep_i <- variants$initial_nrep
    nreps_total <- nrep_i
    step_up_factor <- variants$step_up_factor
    step_up_factor_f <- variants$step_up_factor_f
    time_start <- Sys.time()
    pb_id <- NA

    while ((i <= max_trials) &&
           (nreps_total < variants$total_nrep)) {

      # ==== Progress: Set up cli ====

      x_i_str <- switch(
                    x,
                    n = paste0("|n:", as.character(x_i)),
                    es = paste0("|es:",
                                formatC(
                                  x_i,
                                  digits = digits,
                                  format = "f"
                                ))
                  )

      if ((progress_type == "cli") && (is.na(pb_id))) {
        # The progress message for the loop
        cat("Search Progress Note:",
            "- #: Iteration number",
            "- nrep: Number of replications",
            "- n/es: The value to try",
            "- TE: Time elapsed",
            paste0("- ETA: Estimated time to do all ",
                   variants$total_nrep,
                  " replications"),
            "- Rep: The number of replications used out of the total number of replications",
            paste0("- Dx: The range of changes of x in the last ", last_k, " iterations"),
            paste0("- Df: The range of changes of f in the last ", last_k, " iterations"),
            paste0("- PP: The range(s) of probable power"),
            sep = "\n")
        changes_last_k <- character(0)
        changes_last_k_f <- character(0)
        time_passed <- character(0)
        eta <- character(0)
        hdi_f_str <- character(0)
        pb_id <- cli::cli_progress_message(
            " #:{i}|nrep:{nrep_i}{x_i_str}{time_passed}{eta}|Rep:{nreps_total}/{variants$total_nrep}{changes_last_k}{changes_last_k_f}{hdi_f_str}"
          )
      }

      # ==== Progress: Show iteration progress ====

      if (progress) {
        if (progress_type == "cat") {
          cat("\nIteration #", i, "\n")
          cat("\nTotal replications accumulated: ", nreps_total)
          cat("\nNumber of replications for this iteration: ", nrep_i)
          cat("\n")
        }
        if (progress_type == "cli") {
          time_passed0 <- difftime(
                            Sys.time(),
                            time_start
                          )
          time_passed <- paste0("|TE:",
                                format(time_passed0, digits = 3),
                                "")
          if (i >= 2) {
            tmp <- (variants$total_nrep - nreps_total) * time_passed0 / nreps_total
            tmp2 <- tmp
            units(tmp2) <- "secs"
            if (as.numeric(tmp2) > 60 * 60) {
              units(tmp) <- "hours"
            } else if (as.numeric(tmp2) > 60) {
              units(tmp) <- "mins"
            }
            eta <- paste0("|ETA:",
                          format(tmp, digits = 3),
                          "")
          }
          cli::cli_progress_update(id = pb_id)
        }
      }

      # ==== Compute f(x) ====

      out_i <- f(x_i = x_i,
                 x = x,
                 pop_es_name = pop_es_name,
                 target_power = target_power,
                 ci_level = ci_level,
                 progress = progress,
                 progress_type = progress_type,
                 digits = digits,
                 nrep = nrep_i,
                 R = R,
                 what = what,
                 simulation_progress = simulation_progress,
                 save_sim_all = save_sim_all,
                 store_output = TRUE,
                 target_nrep = final_nrep,
                 pb_id = pb_id)

      output_i <- attr(out_i, "output")
      by_x_1 <- c(by_x_1, output_i,
                  skip_checking_models = TRUE)
      # Arguments for rejection rates should be retrieved from the object
      # No need for other arguments. Only `reject` is used.
      reject_i <- rejection_rates(output_i)$reject

      if (progress) {
        if (progress_type == "cat") {
          cat("- Rejection Rates:\n")
          # Arguments for rejection rates should be retrieved from the object
          tmp <- rejection_rates(by_x_1,
                                 level = ci_level)
          print(tmp, annotation = FALSE)
          cat("\n")
        }
      }

      # ==== Histories ====

      f_history[i] <- as.numeric(out_i)
      x_history[i] <- x_i
      # f_interval_history[i, ] <- c(f.lower_i, f.upper_i)
      reject_history[i] <- reject_i

      # ==== Is x a solution?
      #
      # No need to check during the iteration because the
      # nrep will not be the final nrep.
      # The solution will be checked again after the search.

      # ==== Check and display changes in the last_k iterations ====

      # For PBA, changes_ok FALSE is a termination criterion,
      # not a problem.

      tmp0 <- target_power_adjusted(
                target_power = target_power,
                goal = goal,
                what = what,
                tolerance = .001,
                nrep = nrep_i,
                level = ci_level
              )
      tmp1 <- reject_ci_wilson(
                nreject = ceiling(tmp0 * nrep_i),
                nvalid = nrep_i,
                level = .95
              )
      delta_tol_f <- diff(range(tmp1[1, ]))

      changes_ok <- check_changes(
              x_history = x_history,
              delta_tol = delta_tol,
              last_k = last_k
            )
      changes_ok_f <- check_changes(
              x_history = f_history,
              delta_tol = delta_tol_f,
              last_k = last_k
            )

      if (progress && (i >= last_k)) {
        tmp <- x_history[(i - last_k + 1):i]
        tmp <- diff(range(tmp))
        last_k_str <- switch(x_type,
                          n = tmp,
                          es = formatC(tmp,
                                       digits = digits,
                                       format = "f"))
        tmp <- f_history[(i - last_k + 1):i]
        tmp <- diff(range(tmp))
        last_k_f_str <- formatC(
                            tmp,
                            digits = digits,
                            format = "f"
                          )
        if (progress_type == "cat") {
          cat("The range of changes in the last",
              last_k,
              "iteration:",
              last_k_str,
              "\n")
        }
        if (progress_type == "cli") {
          changes_last_k <- paste0(
                                  "|",
                                  "Dx:",
                                  last_k_str)
          changes_last_k_f <- paste0(
                                  "|",
                                  "Df:",
                                  last_k_f_str)
          cli::cli_progress_update(
              id = pb_id
            )
        }
      }

      # ==== Where the solution may be ====

      if (sign(out_i) == sign(f.lower_i)) {

        # ==== Solution on the right ====

        z_i <- 1

      } else {

        # ==== Solution on the left ====

        z_i <- -1

      }

      # ==== Update the density function ====

      if (variants$use_estimated_p) {
        p_c_i <- tryCatch(p_c(
                    target_power = target_power,
                    power_i = reject_i,
                    goal = goal,
                    what = what,
                    trial_nrep = nrep_i,
                    level = ci_level,
                    final_nrep = final_nrep
                  ), error = function(e) e)
        if (!inherits(p_c_i, "error")) {
          p_c_i <- max(variants$adjust_p_c * p_c_i,
                      variants$p)
        } else {
          p_c_i <- variants$p
        }
        p_c_history[i] <- p_c_i
        dfun_i <- update_dfun(
                    dfun = dfun_i,
                    x_i = x_i,
                    p = p_c_i,
                    z_i = z_i
                  )
      } else {
        dfun_i <- update_dfun(
                    dfun = dfun_i,
                    x_i = x_i,
                    p = p,
                    z_i = z_i
                  )
      }

      dfun_history[[i]] <- dfun_i

      x_i <- q_dfun(
                dfun = dfun_i,
                prob = .50
              )

      if (x_type == "n") {
        x_i <- ceiling(x_i)
      }

      crlo <- q_dfun(
                  dfun_i,
                  prob = .10
                )
      crhi <- q_dfun(
                  dfun_i,
                  prob = .90
                )

      # ==== Histories ====

      x_interval_history[i, ] <- c(crlo, crhi)

      # No need to reuse results if x has tried.
      # A new draw gives new information.

      if (progress) {
        # # TODO:
        # # - Decide whether print this.
        # if (progress_type == "cat") {
        #   print_interval(lower = crlo,
        #                  upper = crhi,
        #                  digits = digits,
        #                  x_type = x_type,
        #                  prefix = "80% interval for the posterior distribution:")
        #   cat("Updated x:", x_i, "\n")
        # }
      }

      # ==== Termination: last_k changes ====

      if (!changes_ok && !changes_ok_f) {

        status <- bisection_status_message(3, status)

        cat("** Search ended **: The range of changes of x in the last ",
            last_k,
            " iterations is less than ",
            delta_tol,
            ".\n",
            sep = "")
        cat("** Search ended **: The range of changes of f in the last ",
            last_k,
            " iterations is less than ",
            delta_tol_f,
            ".\n",
            sep = "")
        break

      }

      # ==== Termination: f interval ====

      hdi_x <- hdi(
                  dfun = dfun_i,
                  prob = variants$hdi_prob
                )
      # TODO:
      # - Check the argument values
      fit_tmp <- tryCatch(do.call(
                          power_curve,
                          c(list(object = by_x_1),
                            variants$power_curve_args)
                        ),
                        error = function(e) e)
      if (inherits(fit_tmp, "power_curve")) {
        hdi_power <- lapply(
                        hdi_x,
                        \(x) {
                          stats::predict(
                              fit_tmp,
                              newdata = list(x = x)
                            )
                        }
                      )
      } else {
        hdi_power <- lapply(
                        hdi_x,
                        \(x) c(NA, NA)
                      )
      }
      hdi_x_history[[i]] <- hdi_x
      hdi_power_history[[i]] <- hdi_power

      if (progress &&
          (progress_type == "cli")) {
          tmp1 <- sapply(
                    hdi_power,
                    \(x) sprintf("[%1$4.3f,%2$4.3f]", x[1], x[2])
                  )
          tmp2 <- paste0(tmp1, collapse = ";")
          hdi_f_str <- paste0("|PP:", tmp2)
          cli::cli_progress_update(id = pb_id)
      }

      if ((length(hdi_power) == 1) &&
          isFALSE(all(is.na(hdi_power[[1]]))) &&
          (diff(hdi_power[[1]]) <= variants$hdi_power_tol) &&
          (proxy_power >= hdi_power[[1]][1]) &&
          (proxy_power <= hdi_power[[1]][2])) {
        # TODO:
        # - Add a status
        cli::cli_process_done(id = pb_id)
        cat("\n** Search ended **: The range of probable power is less than ",
            formatC(variants$hdi_power_tol,
                    digits = 4,
                    format = "f"),
            ".\n",
            sep = "")
        break
      }

      # ==== Next i: Set nrep ====

      step_up <- !check_changes(
              x_history = x_history,
              delta_tol = delta_tol * step_up_factor,
              last_k = last_k
            )
      # delta_tol_f is dynamically determined.
      # No need for step_up_factor_f
      step_up_factor_f <- min(step_up_factor, .5)
      step_up_f <- !check_changes(
              x_history = f_history,
              delta_tol = delta_tol_f * step_up_factor_f,
              last_k = last_k
            )
      if (step_up || step_up_f) {
        tmp <- variants$initial_nrep +
               variants$step_up_factor * variants$nrep_step
        nrep_i <- min(final_nrep,
                      nrep_i + variants$nrep_step,
                      tmp)
        if (step_up) {
          step_up_factor <- max(0, step_up_factor - 1)
        }
        # if (step_up_f) {
        #   step_up_factor_f <- max(0, step_up_factor - 1)
        # }
      }

      # ==== Next i: Update nreps_total ====

      nreps_total <- nreps_total + nrep_i

      # ==== Next i ====

      i <- i + 1

    }

    # ==== End the loop ====

    if (progress_type == "cli") {
      cat("\n")
    }

    if (i == max_trials) {
      if (progress) {
        cat("\n** Search ended **: Maximum number of trials reached.\n")
      }
    }

    if (nreps_total >= variants$total_nrep) {
      if (progress) {
        cat("\n** Search ended **: Total number of replications reached.\n")
      }
    }

    if (progress) {
      cat("Summary:\n")
      cat("- Number of iterations:", i, "\n")
      cat("- Number of replications:", nreps_total, "\n")
      cat("- Time elapsed:", format(time_passed0, digits = 4), "\n")
      cat("- The range of changes x in the last",
          last_k,
          "iteration:",
          last_k_str,
          "\n")
      cat("- The range of changes f in the last",
          last_k,
          "iteration:",
          last_k_f_str,
          "\n")
    }

  } else {

    # No iteration

  }

  # TODO:
  # - Run one more time using final_nrep

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

  # ** dfun_out **
  # The density function

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
    dfun_out <- NA

  } else {

    # ==== Solution found. Store it ====

    # TODO:
    # - Check solutions

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

    dfun_out <- dfun_i

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

  # Not used in probabilistic bisection
  # x_interval_history[] <- NA
  f_interval_history[] <- NA

  i_tmp <- !is.na(x_history)

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
              x_history = x_history[i_tmp],
              x_interval_history = x_interval_history[i_tmp, ],
              f_interval_history = f_interval_history[i_tmp, ],
              reject_history = reject_history[i_tmp],
              f_history = f_history[i_tmp],
              dfun_history = dfun_history[i_tmp],
              p_c_history = p_c_history[i_tmp],
              hdi_x_history = hdi_x_history[i_tmp],
              hdi_power_history = hdi_power_history[i_tmp],
              tol = tol,
              delta_tol = delta_tol,
              last_k = last_k,
              what = what,
              goal = goal,
              ci_level = ci_level,
              dfun_out = dfun_out)
  out

}


#' @noRd
power_algorithm_prob_bisection_pre_i <- function(object,
                                            x,
                                            pop_es_name,
                                            target_power,
                                            x_max,
                                            x_min,
                                            progress,
                                            progress_type = c("cat", "cli"),
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

  progress_type <- match.arg(progress_type)

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
gen_dfun <- function(interval = c(50, 2000),
                     npoints = NULL,
                     integer = NULL
                    ) {
  if (is.null(integer)) {
    # ==== Determine whether treating x as an integer ====
    tmp <- seq(from = interval[1],
               to = interval[2])
    if (length(tmp) == diff(interval) + 1) {
      integer <- TRUE
      npoints <- length(tmp)
    } else {
      integer <- FALSE
    }
  }
  if (integer) {
    x <- seq(from = interval[1],
             to = interval[2])
  } else {
    x <- seq(from = interval[1],
             to = interval[2],
             length = npoints + 1)
    x <- x[-length(x)] + diff(x) / 2
  }
  out <- cbind(x = x,
               prob = 1 / length(x))
  out
}

#' @noRd
dfun_prob <- function(
  dfun,
  x
) {
  i <- which.min(abs(dfun[, "x", drop = TRUE] - x))
  unname(dfun[i, "prob", drop = TRUE])
}

#' @noRd
update_dfun_prob <- function(
  dfun,
  x,
  new_prob
) {
  i <- which.min(abs(dfun[, "x", drop = TRUE] - x))
  dfun[i, "prob"] <- new_prob
  dfun
}

#' @noRd
normalize_dfun <- function(
  dfun
) {
  dfun[, "prob"] <- dfun[, "prob"] / sum(dfun[, "prob"])
  dfun
}

#' @noRd
# Update the density function
update_dfun <- function(
  dfun,
  x_i,
  p,
  z_i
) {
  j <- which.min(abs(dfun[, "x"] - x_i))
  cd_i <- cumsum(dfun[, "prob"])[j]
  gamma_i <- (1 - cd_i) * p +
             cd_i * (1 - p)
  if (z_i > 0) {
    d_new <- dfun[, "prob"] / gamma_i
    p_i <- ifelse(
              dfun[, "x"] >= x_i,
              yes = p,
              no = (1 - p)
            )
    d_new <- d_new * p_i
  } else {
    d_new <- dfun[, "prob"] / (1 - gamma_i)
    p_i <- ifelse(
              dfun[, "x"] >= x_i,
              yes = (1 - p),
              no = p
            )
    d_new <- d_new * p_i
  }
  dfun_new <- dfun
  dfun_new[, "prob"] <- d_new
  dfun_new <- normalize_dfun(dfun_new)
  dfun_new
}

#' @noRd
q_dfun <- function(
  dfun,
  prob = .50
) {
  qfun <- cumsum(dfun[, "prob"])
  tmp <- prob - qfun
  tmp[tmp < 0] <- NA
  i <- which.min(tmp)
  # Do interpolation
  j0 <- qfun[i]
  j1 <- qfun[i + 1]
  yd <- (prob - j0) / (j1 - j0)
  x0 <- dfun[i, "x"]
  x1 <- dfun[i + 1, "x"]
  out <- x0 + yd * (x1 - x0)
  unname(out)
}

#' @noRd
gen_nreps <- function(
  total_nrep = 10000,
  max_trials = 100,
  initial_nrep = 50
) {
  # TODO:
  # - Handle incompatible settings
  a <- total_nrep - initial_nrep * max_trials
  b <- max_trials - 1
  d <- seq(1, b)
  e <- a / sum(d)
  out <- c(initial_nrep, initial_nrep + d * e)
  out <- round(out)
}