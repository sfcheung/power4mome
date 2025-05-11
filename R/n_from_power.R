#' @title Sample Size Determination
#'
#' @description It searches by simulation
#' the sample size with power to
#' detect an effect close to a target
#' value.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' It returns a `n_from_power` object,
#' which is a list with the following
#' elements:
#'
# TODO
#'
#' @param object A `power4test` object,
#' which is the output of [power4test()].
#'
#' @param target_power The target power,
#' a value greater than 0 and less than
#' one.
#'
#' @param ns_per_trial The initial number
#' of sample sizes to consider in each
#' trial. Should be an integer at least
#' 1. Rounded
#' up if not an integer.
#'
#' @param ci_level The level of confidence
#' of the confidence intervals computed
#' for the estimated power. Default is
#' .95.
#'
#' @param power_min,power_max The minimum
#' and maximum values, respectively,
#' of power
#' when determining the sample sizes to
#' try in each trail. Default is .01.
#'
#' @param n_interval A vector of
#' two values, the minimum sample size
#' and the maximum sample size, in
#' the search for sample sizes.
#'
#' @param extendInt Whether `n_interval`
#' can be expanded when estimating the
#' sample sizes to try. The value will
#' be passed to the argument of the
#' same name in [stats::uniroot()],
#' but the default value is `"upX"`.
#' That is, sample size higher than
#' the maximum in `n_interval` is
#' allowed, if predicted by the tentative
#' model.
#'
#' @param progress Logical. Whether
#' the searching progress is reported.
#'
#' @param simulation_progress Logical.
#' Whether the progress in each call
#' to [power4test()] or [power4test_by_n()]
#' is shown. To be passed to
#' the `progress` argument of these two
#' functions.
#'
#' @param max_trials The maximum number
#' of trials in searching the sample
#' size with the target power. Rounded
#' up if not an integer.
#'
#' @param final_nrep The number of
#' replications in the final stage,
#' also the maximum number of replications
#' in each call to [power4test()] or
#' [power4test_by_n()].
#'
#' @param final_R The number of
#' Monte Carlo simulation or
#' bootstrapping samples in the final
#' stage. The `R` in calling
#' [power4test()] or [power4test_by_n()]
#' will be stepped up to this value
#' when approaching to the target
#' power. Do not need to be very large
#' because the goal is to estimate
#' power by replications, not for high
#' precision in one single replication.
#'
#' @param nrep_steps How many steps
#' the number of replications will be
#' increased to `final_nrep`, if the
#' initial number of replications
#' (`nrep` in [power4test()]) is
#' less than `final_nrep`. The number
#' of replications will be successively
#' increased by this number of steps
#' to increase the precision in estimating
#' the power. Should be at least 1.
#' Increasing this number will result
#' in more trials and take longer to
#' run, but will try more sample sizes.
#' Rounded up if not an integer.
#'
#' @param seed If not `NULL`, [set.seed()]
#' will be used to make the process
#' reproducible. This is not always
#' possible if many stages of
#' parallel processing is involved.
#'
#' @param n_include_interval Logical.
#' Whether `n_interval` is mandatory
#' to be included in the sample sizes
#' to be searched.
#'
#' @param power_curve The nonlinear
#' model to be used when estimating
#' the relation between power and
#' sample size. Should be a formula
#' acceptable by [stats::nls()],
#' with `power` on the left-hand side,
#' and `n` in the right-hand
#' side, with one or more parameters.
#' Users rarely need to change the
#' default value.
#'
#' @param start A named numeric vector
#' of the starting values for `power_curve`
#' when fitted by [stats::nls()].
#' Users rarely need to change the
#' default values.
#'
#' @param lower_bound A named numeric vector
#' of the lower bounds for parameters
#' in `power_curve`
#' when fitted by [stats::nls()].
#' Users rarely need to change the
#' default values.
#'
#' @param nls_args A named list of
#' arguments to be used when calling
#' [stats::nls()]. Used to override
#' internal default, such as the
#' algorithm (default is `"port"`).
#' Used with cautions.
#'
#' @param nls_control A named list of
#' arguments to be passed the `control`
#' argument of [stats::nls()] when
#' estimating the relation between
#' power and sample size. The values will
#' override internal default values,
#' and also override `nls_args`.
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#' # TO PREPARE
#' x <- 1
#' \donttest{
#' }
#'
#' @importFrom graphics abline arrows par points text title
#'
#' @export
n_from_power <- function(object,
                         target_power = .80,
                         ns_per_trial = 3,
                         ci_level = .95,
                         power_min = .01,
                         power_max = .90,
                         n_interval = c(50, 2000),
                         extendInt = c("upX", "no", "yes", "downX"),
                         progress = TRUE,
                         simulation_progress = TRUE,
                         max_trials = 10,
                         final_nrep = 500,
                         final_R = 1000,
                         nrep_steps = 1,
                         seed = NULL,
                         n_include_interval = FALSE,
                         power_curve = power ~ I((n - c0)^e) / (b + I((n - c0)^e)),
                         start = c(b = 2, c0 = 100, e = 1),
                         lower_bound = c(b = 0, c0 = 0, e = 1),
                         nls_control = list(),
                         nls_args = list()
                         ) {

  # Inputs
  # - Target power
  # Steps
  # - Get the initial sample size and power.
  # - Find the k sample sizes to explore.
  # - Fit the curve.
  # - See if the target power is in this range.
  #   - NO: Expand.
  #   - YES: Narrow.
  # - Stop when convergence achieved.
  # Outputs
  # - Initial power4test object.
  # - Final power4test object.
  # - Final model by nls.

  extendInt <- match.arg(extendInt)

  a <- abs(stats::qnorm((1 - ci_level) / 2))
  power_tolerance_in_interval <- a * sqrt(target_power * (1 - target_power) / final_nrep)
  power_tolerance_in_final <- a * sqrt(target_power * (1 - target_power) / final_nrep)

  # Sanity Checks
  if (target_power <= 0 || target_power >= 1) {
    stop("'target power' (",
         target_power,
         ") not between 0 and 1.")
  }

  if (ns_per_trial < 1) {
    stop("'ns_per_trial' (",
         ns_per_trial,
         ") is less than 1.")
  }
  ns_per_trial <- ceiling(ns_per_trial)

  if (power_min <= 0 || power_max >= 1) {
    stop("'power_min' and 'power_max' must be between 0 and 1.")
  }

  if (isTRUE(power_max < power_min)) {
    stop("'power_max' must be greater than 'power_min'.")
  }

  if (power_max < target_power || power_min > target_power) {
    stop("'target_power' must be between 'power_min' and 'power_max'.")
  }

  if (min(n_interval) < 0) {
    stop("The minimum value of 'n_interval' cannot be negative")
  }
  if (length(n_interval) != 2) {
    stop("'n_interval' must be a vector with exactly two values.")
  }
  if (n_interval[2] < n_interval[1]) {
    stop("'n_interval' must be of the form c(minimum, maximum).")
  }

  n_max <- max(n_interval)

  max_trials <- ceiling(max_trials)
  if (max_trials < 1) {
    stop("'max_trials' must be at least 1 (after rounding, if necessary).")
  }

  nrep_steps <- ceiling(nrep_steps)
  if (nrep_steps < 1) {
    stop("'nrep_steps' must be at least 1 (after rounding, if necessary).")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  time_start <- Sys.time()

  # Initial Trial
  if (progress) {
    cat("\n--- Pre-iteration Search ---\n\n")
    tmp <- format(Sys.time(), "%Y-%m-%d %X")
    cat("- Start at", tmp, "\n")
  }

  n_i <- set_n_range(object,
                     target_power = target_power,
                     k = ns_per_trial,
                     n_max = n_max)
  n0 <- attr(object, "args")$n
  n_i <- setdiff(n_i, n0)
  if (n_include_interval) {
    n_i <- c(n_interval, n_i)
  }
  n_i <- sort(unique(n_i))
  if (progress) {
    cat("- Sample sizes to try: ",
        paste0(n_i, collapse = ", "),
        "\n")
  }
  by_n_i <- power4test_by_n(object,
                            n = n_i,
                            progress = simulation_progress)

  # Add object to the list
  tmp <- list(object)
  class(tmp) <- c("power4test_by_n", class(tmp))
  names(tmp) <- as.character(n0)
  by_n_i <- c(by_n_i, tmp)

  if (progress) {
    cat("- Rejection Rates:\n")
    tmp <- get_rejection_rates_by_n(by_n_i)
    print(tmp)
    cat("\n")
  }
  fit_i <- power_curve_n(by_n_i,
                         formula = power_curve,
                         start = start,
                         lower_bound = lower_bound,
                         nls_control = nls_control,
                         nls_args = nls_args)
  if (progress) {
    cat("- Power Curve:\n")
    plot_power_curve(by_n_i,
                     power_n_fit = fit_i)
    title(paste0("Pre-iteration Search"))
    fit_tmp <- fit_i
    fit_tmp$data <- "(Omitted)"
    print(fit_tmp)
    cat("\n")
  }

  fit_1 <- fit_i
  by_n_1 <- by_n_i
  new_nrep <- get_rejection_rates_by_n(by_n_1,
                                        all_columns = TRUE)$nrep
  new_nrep <- ceiling(mean(new_nrep))
  nrep_seq <- ceiling(seq(from = new_nrep,
                          to = final_nrep,
                          length.out = nrep_steps + 1))
  final_nrep_seq <- ceiling(seq(from = final_nrep * .50,
                                to = final_nrep,
                                length.out = nrep_steps + 1))
  R0 <- attr(object, "args")$R
  if (!is.null(R0)) {
    R_seq <- ceiling(seq(from = R0,
                        to = final_R,
                        length.out = nrep_steps + 1))
  } else {
    R_seq <- NULL
  }
  ns_per_trial_seq <- ceiling(seq(from = ns_per_trial,
                                   to = 2,
                                   length.out = nrep_steps + 1))
  ci_hit <- FALSE
  for (j in seq_len(max_trials)) {
    if (progress) {
      cat("\n\n--- Trial", j, "---\n\n")
      tmp <- format(Sys.time(), "%Y-%m-%d %X")
      cat("- Start at", tmp, "\n")
    }
    # Target power is inserted, hence k can be greater than
    # ns_per_trial by one.
    if (ci_hit) {
      power_tolerance_in_interval <- max(abs(ci_out - power_out))
    }
    n_j <- estimate_n_range(power_n_fit = fit_1,
                            target_power = target_power,
                            k = ns_per_trial_seq[1],
                            tolerance = power_tolerance_in_interval,
                            power_min = power_min,
                            power_max = power_max,
                            interval = n_interval,
                            extendInt = extendInt,
                            n_to_exclude = as.numeric(names(by_n_1)))
    power_j <- predict_fit(fit_1,
                           newdata = list(n = n_j))
    nrep_j <- nrep_from_power(power_j = power_j,
                              target_power = target_power,
                              tolerance = power_tolerance_in_final,
                              nrep_min = nrep_seq[1],
                              nrep_max = final_nrep_seq[1])
    if (progress) {
      cat("- Sample sizes to try:",
          paste0(n_j, collapse = ", "),
          "\n")
      cat("- Numbers of replications:",
          paste0(nrep_j, collapse = ", "),
          "\n")
    }
    by_n_j <- power4test_by_n(object,
                              n = n_j,
                              R = R_seq[1],
                              progress = simulation_progress,
                              by_nrep = nrep_j)
    by_n_1 <- c(by_n_1, by_n_j)
    if (progress) {
      cat("- Rejection Rates:\n")
      tmp <- get_rejection_rates_by_n(by_n_1)
      print(tmp)
      cat("\n")
    }
    fit_1 <- power_curve_n(by_n_1,
                           formula = power_curve,
                           start = stats::coef(fit_1),
                           lower_bound = lower_bound,
                           nls_control = nls_control,
                           nls_args = nls_args)
    tmp1 <- get_rejection_rates_by_n(by_n_1,
                                     all_columns = TRUE)
    tmp1$reject <- tmp1$sig
    tmp2 <- range(tmp1$reject)
    target_in_range <- (target_power > tmp2[1]) &&
                       (target_power < tmp2[2])
    if (target_in_range) {
      tmp3 <- abs(tmp1$reject - target_power)
      n_out_i <- which.min(tmp3)
      n_out <- tmp1$n[n_out_i]
      power_out <- tmp1$reject[n_out_i]
      nrep_out <- tmp1$nrep[n_out_i]
      by_n_ci <- rejection_rates_add_ci(by_n_1,
                                        level = ci_level)
      ci_out <- unlist(by_n_ci[n_out_i, c("reject_ci_lo", "reject_ci_hi")])
      if (progress) {
        cat("- Sample size with closest power:", n_out, "\n")
      }
      if (progress) {
        cat("- Estimated power for ",
            n_out,
            ": ",
            formatC(power_out, digits = 4, format = "f"),
            "\n",
            sep = "")
      }
      by_n_out <- by_n_1[[n_out_i]]
    } else {
      n_out <- estimate_n_range(power_n_fit = fit_1,
                                target_power = target_power,
                                k = 1,
                                tolerance = 0,
                                power_min = power_min,
                                power_max = power_max,
                                interval = n_interval,
                                extendInt = extendInt,
                                n_to_exclude = as.numeric(names(by_n_1)))
      if (progress) {
        cat("- Extrapolated sample size:", n_out, "\n")
      }
      # Use only final_nrep * .50 because it is an estimate
      # test_nrep <- max(final_nrep * .50,
      #                  nrep_j)
      by_n_out <- power4test_by_n(object,
                                  n = n_out,
                                  nrep = nrep_seq[1],
                                  R = R_seq[1],
                                  progress = simulation_progress)
      nrep_out <- nrep_seq[1]
      power_out <- get_rejection_rates_by_n(by_n_out)[1, "reject"]
      by_n_out_ci <- rejection_rates_add_ci(by_n_out,
                                            level = ci_level)
      ci_out <- unlist(by_n_out_ci[1, c("reject_ci_lo", "reject_ci_hi")])
      if (progress) {
        cat("- Estimated power for ",
            n_out,
            ": ",
            formatC(power_out, digits = 4, format = "f"),
            "\n",
            sep = "")
      }
      # Update the results
      by_n_1 <- c(by_n_1, by_n_out)
      fit_1 <- power_curve_n(by_n_1,
                            formula = power_curve,
                            start = stats::coef(fit_1),
                            lower_bound = lower_bound,
                            nls_control = nls_control,
                            nls_args = nls_args)
      # Update by_n_out
      by_n_out <- by_n_out[[1]]
    }

    if (progress) {
      cat("- Power Curve:\n")
      plot_power_curve(by_n_1,
                       power_n_fit = fit_1)
      title(paste0("Trial ", j))
      abline(h = target_power,
             lty = "dotted",
             lwd = 2)
      fit_tmp <- fit_1
      fit_tmp$data <- "(Omitted)"
      print(fit_tmp)
      cat("\n")
    }

    power_diff <- abs(power_out - target_power)
    by_n_ci <- rejection_rates_add_ci(by_n_1,
                                      level = ci_level)
    i0 <- (by_n_ci$reject_ci_lo < target_power) &
          (by_n_ci$reject_ci_hi > target_power)
    if (any(i0)) {
      # Update based on CI and SE
      ci_hit <- TRUE
      i1 <- rank(by_n_ci$reject_se)
      i2 <- which(i1 == min(i1[i0]))[1]
      by_n_out <- by_n_1[[i2]]
      power_out <- by_n_ci$reject[i2]
      nrep_out <- by_n_ci$nrep[i2]
      ci_out <- unlist(by_n_ci[i2, c("reject_ci_lo", "reject_ci_hi")])
    } else {
      ci_hit <- FALSE
    }

    # if (power_diff <= power_tolerance_in_final) {
    if (ci_hit) {
      # if (length(nrep_seq) == 1) {
      if (nrep_out == final_nrep) {
        # Used maximum
        if (progress) {
          cat("- Estimated power is close enough to target power (",
              formatC(target_power, digits = 4, format = "f"), "). ",
              "(CI: [", paste0(formatC(ci_out, digits = 4, format = "f"), collapse = ","), "])",
              "\n",
              sep = "")
        }
        break
      } else {
        # Increase minimum nrep
        if (length(nrep_seq) > 1) {
          nrep_seq <- nrep_seq[-1]
          final_nrep_seq <- final_nrep_seq[-1]
          if (progress) {
            cat("- Minimum number of replications changed to",
                nrep_seq[1], "\n")
          }
          R_seq <- R_seq[-1]
          ns_per_trial_seq <- ns_per_trial_seq[-1]
        }
      }
    } else {
      if (progress) {
        # cat("- Estimated power is not close enough to target power (",
        #     target_power, "). ",
        #     "(Tolerance: ", power_tolerance_in_final, ")",
        #     "\n",
        #     sep = "")
        cat("- Estimated power is not close enough to target power (",
            formatC(target_power, digits = 4, format = "f"), "). ",
            "(CI: [", paste0(formatC(ci_out, digits = 4, format = "f"), collapse = ","), "])",
            "\n",
            sep = "")
      }
    }

  }

  # No need for final estimation
  if (progress) {
    cat("\n\n--- Final Stage ---\n\n")
    tmp <- format(Sys.time(), "%Y-%m-%d %X")
    cat("- Start at", tmp, "\n")

    cat("- Rejection Rates:\n")
    tmp <- get_rejection_rates_by_n(by_n_1)
    print(tmp)
    cat("\n")
    cat("- Estimated Power Curve:\n")
    plot_power_curve(by_n_1,
                     power_n_fit = fit_1)
    abline(h = target_power,
           lty = "dotted",
           lwd = 2)
    if (ci_hit) {
      abline(v = n_out,
            lty = "dotted",
            lwd = 2)
    }
    title("Final Power Curve")
    fit_tmp <- fit_1
    fit_tmp$data <- "(Omitted)"
    print(fit_tmp)
    cat("\n")
  }

  if (ci_hit) {
    by_n_final <- by_n_out
    tmp <- get_rejection_rates(by_n_final)
    power_final <- tmp$reject
    ci_final <- ci_out
    nrep_final <- nrep_out
  } else {
    by_n_final <- NA
    power_final <- NA
    ci_final <- NA
    nrep_final <- NA
  }
  if (progress) {
    if (ci_hit) {
      cat("\n")
      cat("- Final Sample Size:", n_out, "\n")
      cat("- Final Estimated Power:",
          formatC(power_final, digits = 4, format = "f"), "\n")
      cat("- Confidence Interval: [",
          paste0(formatC(ci_final, digits = 4, format = "f"), collapse = "; "),
          "]\n", sep = "")
      cat("- CI Level: ",
          formatC(ci_level*100, digits = 2, format = "f"), "%", "\n", sep = "")
    } else {
      cat("\n")
      cat("- None of the sample sizes examined",
          "in the interval meet the target power.\n")
      n_x <- estimate_n_range(power_n_fit = fit_1,
                              target_power = target_power,
                              k = 1,
                              tolerance = 0,
                              power_min = power_min,
                              power_max = power_max,
                              interval = n_interval,
                              extendInt = "yes",
                              n_to_exclude = as.numeric(names(by_n_1)))
      if (!is.na(n_x)) {
        cat("- The estimated required sample size is ",
            n_x,
            ".\n", sep = "")
      }
      cat("- Try expanding the range of sample sizes",
          "by setting 'n_interval'.\n")
    }
  }
  my_call <- as.list(match.call())[-1]
  args <- formals()
  args <- utils::modifyList(args,
                            my_call)
  args$object <- NULL
  reject_1 <- get_rejection_rates_by_n(by_n_1)
  time_end <- Sys.time()
  out <- list(power4test_trials = by_n_1,
              rejection_rates = reject_1,
              n_tried = reject_1$n,
              power_tried = reject_1$reject,
              n_final = n_out,
              power_final = power_final,
              ci_final = ci_final,
              nrep_final = nrep_final,
              power_curve = fit_1,
              target_power = target_power,
              power_tolerance = power_tolerance_in_final,
              start = time_start,
              end = time_end,
              time_spent = difftime(time_end, time_start),
              args = args,
              call = match.call())
  class(out) <- c("n_from_power", class(out))
  return(out)
}

#' @rdname n_from_power
#'
#' @param x A `n_from_power` object,
#' the output of [n_from_power()].
#'
#' @param ... Optional arguments.
#' Not used for now.
#'
#' @export
plot.n_from_power <- function(x,
                              ...) {
  plot_power_curve(x$power4test_trials,
                   x$power_curve)
  abline(h = x$target_power,
         lty = "dotted")
  abline(v = x$n_final,
         col = "blue")
  abline(h = x$power_final,
         lty = "dotted",
         col = "blue")
  text(x = x$n_final,
       y = 0,
       labels = x$n_final,
       adj = .5,
       cex = 2)
  tmp <- par("usr")
  text(x = tmp[1] + (tmp[2] - tmp[1]) * .05,
       y = x$power_final,
       labels = x$power_final,
       pos = 3,
       cex = 2)
}
