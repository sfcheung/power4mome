#' @title Sample Size Determination
#'
#' @description It searches by simulation
#' the sample size with power to
#' detect an effect close to a target
#' value.
#'
#' @details
#' This is how to use this function:
#'
#' - Specify the model by [power4test()],
#'   with `do_the_test = FALSE`, and set
#'   the magnitude of the effect sizes
#'   to the minimum levels to detect.
#'
#' - Add the test using [power4test()]
#'   using `test_fun` and `test_args`
#'   (see the help page of [power4test()]
#'   for details). Run it on the
#'   starting sample size.
#'
#' - Call [n_from_power()] on the output
#'   of [power4test()] returned with
#'   the starting sample size. This
#'   function will iteratively repeat
#'   the analysis on other sample sizes,
#'   trying to find a sample size with
#'   a power level close enough to the
#'   target power.
#'
#' Usually, the default values of the
#' arguments should be sufficient.
#'
#' The results can be viewed using
#' [summary()], and the output has
#' a [plot.n_from_power()] method to
#' plot the relation between power and
#' sample size for the sample sizes
#' examined.
#'
#' ## Technical Details
#'
#' The detailed workflow of this function
#' can be found in the following webpage:
#'
#' https://sfcheung.github.io/power4mome/articles/n_from_power_workflow.html
#'
#'
#' @return
#' The function [n_from_power()]
#' returns a `n_from_power` object,
#' which is a list with the following
#' elements:
#'
#' - `power4test_trials`: The output of
#' [power4test_by_n()] for all sample
#' sizes examined.
#'
#' - `rejection_rates`: The output of
#' [get_rejection_rates_by_n()] from
#' `power4test_trials`.
#'
#' - `n_tried`: The sample sizes
#' examined.
#'
#' - `power_tried`: The estimated
#' rejection rates for all the sample
#' sizes examined.
#'
#' - `n_final`: The sample size in the
#' solution. `NA` if solution not found.
#'
#' - `power_final`: The estimated power
#' of the sample size in the solution.
#' `NA` if solution not found.
#'
#' - `i_final`: The position of the
#' solution in `power4test_trials`.
#' `NA` if solution not found.
#'
#' - `ci_final`: The confidence interval
#' of the estimated power in the solution.
#' `NA` if solution not found.
#'
#' - `ci_level`: The level of confidence
#' of `ci_final`.
#'
#' - `nrep_final`: The number of
#' replications (`nrep`) when estimating
#' the power in the solution.
#'
#' - `power_curve`: The output of
#' [stats::nls()], [stats::glm()], or
#' [stats::lm()] when estimating the
#' power curve.
#'
#' - `target_power`: The requested
#' target power.
#'
#' - `power_tolerance`: The allowed
#' difference between the solution's
#' estimated power and the target
#' power. Determined by the number
#' of replications and the level of
#' confidence of the confidence intervals.
#'
#' - `n_estimated`: The sample size
#' with the target power, estimated by
#' `power_curve`. This is used, when
#' solution not found, to determine the
#' range of sample sizes to search when
#' calling the function again.
#'
#' - `start`: The time and date when
#' the process started.
#'
#' - `end`: The time and date when the
#' process ended.
#'
#' - `time_spent`: The time spent in
#' doing the search.
#'
#' - `args`: A named list of the arguments
#' of [n_from_power()] used in the search.
#'
#' - `call`: The call when this function
#' is called.
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
#' @param power_model The nonlinear
#' model to be used when estimating
#' the relation between power and
#' sample size. Should be a formula
#' acceptable by [stats::nls()],
#' with `reject` on the left-hand side,
#' and `x` (stands for sample size)
#' on the right-hand
#' side, with one or more parameters.
#' Users rarely need to change the
#' default value.
#'
#' @param start A named numeric vector
#' of the starting values for `power_model`
#' when fitted by [stats::nls()].
#' Users rarely need to change the
#' default values.
#'
#' @param lower_bound A named numeric vector
#' of the lower bounds for parameters
#' in `power_model`
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
#' mod <-
#' "
#' m ~ a*x
#' y ~ b*m + x
#' ab := a * b
#' "
#'
#' mod_es <- c("y ~ m" = "l",
#'             "m ~ x" = "m",
#'             "y ~ x" = "n")
#'
#' sim_only <- power4test(nrep = 10,
#'                        model = mod,
#'                        pop_es = mod_es,
#'                        n = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_parameters,
#'                        test_args = list(pars = "ab"))
#'
#' # In real analysis, to have more stable results:
#' # - Use a larger final_nrep (e.g., 500).
#' # - Use the default ns_per_trial of 3, or just remove it.
#'
#' # If the default values are OK, this call is sufficient:
#' # power_vs_n <- n_from_power(test_out,
#' #                            target_power = .80,
#' #                            seed = 4567)
#' power_vs_n <- n_from_power(test_out,
#'                            progress = TRUE,
#'                            target_power = .80,
#'                            final_nrep = 10,
#'                            ns_per_trial = 1,
#'                            nrep_steps = 1,
#'                            max_trials = 1,
#'                            seed = 4567)
#' summary(power_vs_n)
#' plot(power_vs_n)
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
                         power_model = reject ~ I((x - c0)^e) / (b + I((x - c0)^e)),
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

  # === Sanity Checks ===

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
  if (nrep_steps < 0) {
    stop("'nrep_steps' must be at least 1 (after rounding, if necessary).")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  time_start <- Sys.time()

  # === Initial Trial ===

  if (progress) {
    cat("\n--- Pre-iteration Search ---\n\n")
    tmp <- format(Sys.time(), "%Y-%m-%d %X")
    cat("- Start at", tmp, "\n")
  }

  # Set the initial sample sizes to try

  n_i <- set_n_range(object,
                     target_power = target_power,
                     k = ns_per_trial,
                     n_max = n_max)
  # Exclude the sample size in the input object
  n0 <- attr(object, "args")$n
  n_i <- setdiff(n_i, n0)
  if (n_include_interval) {
    # Include the lowest and highest sample sizes in interval
    n_i <- c(n_interval, n_i)
  }
  n_i <- sort(unique(n_i))

  if (progress) {
    cat("- Sample sizes to try: ",
        paste0(n_i, collapse = ", "),
        "\n")
  }

  # ** by_n_i **
  # The current (i-th) set of sample size examined,
  # along with their results.
  by_n_i <- power4test_by_n(object,
                            n = n_i,
                            progress = simulation_progress)

  # Add the input object to the list
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

  # ** fit_i **
  # The current power curve, based on by_n_i
  # fit_i <- power_curve_n(by_n_i,
  #                        formula = power_curve,
  #                        start = start,
  #                        lower_bound = lower_bound,
  #                        nls_control = nls_control,
  #                        nls_args = nls_args,
  #                        verbose = progress)
  fit_i <- power_curve_x(by_n_i,
                         formula = power_model,
                         start = start,
                         lower_bound = lower_bound,
                         nls_control = nls_control,
                         nls_args = nls_args,
                         verbose = progress)

  if (progress) {
    cat("- Power Curve:\n")
    fit_tmp <- fit_i
    fit_tmp$data <- "(Omitted)"
    print(fit_tmp)
    cat("\n")
  }

  # ** by_n_1 **
  # The collection of all sample sizes tried and their results
  # to be updated when new sample size is tried.
  # Used after the end of the loop.
  by_n_1 <- by_n_i

  # ** fit_1 **
  # The latest power curve
  # To be updated whenever by_n_1 is updated.
  # Used after the end of the loop.
  fit_1 <- fit_i

  # === Initialize the Sequences ===
  # The sequence will be updated when nrep_step is initiated,
  # to successively increase precision and speed by
  # - increasing the number of replication,
  # - increasing the number of resampling, and
  # - decreasing the number of sample sizes to try.

  # The sequence of the numbers of replication
  new_nrep <- get_rejection_rates_by_n(by_n_1,
                                        all_columns = TRUE)$nrep
  new_nrep <- ceiling(mean(new_nrep))
  nrep_seq <- ceiling(seq(from = new_nrep,
                          to = final_nrep,
                          length.out = nrep_steps + 1))
  final_nrep_seq <- ceiling(seq(from = final_nrep * .50,
                                to = final_nrep,
                                length.out = nrep_steps + 1))

  # The sequence of the Rs (for boot and MC CI)
  R0 <- attr(object, "args")$R
  if (!is.null(R0)) {
    R_seq <- ceiling(seq(from = R0,
                        to = final_R,
                        length.out = nrep_steps + 1))
  } else {
    R_seq <- NULL
  }

  # The sequence of the numbers of sample sizes per trial
  ns_per_trial_seq <- ceiling(seq(from = ns_per_trial,
                                   to = 2,
                                   length.out = nrep_steps + 1))
  ci_hit <- FALSE

  # === Loop Over The Trials ===

  for (j in seq_len(max_trials)) {

    if (progress) {
      cat("\n\n--- Trial", j, "---\n\n")
      tmp <- format(Sys.time(), "%Y-%m-%d %X")
      cat("- Start at", tmp, "\n")
    }

    if (ci_hit) {
      # After the first trial,
      # Check whether at least one CI hit the target power.
      # If yes, reduce the range of power levels when
      # selecting the sample sizes to try.
      # Necessary because the number of replication may have
      # increased, requiring narrower CI.
      power_tolerance_in_interval <- max(abs(ci_out - power_out))
    }

    # ** n_j **
    # The vector of sample sizes to be tried in this trial
    # Determined using by latest power curve (fit_1)
    n_j <- estimate_n_range(power_n_fit = fit_1$fit,
                            target_power = target_power,
                            k = ns_per_trial_seq[1],
                            tolerance = power_tolerance_in_interval,
                            power_min = power_min,
                            power_max = power_max,
                            interval = n_interval,
                            extendInt = extendInt,
                            n_to_exclude = as.numeric(names(by_n_1)))

    # Adjust the numbers of replication for each sample size.
    # A sample size with estimated power closer to the
    # target power will have a higher number of replication
    # power_j <- predict_fit(fit_1,
    #                        newdata = list(n = n_j))
    power_j <- predict(fit_1,
                       newdata = list(x = n_j))
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

    # ** by_n_j **
    # The results for this trial (based on n_j)
    by_n_j <- power4test_by_n(object,
                              n = n_j,
                              R = R_seq[1],
                              progress = simulation_progress,
                              by_nrep = nrep_j)

    # Add the results to by_n_1
    by_n_1 <- c(by_n_1, by_n_j)

    if (progress) {
      cat("- Rejection Rates:\n")
      tmp <- get_rejection_rates_by_n(by_n_1)
      print(tmp)
      cat("\n")
    }

    # Update the power curve
    # fit_1 <- power_curve_n(by_n_1,
    #                        formula = power_curve,
    #                        start = stats::coef(fit_1),
    #                        lower_bound = lower_bound,
    #                        nls_control = nls_control,
    #                        nls_args = nls_args,
    #                        verbose = progress)
    fit_i <- power_curve_x(by_n_1,
                           formula = power_model,
                           start = start,
                           lower_bound = lower_bound,
                           nls_control = nls_control,
                           nls_args = nls_args,
                           verbose = progress)

    # Get the rejection rates of all sample sizes tried.
    tmp1 <- get_rejection_rates_by_n(by_n_1,
                                     all_columns = TRUE)
    # tmp1$reject <- tmp1$sig
    tmp2 <- range(tmp1$reject)

    # Is the desired sample size likely already in the range
    # of sample sizes examined, based on the estimated power?
    target_in_range <- (target_power > tmp2[1]) &&
                       (target_power < tmp2[2])

    if (target_in_range) {
      # The desired sample size probably within the range examined

      tmp3 <- abs(tmp1$reject - target_power)
      n_out_i <- which.min(tmp3)
      # If ties, the smallest sample size will be used

      # ** n_out, power_out, nrep_out, ci_out, by_n_out **
      # The results of the candidate solution,
      # - The sample size with power closest to the target power
      n_out <- tmp1$n[n_out_i]
      power_out <- tmp1$reject[n_out_i]
      nrep_out <- tmp1$nrep[n_out_i]
      by_n_ci <- rejection_rates_add_ci(by_n_1,
                                        level = ci_level)
      ci_out <- unlist(by_n_ci[n_out_i, c("reject_ci_lo", "reject_ci_hi")])
      by_n_out <- by_n_1[[n_out_i]]

      if (progress) {
        cat("- Sample size with closest power:", n_out, "\n")
        cat("- Estimated power for ",
            n_out,
            ": ",
            formatC(power_out, digits = 4, format = "f"),
            "\n",
            sep = "")
      }

    } else {
      # The desired sample size may not be within the range examined
      # Use the latest power curve to estimate the desired sample size.
      # Inaccurate, but help approaching the target sample size.

      # ** n_out, power_out, nrep_out, ci_out, by_n_out **
      # Considered a candidate solution.
      n_out <- estimate_n_range(power_n_fit = fit_1$fit,
                                target_power = target_power,
                                k = 1,
                                tolerance = 0,
                                power_min = power_min,
                                power_max = power_max,
                                interval = n_interval,
                                extendInt = extendInt,
                                n_to_exclude = as.numeric(names(by_n_1)))
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
        cat("- Extrapolated sample size:", n_out, "\n")
        cat("- Estimated power for ",
            n_out,
            ": ",
            formatC(power_out, digits = 4, format = "f"),
            "\n",
            sep = "")
      }

      # Add the results for the new sample size to
      # the collection of results
      by_n_1 <- c(by_n_1, by_n_out)

      # Update by_n_out
      by_n_out <- by_n_out[[1]]

      # Update the power curve
      # fit_1 <- power_curve_n(by_n_1,
      #                       formula = power_curve,
      #                       start = stats::coef(fit_1),
      #                       lower_bound = lower_bound,
      #                       nls_control = nls_control,
      #                       nls_args = nls_args,
      #                       verbose = progress)
      fit_i <- power_curve_x(by_n_1,
                             formula = power_model,
                             start = stats::coef(fit_1),
                             lower_bound = lower_bound,
                             nls_control = nls_control,
                             nls_args = nls_args,
                             verbose = progress)
    }

    if (progress) {
      cat("- Power Curve:\n")
      fit_tmp <- fit_1
      fit_tmp$data <- "(Omitted)"
      print(fit_tmp)
      cat("\n")
    }

    # Check results accumulated so far

    by_n_ci <- rejection_rates_add_ci(by_n_1,
                                      level = ci_level)
    i0 <- (by_n_ci$reject_ci_lo < target_power) &
          (by_n_ci$reject_ci_hi > target_power)

    # Is there at least one CI hitting the target power?
    if (any(i0)) {
      # At least one CI hits the target power

      ci_hit <- TRUE

      # Find the sample size with CI hitting the target power
      # and has the smallest SE.
      i1 <- rank(by_n_ci$reject_se)
      # Do not consider those with nrep < final_nrep
      i1[by_n_ci$nrep < final_nrep] <- Inf
      # If ties, the smallest sample size will be used
      i2 <- which(i1 == min(i1[i0]))[1]

      # Updated *_out objects
      by_n_out <- by_n_1[[i2]]
      n_out <- by_n_ci$n[i2]
      power_out <- by_n_ci$reject[i2]
      nrep_out <- by_n_ci$nrep[i2]
      ci_out <- unlist(by_n_ci[i2, c("reject_ci_lo", "reject_ci_hi")])

    } else {
      # No CI hits the target power

      ci_hit <- FALSE
    }

    if (ci_hit) {
      # Is the nrep of the candidate already equal to
      # target nrep for the final solution?

      if (nrep_out == final_nrep) {
        # Desired accuracy (based on nrep) achieved.
        # Exit the loop and finalize the results.

        if (progress) {
          cat("- Estimated power is close enough to target power (",
              formatC(target_power, digits = 4, format = "f"), "). ",
              "(CI: [", paste0(formatC(ci_out, digits = 4, format = "f"), collapse = ","), "])",
              "\n",
              sep = "")
        }

        break
      } else {
        # Move to the next step in the sequences
        # E.g., increase nrep.

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
      # No sample size has CI hitting the target power.

      if (progress) {
        cat("- Estimated power is not close enough to target power (",
            formatC(target_power, digits = 4, format = "f"), "). ",
            "(CI: [", paste0(formatC(ci_out, digits = 4, format = "f"), collapse = ","), "])",
            "\n",
            sep = "")
      }
    }

  }

  # End of the loop of trials.

  if (progress) {
    cat("\n\n--- Final Stage ---\n\n")
    tmp <- format(Sys.time(), "%Y-%m-%d %X")
    cat("- Start at", tmp, "\n")

    cat("- Rejection Rates:\n")
    tmp <- get_rejection_rates_by_n(by_n_1)
    print(tmp)
    cat("\n")
    cat("- Estimated Power Curve:\n")
    fit_tmp <- fit_1
    fit_tmp$data <- "(Omitted)"
    print(fit_tmp)
    cat("\n")
  }

  # Is solution found?
  # - At least one CI hits the target power
  # - The maximum number of replications reached.

  if (ci_hit && nrep_out == final_nrep) {
    # Created when ci_hit set to TRUE

    # ** n_final, by_n_final, power_final, ci_final, nrep_final, i_final **
    # The solution.
    n_final <- n_out
    by_n_final <- by_n_out
    power_final <- power_out
    ci_final <- ci_out
    nrep_final <- nrep_out
    i_final <- i2
  } else {
    # No solution found.
    # Force ci_hit to be FALSE.
    # Set the NAs to denote this.
    n_final <- NA
    by_n_final <- NA
    power_final <- NA
    ci_final <- NA
    nrep_final <- NA
    i_final <- NA
  }

  # ** n_x **
  # The estimated sample size based on power_curve.
  # Used as a suggestion when no solution was found.
  n_x <- NA
  if (ci_hit) {
    n_x <- estimate_n_range(power_n_fit = fit_1$fit,
                            target_power = target_power,
                            k = 1,
                            tolerance = 0,
                            power_min = power_min,
                            power_max = power_max,
                            interval = n_interval,
                            extendInt = "yes",
                            n_to_exclude = as.numeric(names(by_n_1)))
  }

  if (progress) {
    if (ci_hit && nrep_out == final_nrep) {
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
      if (!is.na(n_x)) {
        cat("- The estimated required sample size is ",
            n_x,
            ".\n", sep = "")
      }
      cat("- Try expanding the range of sample sizes",
          "by setting 'n_interval'.\n")
    }
  }

  # === Finalize the Output ===

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
              n_final = n_final,
              power_final = power_final,
              i_final = i_final,
              ci_final = ci_final,
              ci_level = ci_level,
              nrep_final = nrep_final,
              power_curve = fit_1,
              target_power = target_power,
              power_tolerance = power_tolerance_in_final,
              n_estimated = n_x,
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
#' @param x The output of
#' [n_from_power()], the
#' `print` method of
#' an `n_from_power` object,
#' which is the output of
#' [n_from_power()].
#'
#' @param digits The number of digits
#' after the decimal when printing
#' the results.
#'
#' @param ... Optional arguments.
#' Not used for now.
#'
#' @description
#' The `print` method only print
#' basic information. Call
#' [summary.n_from_power()] and its
#' `print` method for detailed output.
#'
#' @return
#' The `print`-method of `n_from_power`
#' objects returns the object `x`
#' invisibly.
#' It is called for its side effect.
#'
#' @export
print.n_from_power <- function(x,
                               digits = 3,
                               ...) {

  my_call <- x$call
  cat("Call:\n")
  print(my_call)
  solution_found <- !is.na(x$n_final)

  cat("- Target Power:",
      formatC(x$target_power, digits = digits, format = "f"),
      "\n")
  if (solution_found) {
    cat("- Final Sample Size:", x$n_final, "\n")
    cat("- Final Estimated Power:",
        formatC(x$power_final, digits = digits, format = "f"),
        "\n")
  } else {
    cat("- Solution not found.\n")
  }
  cat("Call `summary()` for detailed results.\n")
  invisible(x)
}


#'
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
