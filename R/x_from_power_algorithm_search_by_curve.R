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
                                            ci_hit,
                                            nrep_seq,
                                            final_nrep_seq,
                                            R_seq,
                                            solution_found) {
    i2 <- NULL
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
                            digits = switch(x, n = 0, es = 4),
                            format = "f")
        cat("- Value(s) to try:",
            paste0(x_j_str, collapse = ", "),
            "\n")
        cat("- Numbers of replications:",
            paste0(nrep_j, collapse = ", "),
            "\n")
      }

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
        cat("- Rejection Rates:\n")
        tmp <- rejection_rates(by_x_1)
        print(tmp)
        cat("\n")
      }
      fit_i <- power_curve(by_x_1,
                          formula = power_model,
                          start = start,
                          lower_bound = lower_bound,
                          upper_bound = upper_bound,
                          nls_control = nls_control,
                          nls_args = nls_args,
                          verbose = progress)

      # Get the rejection rates of all values tried.
      tmp1 <- rejection_rates(by_x_1,
                              all_columns = TRUE)
      # tmp1$reject <- tmp1$sig
      tmp2 <- range(tmp1$reject)

      # Is the desired value likely already in the range
      # of values examined, based on the estimated power?
      target_in_range <- (target_power > tmp2[1]) &&
                        (target_power < tmp2[2])

      if (target_in_range) {
        # The desired value probably within the range examined

        tmp3 <- abs(tmp1$reject - target_power)
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
                            verbose = progress)

      }

      if (progress) {
        cat("- Power Curve:\n")
        print(fit_i)
        cat("\n")
      }

      # Check results accumulated so far

      by_x_ci <- rejection_rates_add_ci(by_x_1,
                                        level = ci_level)
      i0 <- (by_x_ci$reject_ci_lo < target_power) &
            (by_x_ci$reject_ci_hi > target_power)

      # Is there at least one CI hitting the target power?
      if (any(i0)) {
        # At least one CI hits the target power

        ci_hit <- TRUE

        i2 <- find_ci_hit(by_x_1,
                          ci_level = ci_level,
                          target_power = target_power,
                          final_nrep = final_nrep)
        if (!is.na(i2) && !is.null(i2)) {
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
        # No CI hits the target power

        ci_hit <- FALSE
      }
      if (ci_hit) {
        # Is the nrep of the candidate already equal to
        # target nrep for the final solution?

        if (solution_found) {
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
            xs_per_trial_seq <- xs_per_trial_seq[-1]
          }
        }
      } else {
        # No value has CI hitting the target power.

        if (progress) {
          cat("- Estimated power is not close enough to target power (",
              formatC(target_power, digits = 4, format = "f"), "). ",
              "(CI: [", paste0(formatC(ci_out, digits = 4, format = "f"), collapse = ","), "])",
              "\n",
              sep = "")
        }
      }

  }
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