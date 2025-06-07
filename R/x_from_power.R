#' @title Sample Size and Effect Size Determination
#'
#' @description It searches by simulation
#' the sample size (given other factors,
#' such as effect sizes) or effect size
#' (given other factors, such as sample
#' size) with power to
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
#'   starting sample size or
#'   effect size.
#'
#' - Call [x_from_power()] on the output
#'   of [power4test()] returned from
#'   the previous step. This
#'   function will iteratively repeat
#'   the analysis on either other sample
#'   sizes, or other values for a
#'   selected model parameter (the
#'   effect sizes),
#'   trying to find a value (a sample
#'   size, or a population value of
#'   the selected model parameter) with
#'   a power level close enough to the
#'   target power.
#'
#' If several values of `x` (sample
#' size or the population value of
#' a model parameter) have already been
#' examined by [power4test_by_n()] or
#' [power4test_by_es()], the output
#' of these two functions can also be
#' used as `object` by [x_from_power()].
#'
#' Usually, the default values of the
#' arguments should be sufficient.
#'
#' The results can be viewed using
#' [summary()], and the output has
#' a `plot` method ([plot.x_from_power()]) to
#' plot the relation between power and
#' values (of `x`) examined.
#'
#' A detailed illustration on how to
#' use this function can be found
#' from these pages:
#'
#' - For example sizes: <https://sfcheung.github.io/power4mome/articles/x_from_power_for_n.html>
#'
#' - For effect sizes: <https://sfcheung.github.io/power4mome/articles/x_from_power_for_es.html>
#'
#' # Algorithms
#'
#' Two algorithms are currently available,
#' the simple (though inefficient)
#' bisection method, and a method that
#' makes use of the estimated crude power
#' curve.
#'
#' Unlike typical root-finding problems,
#' the prediction of the level of power
#' is stochastic. Moreover, the computational
#' cost is high when Monte Carlo or
#' bootstrap confidence intervals are
#' used to do a test because the estimation
#' of the power for one single value of
#' `x` can sometimes take one minute or
#' longer. Therefore, in addition to
#' the simple bisection method, a method,
#' named *power curve* method, was also
#' specifically developed for this
#' scenario.
#'
#' ## Bisection Method
#'
#' This method, `algorithm = "bisection"`,
#' basically start with
#' an interval that probably enclose the
#' value of `x` with the target power,
#' and then successively narrow this
#' interval. The mid-point of this
#' interval is used as the estimate.
#' Though simple, there are cases in
#' which it can be slow. Nevertheless,
#' preliminary examination suggests that
#' this method is good enough for common
#' scenarios. Therefore, this method is
#' the default algorithm ().
#'
#' ## Power Curve Method
#'
#' This method, `algorithm = "power_curve"`,
#' starts with a crude
#' power curve based on a few points.
#' This tentative model is then used
#' to suggest the values to examine in
#' the next iteration. The form, not
#' just the parameters, of the
#' model can change across iterations,
#' as more and more data points are
#' available.
#'
#' The technical internal workflow of
#' this method implemented in
#' [x_from_power()] can be found in
#' this page: <https://sfcheung.github.io/power4mome/articles/x_from_power_workflow.html>.
#'
#' @return
#' The function [x_from_power()]
#' returns an `x_from_power` object,
#' which is a list with the following
#' elements:
#'
#' - `power4test_trials`: The output of
#' [power4test_by_n()] for all sample
#' sizes examined, or of
#' [power4test_by_es()] for all
#' population values of the selected
#' parameter examined.
#'
#' - `rejection_rates`: The output of
#' [rejection_rates()].
#'
#' - `x_tried`: The sample sizes or
#' population values
#' examined.
#'
#' - `power_tried`: The estimated
#' rejection rates for all the values
#' examined.
#'
#' - `x_final`: The sample size or
#' population value in the
#' solution. `NA` if a solution not found.
#'
#' - `power_final`: The estimated power
#' of the value in the solution.
#' `NA` if a solution not found.
#'
#' - `i_final`: The position of the
#' solution in `power4test_trials`.
#' `NA` if a solution not found.
#'
#' - `ci_final`: The confidence interval
#' of the estimated power in the solution,
#' formed by normal approximation.
#' `NA` if a solution not found.
#'
#' - `ci_level`: The level of confidence
#' of `ci_final`.
#'
#' - `nrep_final`: The number of
#' replications (`nrep`) when estimating
#' the power in the solution.
#'
#' - `power_curve`: The output of
#' [power_curve()] when estimating the
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
#' - `x_estimated`: The value
#' (sample size or population value)
#' with the target power, estimated by
#' `power_curve`. This is used, when
#' solution not found, to determine the
#' range of the values to search when
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
#' of [x_from_power()] used in the search.
#'
#' - `call`: The call when this function
#' is called.
#'
#' @param object A `power4test` object,
#' which is the output of [power4test()].
#' Can also be a `power4test_by_n` object,
#' the output
#' of [power4test_by_n()], or
#' a `power4test_by_es` object, the
#' output of
#' [power4test_by_es()]. For these
#' two types of objects, the attempt
#' with power closest to the
#' `target_power` will be used as
#' `object`, and all other attempts in
#' them will be included in the estimation
#' of subsequent attempts and the final
#' output. Last, it can also be the
#' output of a previous call to
#' [x_from_power()], and the stored
#' trials will be retrieved.
#'
#' @param x For [x_from_power()],
#' `x` set the value to
#' be searched. Can be `"n"`, the sample
#' size, or `"es"`, the population value
#' of a parameter (set by `pop_es_name`).
#' For the `print` method of `x_from_power`
#' objects, this is the output of
#' [x_from_power()].
#'
#' @param pop_es_name The name of the
#' parameter. Required if `x` is `"es"`.
#' See the help page
#' of [ptable_pop()] on the names for
#' the argument `pop_es`.
#'
#' @param target_power The target power,
#' a value greater than 0 and less than
#' one.
#'
#' @param xs_per_trial The initial number
#' of values (sample sizes or population
#' values) to consider in each
#' trial. Should be an integer at least
#' 1. Rounded
#' up if not an integer.
#'
#' @param ci_level The level of confidence
#' of the confidence intervals computed
#' for the estimated power. Default is
#' .95, denoting 95%.
#'
#' @param power_min,power_max The minimum
#' and maximum values, respectively,
#' of power
#' when determining the values to
#' try in each trail. Default is .01.
#'
#' @param x_interval A vector of
#' two values, the minimum value
#' and the maximum values of `x`, in
#' the search for the values
#' (sample sizes or population values).
#'
#' @param extendInt Whether `x_interval`
#' can be expanded when estimating the
#' the values to try. The value will
#' be passed to the argument of the
#' same name in [stats::uniroot()].
#' If `x` is `"n"`, then the default
#' value is `"upX"`.
#' That is, a value higher than
#' the maximum in `x_interval` is
#' allowed, if predicted by the tentative
#' model. Otherwise, the default value
#' is `"no"`. See the help page of
#' [stats::uniroot()] for further
#' information.
#'
#' @param progress Logical. Whether
#' the searching progress is reported.
#'
#' @param simulation_progress Logical.
#' Whether the progress in each call
#' to [power4test()], [power4test_by_n()],
#' or [power4test_by_es()]
#' is shown. To be passed to
#' the `progress` argument of these
#' functions.
#'
#' @param max_trials The maximum number
#' of trials in searching the value
#' with the target power. Rounded
#' up if not an integer.
#'
#' @param initial_nrep The initial
#' number of replications. If set
#' to `NULL`, the `nrep` used in
#' `object` will be used. If higher
#' than `final_nrep`, it will be
#' converted to one-fourth of `final_nrep`.
#' If lower than the `nrep` in `object`
#' after the conversion,
#' then set to `nrep` in the `object`.
#'
#' @param final_nrep The number of
#' replications in the final stage,
#' also the maximum number of replications
#' in each call to [power4test()],
#' [power4test_by_n()], or
#' [power4test_by_es()].
#'
#' @param initial_R The initial number of
#' Monte Carlo simulation or
#' bootstrapping samples. The `R` in calling
#' [power4test()], [power4test_by_n()],
#' or [power4test_by_es()]. If set to `NULL`,
#' the `R` used in
#' `object` will be used.
#' If higher
#' than `final_R`, it will be
#' converted to one-fourth of `final_R`.
#' If lower than the `R` in `object`
#' after the conversion,
#' then set to `R` in `object``.
#'
#' @param final_R The number of
#' Monte Carlo simulation or
#' bootstrapping samples in the final
#' stage. The `R` in calling
#' [power4test()], [power4test_by_n()],
#' or [power4test_by_es()]
#' will be stepped up to this value
#' when approaching the target
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
#' run, but will try more values.
#' Rounded up if not an integer.
#'
#' @param seed If not `NULL`, [set.seed()]
#' will be used to make the process
#' reproducible. This is not always
#' possible if many stages of
#' parallel processing is involved.
#'
#' @param x_include_interval Logical.
#' Whether the minimum and maximum
#' values in `x_interval` are mandatory
#' to be included in the values
#' to be searched.
#'
#' @param power_curve_args A named
#' list of arguments to be passed
#' [power_curve()] when estimating
#' the relation between power and `x`
#' (sample size or effect size). Please
#' refer to [power_curve()] on available
#' arguments. There is one except:
#' `power_model` is mapped to
#' the `formula` argument of
#' [power_curve()].
#'
# #'
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
# #'
#' @param save_sim_all If `FALSE`,
#' the default, the data in each
#' `power4test` object for each
#' value of `x` is not saved,
#' to reduce the size of the output.
#' If set to `TRUE`, the size of the
#' output can be very large in size.
#'
#' @param algorithm The algorithm for
#' finding `x`. Can be `"power_curve"`
#' or `"bisection"`.
#'
#' @param control A named list of
#' additional
#' arguments to be passed to the
#' algorithm to be used. For advanced
#' users.
#'
#' @seealso [power4test()], [power4test_by_n()],
#' and [power4test_by_es()].
#'
#' @examples
#'
#' # Specify the population model
#'
#' mod <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' # Specify the population values
#'
#' mod_es <-
#' "
#' m ~ x: m
#' y ~ m: l
#' y ~ x: n
#' "
#'
#' # Generate the datasets
#'
#' sim_only <- power4test(nrep = 5,
#'                        model = mod,
#'                        pop_es = mod_es,
#'                        n = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 2345)
#'
#' # Do a test
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_parameters,
#'                        test_args = list(pars = "m~x"))
#'
#' # Determine the sample size with a power of .80 (default)
#'
#' # In real analysis, to have more stable results:
#' # - Use a larger final_nrep (e.g., 400).
#' # - Use the default xs_per_trial of 3, or just remove it.
#'
#' # If the default values are OK, this call is sufficient:
#' # power_vs_n <- x_from_power(test_out,
#' #                            x = "n",
#' #                            seed = 4567)
#' power_vs_n <- x_from_power(test_out,
#'                            x = "n",
#'                            progress = TRUE,
#'                            target_power = .80,
#'                            final_nrep = 5,
#'                            xs_per_trial = 1,
#'                            nrep_steps = 1,
#'                            max_trials = 1,
#'                            seed = 1234)
#' summary(power_vs_n)
#' plot(power_vs_n)
#'
#'
#' @importFrom graphics abline arrows par points text title
#'
#' @export
x_from_power <- function(object,
                         x,
                         pop_es_name = NULL,
                         target_power = .80,
                         xs_per_trial = 3,
                         ci_level = .95,
                         power_min = .01,
                         power_max = .90,
                         x_interval = switch(x,
                                             n = c(50, 2000),
                                             es = c(0, .95)),
                         extendInt = NULL,
                         progress = TRUE,
                         simulation_progress = TRUE,
                         max_trials = 10,
                         initial_nrep = 100,
                         final_nrep = 400,
                         initial_R = 250,
                         final_R = 1000,
                         nrep_steps = 1,
                         seed = NULL,
                         x_include_interval = FALSE,
                         power_curve_args = list(power_model = NULL,
                                                 start = NULL,
                                                 lower_bound = NULL,
                                                 upper_bound = NULL,
                                                 nls_control = list(),
                                                 nls_args = list()),
                         save_sim_all = FALSE,
                         algorithm = c("bisection", "power_curve"),
                         control = list()
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

  algorithm <- match.arg(algorithm)

  x <- match.arg(x,
                 choices = c("n", "es"))

  a <- abs(stats::qnorm((1 - ci_level) / 2))
  power_tolerance_in_interval <- a * sqrt(target_power * (1 - target_power) / final_nrep)
  power_tolerance_in_final <- a * sqrt(target_power * (1 - target_power) / final_nrep)

  # === Sanity Checks ===

  if (target_power <= 0 || target_power >= 1) {
    stop("'target power' (",
         target_power,
         ") not between 0 and 1.")
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

  if (isTRUE(power_max < power_min)) {
    stop("'power_max' must be greater than 'power_min'.")
  }

  if (power_max < target_power || power_min > target_power) {
    stop("'target_power' must be between 'power_min' and 'power_max'.")
  }

  if (x == "n") {
    if (min(x_interval) < 0) {
      stop("The minimum value of 'n_interval' cannot be negative")
    }
  }

  if (length(x_interval) != 2) {
    stop("'x_interval' must be a vector with exactly two values.")
  }
  if (x_interval[2] < x_interval[1]) {
    stop("'x_interval' must be of the form c(minimum, maximum).")
  }

  if (is.null(extendInt)) {
    if (x == "n") {
      extendInt <- match.arg(extendInt,
                             choices = c("upX", "no", "yes", "downX"))
    }
    if (x == "es") {
      extendInt <- match.arg(extendInt,
                             choices = c("yes", "no", "upX", "downX"))
    }
  }

  x_max <- max(x_interval)
  x_min <- min(x_interval)

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

  # Handle the object

  if (inherits(object, "x_from_power")) {
    # Throw an error if imcompatible
    check_x_from_power_as_input(object,
                                x = x,
                                pop_es_name = pop_es_name)
    object <- object$power4test_trials
  }

  is_by_x <- FALSE
  if (inherits(object, "power4test_by_n") ||
      inherits(object, "power4test_by_es")) {
    is_by_x <- TRUE
    object_by_org <- object
    # TODO:
    # - Should quit if solution is already in object
    i_org <- find_ci_hit(object_by_org,
                         ci_level = ci_level,
                         target_power = target_power,
                         final_nrep = 1,
                         closest_ok = TRUE)
    object <- object_by_org[[i_org]]
  } else {
    object_by_org <- NA
  }

  time_start <- Sys.time()

  # Change nrep?

  nrep_org <- attr(object, "args")$nrep

  if (is.null(initial_nrep)) {
    nrep0 <- nrep_org
  } else {
    # Fix initial_nrep greater than final_nrep
    if (initial_nrep > final_nrep) {
      initial_nrep <- ceiling(final_nrep / 4)
      if ((initial_nrep < 100) && (nrep_org <= final_nrep)) {
        initial_nrep <- nrep_org
      }
    }
    nrep0 <- ceiling(initial_nrep)
  }

  R_org <- attr(object, "args")$R

  if (is.null(initial_R)) {
    R0 <- R_org
  } else {
    if (initial_R > final_R) {
      initial_R <- ceiling(final_R / 4)
      if ((initial_R < 100) && (R_org <= final_R)) {
        initial_R <- R_org
      }
    }
    R0 <- ceiling(initial_R)
  }

  # === Initial Trial ===

  # Set the initial values to try

  if (algorithm == "power_curve") {

    if (progress) {
      cat("\n--- Pre-iteration Search ---\n\n")
      tmp <- format(Sys.time(), "%Y-%m-%d %X")
      cat("- Start at", tmp, "\n")
    }

    a_out <- do.call(power_algorithm_search_by_curve_pre_i,
                     c(list(object = object,
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
                            power_model = power_curve_args$power_model,
                            start = power_curve_args$start,
                            lower_bound = power_curve_args$lower_bound,
                            upper_bound = power_curve_args$upper_bound,
                            nls_control = power_curve_args$nls_control,
                            nls_args = power_curve_args$nls_args,
                            final_nrep = final_nrep,
                            nrep_steps = nrep_steps,
                            final_R = final_R),
                      control))

    by_x_1 <- a_out$by_x_1
    fit_1 <- a_out$fit_1
    nrep_seq <- a_out$nrep_seq
    final_nrep_seq <- a_out$final_nrep_seq
    R_seq <- a_out$R_seq
    xs_per_trial_seq <- a_out$xs_per_trial_seq

    rm(a_out)

  }

  if (algorithm == "bisection") {

    a_out <- do.call(power_algorithm_bisection_pre_i,
                     c(list(object = object,
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
                            power_model = power_curve_args$power_model,
                            start = power_curve_args$start,
                            lower_bound = power_curve_args$ower_bound,
                            upper_bound = power_curve_args$upper_bound,
                            nls_control = power_curve_args$nls_control,
                            nls_args = power_curve_args$nls_args,
                            final_nrep = final_nrep,
                            nrep_steps = nrep_steps,
                            final_R = final_R),
                       control))

    x_interval_updated <- a_out$x_interval_updated
    by_x_1 <- a_out$by_x_1
    fit_1 <- a_out$fit_1

    # # Not used by bisection for now
    # nrep_seq <- a_out$nrep_seq
    # final_nrep_seq <- a_out$final_nrep_seq
    # R_seq <- a_out$R_seq
    # xs_per_trial_seq <- a_out$xs_per_trial_seq

    rm(a_out)

  }

  # ** by_x_1 **
  # The collection of all values tried and their results
  # to be updated when new value is tried.
  # Used after the end of the loop.

  # ** fit_1 **
  # The latest power curve
  # To be updated whenever by_x_1 is updated.
  # Used after the end of the loop.

  ci_hit <- FALSE
  solution_found <- FALSE

  if (algorithm == "power_curve") {

    # === Loop Over The Trials ===

    a_out <- do.call(power_algorithm_search_by_curve,
                     c(list(object = object,
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
                            power_model = power_curve_args$power_model,
                            start = power_curve_args$start,
                            lower_bound = power_curve_args$lower_bound,
                            upper_bound = power_curve_args$upper_bound,
                            nls_control = power_curve_args$nls_control,
                            nls_args = power_curve_args$nls_args,
                            save_sim_all = save_sim_all,
                            power_tolerance_in_interval = power_tolerance_in_interval,
                            power_tolerance_in_final = power_tolerance_in_final,
                            by_x_1 = by_x_1,
                            fit_1 = fit_1,
                            ci_hit = ci_hit,
                            nrep_seq = nrep_seq,
                            final_nrep_seq = final_nrep_seq,
                            R_seq = R_seq,
                            solution_found = solution_found),
                     control))

    by_x_1 <- a_out$by_x_1
    fit_1 <- a_out$fit_1
    ci_hit <- a_out$ci_hit
    x_tried <- a_out$x_tried
    x_out <- a_out$x_out
    power_out <- a_out$power_out
    nrep_out <- a_out$nrep_out
    ci_out <- a_out$ci_out
    by_x_out <- a_out$by_x_out
    i2 <- a_out$i2
    solution_found <- a_out$solution_found

    rm(a_out)

  }

  if (algorithm == "bisection") {

    # === Loop Over The Trials ===


    # TODO:
    # - Add sth like args = list(...) for additional
    #   arguments and let users change algorithm-specific
    #   arguments.
    lower_hard <- min(x_interval)
    upper_hard <- max(x_interval)
    extend_maxiter <- 3
    tol <- .02

    a_out <- do.call(power_algorithm_bisection,
                     c(list(object = object,
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
                            R = R_org,
                            power_model = power_curve_args$power_model,
                            power_curve_start = power_curve_args$start,
                            lower_bound = power_curve_args$lower_bound,
                            upper_bound = power_curve_args$upper_bound,
                            nls_control = power_curve_args$nls_control,
                            nls_args = power_curve_args$nls_args,
                            save_sim_all = save_sim_all,
                            by_x_1 = by_x_1,
                            fit_1 = fit_1,
                            ci_hit = ci_hit,
                            is_by_x = is_by_x,
                            solution_found = solution_found,
                            digits = 3,
                            lower_hard = lower_hard,
                            upper_hard = upper_hard,
                            extend_maxiter = extend_maxiter,
                            what = "point",
                            goal = "ci_hit",
                            tol = tol),
                      control))

    by_x_1 <- a_out$by_x_1
    fit_1 <- a_out$fit_1
    ci_hit <- a_out$ci_hit
    x_tried <- a_out$x_tried
    x_out <- a_out$x_out
    power_out <- a_out$power_out
    nrep_out <- a_out$nrep_out
    ci_out <- a_out$ci_out
    by_x_out <- a_out$by_x_out
    i2 <- a_out$i2
    solution_found <- a_out$solution_found

    rm(a_out)

  }

  if (progress) {
    cat("\n\n--- Final Stage ---\n\n")
    tmp <- format(Sys.time(), "%Y-%m-%d %X")
    cat("- Start at", tmp, "\n")

    cat("- Rejection Rates:\n")
    tmp <- rejection_rates(by_x_1)
    print(tmp)
    cat("\n")
    cat("- Estimated Power Curve:\n")
    print(fit_1)
    cat("\n")
  }

  # Is solution found?
  # - At least one CI hits the target power
  # - The maximum number of replications reached.

  if (ci_hit && (nrep_out == final_nrep)) {
    # Created when ci_hit set to TRUE

    # ** x_final, by_x_final, power_final, ci_final, nrep_final, i_final **
    # The solution.
    x_final <- x_out
    by_x_final <- by_x_out
    power_final <- power_out
    ci_final <- ci_out
    nrep_final <- nrep_out
    i_final <- i2
  } else {
    # No solution found.
    # Force ci_hit to be FALSE.
    # Set the NAs to denote this.
    x_final <- NA
    by_x_final <- NA
    power_final <- NA
    ci_final <- NA
    nrep_final <- NA
    i_final <- NA
  }

  # ** x_x **
  # The estimated value based on power_curve.
  # Used as a suggestion when no solution was found.
  x_x <- NA
  if (ci_hit) {
    x_tried <- switch(x,
                      n = as.numeric(names(by_x_1)),
                      es = sapply(by_x_1,
                                  \(x) {attr(x, "pop_es_value")},
                                  USE.NAMES = FALSE))
    x_x <- estimate_x_range(power_x_fit = fit_1,
                            x = x,
                            target_power = target_power,
                            k = 1,
                            tolerance = 0,
                            power_min = power_min,
                            power_max = power_max,
                            interval = x_interval,
                            extendInt = "yes",
                            x_to_exclude = x_tried)
  }
  if (progress) {
    if (ci_hit && (nrep_out == final_nrep)) {
      cat("\n")
      x_out_str <- formatC(x_out,
                           digits = switch(x, n = 0, es = 4),
                           format = "f")
      cat("- Final Value:", x_out_str, "\n")
      cat("- Final Estimated Power:",
          formatC(power_final, digits = 4, format = "f"), "\n")
      cat("- Confidence Interval: [",
          paste0(formatC(ci_final, digits = 4, format = "f"), collapse = "; "),
          "]\n", sep = "")
      cat("- CI Level: ",
          formatC(ci_level*100, digits = 2, format = "f"), "%", "\n", sep = "")
    } else {
      cat("\n")
      cat("- None of the value(s) examined",
          "in the interval meet the target power.\n")
      if (!is.na(x_x)) {
        x_x_str <- formatC(x_x,
                           digits = switch(x, n = 0, es = 4),
                           format = "f")
        cat("- The estimated required value is ",
            x_x_str,
            ".\n", sep = "")
      }
      cat("- Try expanding the range of values",
          "by setting 'x_interval'.\n")
    }
  }

  # === Finalize the Output ===

  my_call <- as.list(match.call())[-1]
  args <- formals()
  args <- utils::modifyList(args,
                            my_call)
  args$object <- NULL
  reject_1 <- rejection_rates(by_x_1)
  time_end <- Sys.time()

  out <- list(x = x,
              pop_es_name = pop_es_name,
              power4test_trials = by_x_1,
              rejection_rates = reject_1,
              x_tried = switch(x,
                               n = reject_1$n,
                               es = reject_1$es),
              power_tried = reject_1$reject,
              x_final = x_final,
              power_final = power_final,
              i_final = i_final,
              ci_final = ci_final,
              ci_level = ci_level,
              nrep_final = nrep_final,
              power_curve = fit_1,
              target_power = target_power,
              power_tolerance = power_tolerance_in_final,
              x_estimated = x_x,
              start = time_start,
              end = time_end,
              time_spent = difftime(time_end, time_start),
              args = args,
              call = match.call())
  class(out) <- c("x_from_power", class(out))
  return(out)
}

#' @rdname x_from_power
#'
#'
#' @param digits The number of digits
#' after the decimal when printing
#' the results.
#'
#' @param ... Optional arguments.
#' Not used for now.
#'
#' @details
#' The `print` method only print
#' basic information. Call the
#' `summary` method of `x_from_power` objects
#' ([summary.x_from_power()]) and its
#' `print` method for detailed results
#'
#' @return
#' The `print`-method of `x_from_power`
#' objects returns the object `x`
#' invisibly.
#' It is called for its side effect.
#'
#' @export
print.x_from_power <- function(x,
                               digits = 3,
                               ...) {

  my_call <- x$call
  cat("Call:\n")
  print(my_call)
  solution_found <- !is.na(x$x_final)
  predictor <- x$x
  cat("Predictor (x):",
      switch(predictor,
             n = "Sample Size",
             es = "Effect Size"),
      "\n")
  if (predictor == "es") {
    cat("Parameter Name (pop_es_name):",
        x$pop_es_name,
        "\n")
  }

  cat("- Target Power:",
      formatC(x$target_power, digits = digits, format = "f"),
      "\n")
  if (solution_found) {
    x_final_str <- formatC(x$x_final,
                           digits = switch(predictor,
                                           n = 0,
                                           es = digits),
                           format = "f")
    cat("- Final Value:", x_final_str, "\n")
    cat("- Final Estimated Power:",
        formatC(x$power_final, digits = digits, format = "f"),
        "\n")
  } else {
    cat("- Solution not found.\n")
  }
  cat("Call `summary()` for detailed results.\n")
  invisible(x)
}
