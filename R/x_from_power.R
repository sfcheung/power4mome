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
#' This is how to use [x_from_power()]:
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
#'   trying to achieve a goal (`goal`) for
#'   a value of interest (`what`).
#'
#' If the `goal` is `"ci_hit"`, the
#' search will try to find a value (a sample
#' size, or a population value of
#' the selected model parameter) with
#' a power level close enough to the
#' target power, defined by having its
#' confidence interval for the power
#' including the target power.
#'
#' If the `goal` is `"close_enough"`,
#' then the search will try to find a
#' value of `x` with its level of
#' power (`"point"`), the upper bound
#' of the confidence interval for this
#' level of power (`"ub"`), or the
#' lower bound of the confidence interval
#' fro this level of power (`"lb"`)
#' "close enough" to the target level of
#' power, defined by having an absolute
#' difference less than the `tolerance`.
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
#' use this function for sample size can be found
#' from this page:
#'
#' <https://sfcheung.github.io/power4mome/articles/x_from_power_for_n.html>
#'
#' # Algorithms
#'
#' Two algorithms are currently available,
#' the simple (though sometimes inefficient)
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
#' basically starts with
#' an interval that probably encloses the
#' value of `x` that meets the goal,
#' and then successively narrows this
#' interval. The mid-point of this
#' interval is used as the estimate.
#' Though simple, there are cases in
#' which it can be slow. Nevertheless,
#' preliminary examination suggests that
#' this method is good enough for common
#' scenarios. Therefore, this method is
#' the default algorithm when `x` is
#' `n`.
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
#' This method is the default method
#' for `x = "es"` with `goal = "ci_hit"`
#' because the relation
#' between the power and the population
#' value of a parameter varies across
#' parameters, unlike the relation
#' between power and sample size. Therefore,
#' taking into account the working
#' power curve may help finding the
#' desired value of `x`.
#'
#' Before version 0.1.1.33, this
#' method can be used only with
#' the goal `"ci_hit"`. Since
#' version 0.1.1.34, it supports all
#' goals, like the bisection method.
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
#' of the estimated power in the solution.
#' The method is determined
#' by the option `power4mome.ci_method`.
#' If `NULL` or `"wilson"`, Wilson's
#' (1927) method is used. If
#' `"norm"`, normal approximation
#' is used.
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
#' @param what The value for which is
#' searched: the estimate power (`"point"`),
#' the upper bound of the confidence
#' interval (`"ub"`), or the lower bound
#' of the confidence interval (`"lb"`).
#'
#' @param goal The goal of the search.
#' If `"ci_hit"`, then the goal is to
#' find a value of `x` with the
#' confidence interval of the estimated
#' power including the target power.
#' If `"close_enough"`, then the goal
#' is to find a value of `x` with the
#' value in `what` "close enough" to
#' the target power, defined by having
#' an absolute difference with the
#' target power less than `tolerance`.
#'
#' @param tolerance Used when the goal
#' is `"close_enough"`.
#' If `NULL`, set automatically based
#' on the algorithm used.
#'
#' @param ci_level The level of confidence
#' of the confidence intervals computed
#' for the estimated power. Default is
#' .95, denoting 95%.
#'
#' @param x_interval A vector of
#' two values, the minimum value
#' and the maximum values of `x`, in
#' the search for the values
#' (sample sizes or population values).
#' If `NULL`, default when `x = "es"`,
#' it will be determined internally.
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
#' If `NULL`, set automatically based
#' on the algorithm used.
#'
#' @param max_trials The maximum number
#' of trials in searching the value
#' with the target power. Rounded
#' up if not an integer.
#' If `NULL`, set automatically based
#' on the algorithm used.
#'
#' @param final_nrep The number of
#' replications in the final stage,
#' also the maximum number of replications
#' in each call to [power4test()],
#' [power4test_by_n()], or
#' [power4test_by_es()].
#' If `object`
#' is an output of [power4test()]
#' or [x_from_power()] and
#' this argument is not set, `final_nrep`
#' will be set to `nrep` or `final_nrep`
#' stored in
#' `object`.
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
#' If `object`
#' is an output of [power4test()]
#' or [x_from_power()] and
#' this argument is not set, `final_R`
#' will be set to `R` or `final_R`
#' stored in
#' `object`.
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
#' @param save_sim_all If `FALSE`,
#' the default, the data in each
#' `power4test` object for each
#' value of `x` is not saved,
#' to reduce the size of the output.
#' If set to `TRUE`, the size of the
#' output can be very large in size.
#'
#' @param algorithm The algorithm for
#' finding `x`. Can be `"power_curve"`,
#' `"bisection"`, or
#' `"probabilistic_bisection"`. The default algorithm
#' depends on `x`.
#'
#' @param control A named list of
#' additional
#' arguments to be passed to the
#' algorithm to be used. For advanced
#' users.
#'
#' @param check_es_interval If `TRUE`,
#' the default, and `x` is `"es"`,
#' a conservative probable
#' range of valid values for the selected
#' parameter will be determined, and it
#' will be used instead of `x_interval`.
#' If the range spans both positive and
#' negative values, only the interval
#' of the same sign as the population
#' value in `object` will be used.
#'
#' @param internal_args A named list
#' of internal arguments. For internal
#' testing. Do not use it.
#'
#' @param rejection_rates_args
#' Argument values to be used when
#' [rejection_rates()] is called, used
#' to decide how rejection rates will
#' be estimated. Only one single test
#' is supported by [x_from_power()].
#' Therefore, `merge_all_tests` is
#' always `TRUE` and cannot be changed.
#' The argument `collapse` can be
#' `"all_sig"`, `"at_least_one_sig"`,
#' or `"at_least_k_sig"` (it cannot be
#' `"none"`). Please refer to
#' [rejection_rates()] for other
#' possible arguments. These values,
#' if set, will overwrite any stored
#' settings in `object`.
#'
#' @references
#' Wilson, E. B. (1927). Probable inference, the law of
#' succession, and statistical inference.
#' *Journal of the American Statistical Association, 22*(158),
#' 209-212. \doi{10.1080/01621459.1927.10502953}
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
                         x = arg_x_from_power(object, "x", arg_in = "call") %||% "n",
                         pop_es_name = arg_x_from_power(object, "pop_es_name", arg_in = "call"),
                         target_power = .80,
                         what = arg_x_from_power(object, "what") %||% "point",
                         goal = arg_x_from_power(object, "goal") %||% {switch(what,
                                       point = "ci_hit",
                                       ub = "close_enough",
                                       lb = "close_enough")},
                         ci_level = .95,
                         tolerance = NULL,
                         x_interval = switch(x,
                                             n = c(50, 2000),
                                             es = NULL),
                         extendInt = NULL,
                         progress = TRUE,
                         simulation_progress = NULL,
                         max_trials = NULL,
                         final_nrep = attr(object, "args")$nrep %||% (object$nrep_final %||% 400),
                         final_R = attr(object, "args")$R %||% (object$args$final_R %||% 1000),
                         seed = NULL,
                         x_include_interval = FALSE,
                         check_es_interval = TRUE,
                         power_curve_args = list(power_model = NULL,
                                                 start = NULL,
                                                 lower_bound = NULL,
                                                 upper_bound = NULL,
                                                 nls_control = list(),
                                                 nls_args = list()),
                         save_sim_all = FALSE,
                         algorithm = NULL,
                         control = list(),
                         internal_args = list(),
                         rejection_rates_args = list(collapse = "all_sig",
                                                     at_least_k = 1,
                                                     p_adjust_method = "none",
                                                     alpha = .05)
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

  what <- match.arg(what,
                    c("point", "ub", "lb"))
  goal <- match.arg(goal,
                    c("ci_hit", "close_enough"))

  internal_args0 <- list(keep_algorithm = TRUE)

  internal_args <- utils::modifyList(
                      internal_args0,
                      internal_args
                    )

  # ==== Update the object if x_from_power ====

  if (inherits(object, "x_from_power")) {
    # Make object to be match some arguments
    if ((object$what != what) ||
        (object$goal != goal)) {
      # what or goal changed.
      object$solution_found <- FALSE
    }
    object$what <- what
    object$goal <- goal
    # No need to update them.
    # They will be updated using
    # what and goal in this call
    # object$call$what <- what
    # object$call$goal <- goal
  }

  # ==== Fix some arguments ====

  changed_to_point <- FALSE
  if ((goal == "ci_hit") &&
      (what != "point")) {
    changed_to_point <- TRUE
    what <- "point"
  }

  x <- match.arg(x,
                 choices = c("n", "es"))

  if (!is.null(algorithm)) {
    algorithm <- match.arg(algorithm,
                          c("bisection",
                            "power_curve",
                            "probabilistic_bisection"))
  }

  # merge_all_tests is always TRUE
  # Ignored by rejection_rates() if there is only one test
  tmp <- list(collapse = "all_sig",
              at_least_k = 1,
              p_adjust_method = "none",
              alpha = .05)
  rejection_rates_args <- utils::modifyList(
                            tmp,
                            rejection_rates_args
                          )
  rejection_rates_args$merge_all_tests <- TRUE

  # ==== Set default algorithm ====

  # what and goal take precedence

  if (goal == "ci_hit") {
    if (is.null(algorithm)) {
      algorithm <- switch(x,
                          n = "bisection",
                          es = "power_curve")
    }
  }

  changed_to_bisection <- FALSE
  if (goal == "close_enough") {
    # internal_args$keep_algorithm is now ignored
    if (is.null(algorithm)) {
      algorithm <- match.arg(algorithm,
                             c("bisection",
                               "probabilistic_bisection",
                               "power_curve"))
    }
  }

  if (algorithm == "probabilistic_bisection") {
    # PBA does not support ci_hit separately.
    # Instead, close_enough is used to emulated ci_hit
    goal <- "close_enough"
  }


  # what: The value to be examined.
  # goal:
  # - ci_hit: Only relevant for what == "point"
  # - close_enough: Can be used for all what.

  a <- reject_ci_wilson(
          nreject = ceiling(target_power * final_nrep),
          nvalid = final_nrep,
          level  = ci_level)
  power_tolerance_in_interval <- max(abs(target_power - a))
  power_tolerance_in_final <- max(abs(target_power - a))

  # ==== Sanity checks ====

  if (target_power <= 0 || target_power >= 1) {
    stop("'target power' (",
         target_power,
         ") not between 0 and 1.")
  }

  if (x == "n") {
    if (min(x_interval) < 0) {
      stop("The minimum value of 'n_interval' cannot be negative")
    }
  }

  if ((length(x_interval) != 2) &&
      !is.null(x_interval)) {
    stop("'x_interval' must be a vector with exactly two values.")
  }
  if ((x_interval[2] < x_interval[1]) &&
      !is.null(x_interval)) {
    stop("'x_interval' must be of the form c(minimum, maximum).")
  }

  if (is.null(extendInt)) {
    if (x == "n") {
      extendInt <- match.arg(extendInt,
                             choices = c("yes", "no", "upX", "downX"))
    }
    if (x == "es") {
      extendInt <- match.arg(extendInt,
                             choices = c("no", "yes", "upX", "downX"))
    }
  }

  if (!is.null(max_trials)) {
    max_trials <- ceiling(max_trials)
    if (max_trials < 1) {
      stop("'max_trials' must be at least 1 (after rounding, if necessary).")
    }
  }

  # ==== Set tolerance if NULL ====

  if (is.null(tolerance)) {
    tolerance <- set_tolerance(
                    algorithm = algorithm,
                    target_power = target_power,
                    goal = goal,
                    what = what,
                    final_nrep = final_nrep,
                    ci_level = ci_level
                  )
  }

  # ==== Set max_trials if NULL ====

  if (is.null(max_trials)) {
    max_trials <- set_max_trials(
                    algorithm = algorithm
                  )
  }

  # ==== Set max_trials if NULL ====

  if (is.null(simulation_progress)) {
    simulation_progress <- switch(
      algorithm,
      probabilistic_bisection = FALSE,
      TRUE
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # ==== Process the object ====

  # Is it a compatible x_from_power object?
  # - If yes and solution not found,
  #   - extract the stored by_x object
  # - If yes and solution found,
  #   - The object is returned as is.

  is_x_from_power <- FALSE
  if (inherits(object, "x_from_power")) {

    # ==== x_from_power object ====

    # Throw an error if incompatible

    # TODO:
    # - Will run if goal and/or what changed

    check_x_from_power_as_input(object,
                                x = x,
                                pop_es_name = pop_es_name,
                                final_nrep = final_nrep,
                                ci_level = ci_level,
                                rejection_rates_args = rejection_rates_args)

    # Check these for compatibility:

    is_x_from_power <- TRUE
    if (object$solution_found &&
        (object$nrep_final == final_nrep) &&
        (object$ci_level == ci_level) &&
        (object$target_power == target_power)) {
      if (progress) {
        cat("\n--- Solution Already Found ---\n\n")
        cat("Solution already found in object. It is returned as is.\n")
      }
      return(object)
    }
    object <- object$power4test_trials
  }

  # Is it a by_x object??
  # - If yes and solution not found,
  #   - extract the power4test object closest to the solution
  # - If yes and solution found,
  #   - This is possible because the by_x object
  #     may be generated directly by user,
  #     not extracted from an x_from_power object.

  is_by_x <- FALSE
  if (inherits(object, "power4test_by_n") ||
      inherits(object, "power4test_by_es")) {

    # ==== by_x object ====

    is_by_x <- TRUE
    object_by_org <- object

    # Update rejection_rates_args

    object_by_org <- set_rejection_rates_args_by_x(
                        object_by_org,
                        rejection_rates_args = rejection_rates_args
                      )

    # Whether a solution exists will be checked later

    i_org <- find_solution(
               object_by_org,
               target_power = target_power,
               ci_level = ci_level,
               what = what,
               tol = tolerance,
               goal = goal,
               final_nrep = 1,
               closest_ok = TRUE,
               if_ties = "min")
    object <- object_by_org[[i_org]]
  } else {
    object_by_org <- NA
  }

  # ==== Update rejection_rates_args ====

  tmp <-  attr(object, "args")
  tmp2 <- tmp$rejection_rates_args
  tmp2 <- utils::modifyList(
                tmp2,
                rejection_rates_args,
                keep.null = TRUE
              )
  tmp$rejection_rates_args <- tmp2
  attr(object, "args") <- tmp
  rm(tmp)

  # The object to be used below is always a power4test object

  time_start <- Sys.time()

  if (progress) {
    cat("\n--- Setting ---\n\n")

    cat("Algorithm: ", algorithm, "\n")
    cat("Goal: ", goal, "\n")
    tmp <- switch(what,
                  point = "(Estimated Power)",
                  ub = "(Upper bound of the confidence interval)",
                  lb = "(Lower bound of the confidence interval)")
    cat("What: ", what, " ", tmp, "\n")

    if (changed_to_bisection) {
      catwrap(paste0("Note: Only bisection is supported when goal is 'close_enough'. ",
                     "Algorithm switched automatically to bisection."),
              exdent = 0)
    }
    if (changed_to_point) {
      cat("\n")
      catwrap(paste0("Note: The goal 'ci_hit' only supports 'point'; ",
                     "'what' is changed to 'point'."),
              exdent = 0)
    }

    if (progress) {
      cat("\n--- Progress  ---\n\n")
      catwrap("- Set 'progress = FALSE' to suppress displaying the progress.",
              exdent = 2)
      if (simulation_progress) {
        catwrap(paste0("- Set 'simulation progress = FALSE' ",
                       "to suppress displaying the progress ",
                       "in the simulation."),
                exdent = 2)
      }
    }
  }

  # Initialize these flags because a solution
  # may already be present before doing the search

  ci_hit <- FALSE
  solution_found <- FALSE
  status <- NULL
  technical <- NULL

  # === Check Existing Solution ===

  # If the input object is a by_x object,
  # - Check if the solution is already in one of the
  #   power4test object.
  #   - If yes, skip the search and create the
  #     x_from_power object.

  # ==== Solution already in by_x? ====

  if (is_by_x) {

    i_org_hit <- find_solution(
                   object_by_org,
                   ci_level = ci_level,
                   target_power = target_power,
                   what = what,
                   tol = tolerance,
                   goal = goal,
                   final_nrep = final_nrep,
                   closest_ok = FALSE,
                   if_ties = "min")

    if (!is.na(i_org_hit) && !is.null(i_org_hit)) {

      # ==== Solution in by_x. Skip the search ====

      # Solution already in the input.
      # DO not do the search

      if (progress) {
        cat("\n--- Solution Already Found ---\n\n")
        cat("Solution already found in the object. Search will be skipped.\n")
      }

      # ==== Create the output ====

      # Prepare the objects as if the search has completed

      ci_hit <- ifelse(goal == "ci_hit",
                       TRUE,
                       NA)
      solution_found <- TRUE
      i2 <- i_org_hit
      if (is_x_from_power || is_by_x) {
        by_x_1 <- object_by_org
        tmp1 <- rejection_rates(object_by_org,
                                level = ci_level,
                                all_columns = TRUE)
        fit_1 <- power_curve(by_x_1,
                             formula = power_curve_args$power_model,
                             start = power_curve_args$start,
                             lower_bound = power_curve_args$lower_bound,
                             upper_bound = power_curve_args$upper_bound,
                             nls_control = power_curve_args$nls_control,
                             nls_args = power_curve_args$nls_args,
                             verbose = progress)
        ci_hit <- ci_hit
        x_tried <- get_x_tried(tmp1,
                               x = x)
        x_out <- switch(x,
                        n = tmp1$n[i2],
                        es = tmp1$es[i2])
        power_out <- tmp1$reject[i2]
        nrep_out <- tmp1$nrep[i2]
        ci_out <- unlist(tmp1[i2, c("reject_ci_lo", "reject_ci_hi")])
        by_x_out <- by_x_1[[i2]]

        # ==== Reconstruct technical ====

        # TODO:
        # - (Not urgent) Not an ideal solution.
        #   Other functions should not rely on $technical
        #   because the search may have been skipped.

        technical <- list(tol = tolerance)

      }
    }
  }

  # x_interval is the actual range of values
  # to be searched, not necessarily the one
  # set by users.

  # ==== Fix x_interval ====

  if (solution_found) {
    x_interval <- range(x_tried)
  } else {
    if (check_es_interval) {

      # ==== Find probable es interval ====

      x_interval <- fix_es_interval(object = object,
                                    x = x,
                                    pop_es_name = pop_es_name,
                                    x_interval = x_interval,
                                    progress = progress)
    }
  }

  x_max <- max(x_interval)
  x_min <- min(x_interval)

  # ==== Check the tests ====
  tmp <- object$test_all[[1]][[1]]$test_results
  if (!is.null(dim(tmp))) {
    if (dim(tmp)[1] > 1) {
      tmp2 <- utils::capture.output(print(tmp))
      tmp3 <- paste(
                c("Does not support a test with more than one result:",
                tmp2),
                collapse = "\n")
      stop(tmp3)
    }
  }

  # ==== Call the algorithm ====

  if ((algorithm == "power_curve") && !solution_found) {

    a_out <- do.call(alg_power_curve,
      list(
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
        power_model = power_curve_args$power_model,
        start = power_curve_args$start,
        lower_bound = power_curve_args$lower_bound,
        upper_bound = power_curve_args$upper_bound,
        nls_control = power_curve_args$nls_control,
        nls_args = power_curve_args$nls_args,
        models = power_curve_args$models,
        final_nrep = final_nrep,
        final_R = final_R,
        max_trials = max_trials,
        ci_level = ci_level,
        extendInt = extendInt,
        power_tolerance_in_interval = power_tolerance_in_interval,
        power_tolerance_in_final = power_tolerance_in_final,
        what = what,
        goal = goal,
        tol = tolerance,
        variants = control
      ))

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
    status <- a_out$status
    technical <- list(iteration = a_out$iteration,
                      x_history = a_out$x_history,
                      reject_history = a_out$reject_history,
                      delta_tol = a_out$delta_tol,
                      last_k = a_out$last_k,
                      tol = a_out$tol)

    rm(a_out)

  }

  if ((algorithm == "probabilistic_bisection") && !solution_found) {

    a_out <- do.call(alg_prob_bisection,
      list(
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
          ci_level = ci_level,
          extendInt = extendInt,
          max_trials = max_trials,
          R = attr(object, "args")$R,
          digits = 3,
          what = what,
          goal = goal,
          tol = tolerance,
          variants = control
        ))

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
    status <- a_out$status
    technical <- list(iteration = a_out$iteration,
                      x_history = a_out$x_history,
                      x_interval_history = a_out$x_interval_history,
                      f_interval_history = a_out$f_interval_history,
                      reject_history = a_out$reject_history,
                      reject_by_power_curve_history = a_out$reject_by_power_curve_history,
                      f_history = a_out$f_history,
                      dfun_history = a_out$dfun_history,
                      p_c_history = a_out$p_c_history,
                      hdr_x_history = a_out$hdr_x_history,
                      hdr_power_history = a_out$hdr_power_history,
                      final_check_history = a_out$final_check_history,
                      seed_history = a_out$seed_history,
                      time_passed_history = a_out$time_passed_history,
                      delta_tol = a_out$delta_tol,
                      last_k = a_out$last_k,
                      tol = a_out$tol,
                      f_power = a_out$f_power,
                      f_goal = a_out$f_goal,
                      hdr_power_tol = a_out$hdr_power_tol)

    rm(a_out)

  }


  if ((algorithm == "bisection") && !solution_found) {

    a_out <- do.call(alg_bisection,
      list(
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
          ci_level = ci_level,
          extendInt = extendInt,
          max_trials = max_trials,
          R = attr(object, "args")$R,
          digits = 3,
          what = what,
          goal = goal,
          tol = tolerance,
          variants = control
        ))

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
    status <- a_out$status
    technical <- list(iteration = a_out$iteration,
                      x_history = a_out$x_history,
                      x_interval_history = a_out$x_interval_history,
                      f_interval_history = a_out$f_interval_history,
                      reject_history = a_out$reject_history,
                      f_history = a_out$f_history,
                      delta_tol = a_out$delta_tol,
                      last_k = a_out$last_k,
                      tol = a_out$tol)

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


  if (progress) {
    cat("\n========== Final Stage ==========\n\n")
    tmp <- format(Sys.time(), "%Y-%m-%d %X")
    cat("- Start at", tmp, "\n")

    cat("- Rejection Rates:\n\n")
    tmp <- rejection_rates(by_x_1,
                           level = ci_level)
    print(tmp)
    cat("\n")
    cat("- Estimated Power Curve:\n\n")
    print(fit_1)
    cat("\n")
  }

  # ==== Is solution found ====

  # Is solution found?
  # - The maximum number of replications reached.

  if (goal == "ci_hit") {

    # ==== Goal: ci_hit ====

    if (isTRUE(ci_hit) && (nrep_out == final_nrep)) {

      # ==== Solution found ====

      # ** x_final, by_x_final, power_final, ci_final, nrep_final, i_final **
      # The solution.

      # ==== Create _final objects ====

      x_final <- x_out
      by_x_final <- by_x_out
      power_final <- power_out
      ci_final <- ci_out
      nrep_final <- nrep_out
      i_final <- i2
    } else {

      # ==== Solution not found ====

      # No solution found.
      # Set the NAs to denote this.

      # ==== Set _final objects to NA====

      x_final <- NA
      by_x_final <- NA
      power_final <- NA
      ci_final <- NA
      nrep_final <- NA
      i_final <- NA
    }
  }

  if (goal == "close_enough") {

    # ==== Goal: close_enough ====

    if (isTRUE(solution_found) && (nrep_out == final_nrep)) {

      # ==== Solution found ====

      # ** x_final, by_x_final, power_final, ci_final, nrep_final, i_final **
      # The solution.

      # ==== Create _final objects ====

      x_final <- x_out
      by_x_final <- by_x_out
      power_final <- power_out
      ci_final <- ci_out
      nrep_final <- nrep_out
      i_final <- i2
    } else {

      # ==== Solution not found ====

      # No solution found.
      # Set the NAs to denote this.

      # ==== Set _final objects to NA====

      x_final <- NA
      by_x_final <- NA
      power_final <- NA
      ci_final <- NA
      nrep_final <- NA
      i_final <- NA
    }
  }

  # ==== Compute extrapolated x? ====

  # ** x_x **
  # The estimated value based on power_curve.
  # Used as a suggestion when no solution was found.
  x_x <- NA

  if (solution_found) {

    # ==== Yes only if solution_found ====

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
                            power_min = .01,
                            power_max = .99,
                            interval = x_interval,
                            extendInt = "yes",
                            x_to_exclude = x_tried)
  }

  if (progress) {
    if (solution_found && (nrep_out == final_nrep)) {
      cat("\n")
      x_out_str <- formatC(x_out,
                           digits = switch(x, n = 0, es = 4),
                           format = "f")
      cat("- Final Value:", x_out_str, "\n\n")
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
          "in the interval meet the goal.\n")
      if (!is.na(x_x) &&
          (goal == "ci_hit")) {
        x_x_str <- formatC(x_x,
                           digits = switch(x, n = 0, es = 4),
                           format = "f")
        cat("- The estimated required value is ",
            x_x_str,
            ".\n", sep = "")
      }
      cat("\nTry changing the setup, such as:\n")
      tmp <- switch(x,
                    n = paste0("[",
                               paste0(x_interval, collapse = ", "),
                               "]"),
                    es = paste0("[",
                               paste0(formatC(x_interval,
                                              digits = 3,
                                              format = "f"),
                                      collapse = ", "),
                               "]"))
      catwrap(paste0("- Changing the 'seed' to another value."),
              exdent = 2)
      catwrap(paste0("- Changing 'x_interval' to a wider range to examine. ",
                     "(Current 'x_interval' is ",
                     tmp, ".)"),
              exdent = 2)
      catwrap(paste0("- Increasing the maximum number of trials by ",
                     "setting 'max_trials' to a larger number. ",
                     "(Current 'max_trials' is ",
                     max_trials,
                     ".)"),
              exdent = 2)
      if (goal == "close_enough") {
        catwrap(paste0("- Increasing the tolerance for the goal 'close_enough'. ",
                      "(Current 'tolerance' is ",
                      tolerance,
                      ".)"),
                exdent = 2)
      }
    }
  }

  # ==== Finalize the Output ====

  my_call <- as.list(match.call())[-1]
  args <- formals()
  args <- utils::modifyList(args,
                            my_call)
  args$object <- NULL
  reject_1 <- rejection_rates(by_x_1,
                              level = ci_level)
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
              solution_found = solution_found,
              what = what,
              goal = goal,
              args = args,
              status = status,
              technical = technical,
              algorithm = algorithm,
              call = match.call(),
              rejection_rates_args = rejection_rates_args)
  class(out) <- c("x_from_power", class(out))
  return(out)
}

#' @rdname x_from_power
#'
#' @details
#'
#' The function [n_from_power()] is just
#' a wrapper of [x_from_power()], with
#' `x` set to `"n"`.
#'
#' @export
n_from_power <- function(object,
                         pop_es_name = NULL,
                         target_power = .80,
                         what = formals(x_from_power)$what,
                         goal = formals(x_from_power)$goal,
                         ci_level = .95,
                         tolerance = NULL,
                         x_interval = c(50, 2000),
                         extendInt = NULL,
                         progress = TRUE,
                         simulation_progress = NULL,
                         max_trials = NULL,
                         final_nrep = formals(x_from_power)$final_nrep,
                         final_R = formals(x_from_power)$final_R,
                         seed = NULL,
                         x_include_interval = FALSE,
                         check_es_interval = TRUE,
                         power_curve_args = list(power_model = NULL,
                                                 start = NULL,
                                                 lower_bound = NULL,
                                                 upper_bound = NULL,
                                                 nls_control = list(),
                                                 nls_args = list()),
                         save_sim_all = FALSE,
                         algorithm = NULL,
                         control = list(),
                         internal_args = list(),
                         rejection_rates_args = list(collapse = "all_sig",
                                                     at_least_k = 1,
                                                     p_adjust_method = "none",
                                                     alpha = .05)
                         ) {
  what <- match.arg(eval(what),
                    c("point", "ub", "lb"))
  goal <- match.arg(eval(goal),
                    c("ci_hit", "close_enough"))
  if ((goal == "ci_hit") &&
      (what != "point")) {
    what <- "point"
  }
  my_call <- match.call()
  my_call$x <- "n"
  my_call$what <- what
  my_call$goal <- goal
  my_call$final_nrep <- eval(final_nrep)
  my_call$final_R <- eval(final_R)
  my_call[[1]] <- quote(power4mome::x_from_power)
  out <- eval(my_call,
              envir = parent.frame())
  out
}

#' @rdname x_from_power
#'
#' @details
#'
#' The function [n_region_from_power()] is just
#' a wrapper of [x_from_power()], with
#' `x` set to `"n"`, with two passes, one
#' with `what = "ub"` and one with
#' `what = "lb"`.
#'
#' @return
#' The function [n_region_from_power()]
#' returns a named list of two output of
#' [n_from_power()], of the class
#' `n_region_from_power`. The output
#' with `what = "ub"` is named `"below"`,
#' and the output with `what = "lb"` is
#' namd `"above"`.
#'
#' @export
n_region_from_power <- function(
                         object,
                         pop_es_name = NULL,
                         target_power = .80,
                         ci_level = .95,
                         tolerance = NULL,
                         x_interval = c(50, 2000),
                         extendInt = NULL,
                         progress = TRUE,
                         simulation_progress = NULL,
                         max_trials = NULL,
                         final_nrep = formals(x_from_power)$final_nrep,
                         final_R = formals(x_from_power)$final_R,
                         seed = NULL,
                         x_include_interval = FALSE,
                         check_es_interval = TRUE,
                         power_curve_args = list(power_model = NULL,
                                                 start = NULL,
                                                 lower_bound = NULL,
                                                 upper_bound = NULL,
                                                 nls_control = list(),
                                                 nls_args = list()),
                         save_sim_all = FALSE,
                         algorithm = NULL,
                         control = list(),
                         internal_args = list(),
                         rejection_rates_args = list(collapse = "all_sig",
                                                     at_least_k = 1,
                                                     p_adjust_method = "none",
                                                     alpha = .05)
                         ) {
  my_call <- match.call()
  my_call$final_nrep <- eval(final_nrep)
  my_call$final_R <- eval(final_R)
  my_call$x <- "n"
  my_call[[1]] <- quote(power4mome::x_from_power)
  if (progress) {
    tmp <- strwrap(paste0("Find the approximate region with power ",
                         "significantly below ",
                         target_power,
                         " ..."))
    cat("\n=========== Phase 1: Upper Bound ===========\n\n")
    cat(tmp, sep = "\n")
  }
  my_call$what <- "ub"
  my_call$goal <- "close_enough"
  out_ub <- eval(
              my_call,
              envir = parent.frame())
  if (progress) {
    tmp <- strwrap(paste0("Find the approximate region with power ",
                         "significantly above ",
                         target_power,
                         " ..."))
    cat("\n=========== Phase 2: Lower Bound ===========\n\n")
    cat(tmp, sep = "\n")
  }
  my_call$what <- "lb"
  my_call$goal <- "close_enough"
  tmp <- out_ub
  tmp$what <- "lb"
  tmp$goal <- "close_enough"
  tmp$solution_found <- FALSE
  tmp$call$what <- "lb"
  tmp$call$goal <- "close_enough"
  my_call2 <- my_call
  my_call2$object <- tmp
  out_lb <- eval(
              my_call2,
              envir = parent.frame())
  out_lb$call <- my_call
  out <- list(
          below = out_ub,
          above = out_lb
        )
  class(out) <- c("n_region_from_power", class(out))
  attr(out, "call") <- match.call()
  out
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
#' The `print` method only prints
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
  if (!is.symbol(my_call$object)) {
    my_call$object <- as.symbol("<hidden>")
  }
  if (!is.symbol(my_call[[1]])) {
    my_call[[1]] <- as.symbol("x_from_power")
  }
  cat("\n")
  solution_found <- !is.na(x$x_final)
  predictor <- x$x

  goal <- x$goal
  what <- x$what
  algorithm <- x$algorithm
  ci_level_str <- paste0(formatC(
                          x$ci_level * 100,
                          digits = 2,
                          format = "f"),
                        "%")

  tmp1 <- c("Predictor(x):" =
            switch(predictor,
             n = "Sample Size",
             es = "Effect Size"))
  tmp1b <- c("Parameter:" =
            switch(predictor,
             n = "N/A",
             es = x$pop_es_name))
  tmp2 <- c("goal:" = goal)
  tmp3 <- c("what:" = what)
  tmp4 <- c("algorithm:" = algorithm)
  tmp5 <- c("Level of confidence:" = ci_level_str)
  tmp6 <- c("Target Power:" =
              formatC(x$target_power, digits = digits, format = "f"))

  tmp <- data.frame("Setting" = c(
      tmp1,
      tmp1b,
      tmp2,
      tmp3,
      tmp4,
      tmp5,
      tmp6
    ))

  print(tmp)

  if (solution_found) {
    x_final_str <- formatC(x$x_final,
                           digits = switch(predictor,
                                           n = 0,
                                           es = digits),
                           format = "f")
    cat("\n- Final Value of",
        switch(x$x,
               n = " Sample Size (n): ",
               es = paste0("'", x$pop_es_name, "': ")),
        x_final_str,
        "\n\n",
        sep = "")
    ci_str <- paste0(
        "[",
        formatC(x$ci_final[1], digits = digits, format = "f"),
        ", ",
        formatC(x$ci_final[2], digits = digits, format = "f"),
        "]")
    cat("- Final Estimated Power (CI): ",
        formatC(x$power_final, digits = digits, format = "f"),
        " ",
        ci_str,
        "\n",
        sep = "")
  } else {
    cat("\n- Solution not found.\n")
  }
  cat("\nCall `summary()` for detailed results.\n")
  invisible(x)
}


#' @rdname x_from_power
#'
#' @return
#' The `print`-method of `x_from_power_region`
#' objects returns the object `x`
#' invisibly.
#' It is called for its side effect.
#'
#' @export
print.n_region_from_power <- function(
                              x,
                              digits = 3,
                              ...) {
  my_call <- attr(x, "call")
  cat("Call:\n")
  if (!is.symbol(my_call$object)) {
    my_call$object <- as.symbol("<hidden>")
  }
  if (!is.symbol(my_call[[1]])) {
    my_call[[1]] <- as.symbol("n_region_from_power")
  }
  print(my_call)
  cat("\n")
  x_below <- x$below
  x_above <- x$above
  solution_found_below <- !is.na(x_below$x_final)
  solution_found_above <- !is.na(x_above$x_final)
  predictor <- x_below$x

  goal <- x_below$goal
  what <- x_below$what
  algorithm <- x_below$algorithm
  ci_level_str <- paste0(formatC(
                          x_below$ci_level * 100,
                          digits = 2,
                          format = "f"),
                        "%")

  tmp1 <- c("Predictor(x)" = "Sample Size")
  tmp2 <- c("Goal:" = "Power significantly below or above the target")
  tmp4 <- c("algorithm:" = algorithm)
  tmp5 <- c("Level of confidence:" = ci_level_str)
  target_power_str <- formatC(x_below$target_power, digits = digits, format = "f")
  tmp6 <- c("Target Power:" = target_power_str)

  tmp <- data.frame("Setting" = c(
      tmp1,
      tmp2,
      tmp4,
      tmp5,
      tmp6
    ))

  print(tmp, right = FALSE)

  if (solution_found_below) {

    x_final_below_str <- formatC(x_below$x_final,
                           digits = switch(predictor,
                                           n = 0,
                                           es = digits),
                           format = "f")
    ci_below_str <- paste0(
        "[",
        formatC(x_below$ci_final[1], digits = digits, format = "f"),
        ", ",
        formatC(x_below$ci_final[2], digits = digits, format = "f"),
        "]")

  }

  if (solution_found_above) {

    x_final_above_str <- formatC(x_above$x_final,
                           digits = switch(predictor,
                                           n = 0,
                                           es = digits),
                           format = "f")
    ci_above_str <- paste0(
        "[",
        formatC(x_above$ci_final[1], digits = digits, format = "f"),
        ", ",
        formatC(x_above$ci_final[2], digits = digits, format = "f"),
        "]")

  }

  solution_found <- solution_found_below &&
                    solution_found_above

  if (solution_found_below ||
      solution_found_above) {

    cat("\nSolution: \n")

    cat("\nApproximate region of sample sizes with power:\n")
    if (solution_found) {
      tmp <- paste0("- not significantly different from ",
                    target_power_str,
                    ": ",
                    x_final_below_str,
                    " to ",
                    x_final_above_str)
      cat(strwrap(tmp, exdent = 2), sep = "\n")
    }
    if (solution_found_below) {
      tmp <- paste0("- significantly lower than ",
                    target_power_str,
                    ": ",
                    x_final_below_str)
      cat(strwrap(tmp, exdent = 2), sep = "\n")
    }
    if (solution_found_above) {
      tmp <- paste0("- significantly higher than ",
                    target_power_str,
                    ": ",
                    x_final_above_str)
      cat(strwrap(tmp, exdent = 2), sep = "\n")
    }

    cat("\nConfidence intervals of the estimated power:\n")
    if (solution_found_below) {
      tmp <- paste0("- for the lower bound (",
                    x_final_below_str,
                    "): ",
                    ci_below_str)
      cat(strwrap(tmp, exdent = 2), sep = "\n")
    }
    if (solution_found_above) {
      tmp <- paste0("- for the upper bound (",
                    x_final_above_str,
                    "): ",
                    ci_above_str)
      cat(strwrap(tmp, exdent = 2), sep = "\n")
    }

    if (!solution_found_below) {
      cat("Solution not found for the lower region.")
    }
    if (!solution_found_above) {
      cat("Solution not found for the upper region.")
    }
  } else {
    cat("\n- Solution not found.\n")
  }
  cat("\nCall `summary()` for detailed results.\n")
  invisible(x)
}

#' @rdname x_from_power
#'
#' @details
#'
#' The function [arg_x_from_power()]
#' is a helper to set argument values
#' if `object` is an output
#' of [x_from_power()] or similar
#' functions.
#'
#' @return
#' The function [arg_x_from_power()]
#' returns the requested argument if
#' available. If not available, it
#' returns `NULL`.
#'
#' @param arg The name of element to
#' retrieve.
#'
#' @param arg_in The name of the element
#' from which an element is to be
#' retrieved.
#'
#' @export
arg_x_from_power <- function(
                      object,
                      arg,
                      arg_in = NULL) {
  if (inherits(object, "x_from_power")) {
    if (is.null(arg_in)) {
      return(object[[arg]])
    } else {
      return(object[[arg_in]][[arg]])
    }
  } else {
    return(NULL)
  }
}
