#' @title All-in-One Power Estimation For Mediation Models
#'
#' @description All-in-one functions for
#' estimating power or finding the
#' region with target power for common
#' mediation models.
#'
#' @details
#'
#' # All-in-One Functions for Common Mediation Models
#'
#' These functions are wrappers
#' that call [power4test()] and
#' [n_region_from_power()] to (a)
#' estimate the level of power for
#' a mediation model, given the population
#' effects and the sample size, and
#' (b) find the region of sample sizes
#' with the levels of power not
#' significantly different from the
#' target power.
#'
#' They are convenient functions that
#' set the argument values automatically
#' for common mediation models before
#' calling [power4test()] and
#' [n_region_from_power()]. Please refer
#' to the help pages of these two
#' functions for the details on how
#' the estimation and the search are
#' conducted.
#'
#' For some arguments not described in
#' details here, please refer to the
#' help pages of [power4test()]
#' and [n_region_from_power()],
#'
#' # Simple Mediation Model
#'
#' The function [q_power_mediation_simple()]
#' can be used for the power analysis of
#' a simple mediation model with only
#' one mediator.
#'
#' This function will fit
#' the following model:
#'
#' \preformatted{"m ~ x
#'  y ~ m + x"}
#'
#' # Serial Mediation Model
#'
#' The function [q_power_mediation_serial()]
#' can be used for the power analysis of
#' a serial mediation model with only
#' any number of mediators.
#'
#' This is the model being fitted if
#' the model has two mediators:
#'
#' \preformatted{"m1 ~ x
#'  m2 ~ m1 + x
#'  y ~ m2 + m1 + x"}
#'
#' # Parallel Mediation Model
#'
#' The function [q_power_mediation_parallel()]
#' can be used for the power analysis of
#' a parallel mediation model with only
#' any number of mediators.
#'
#' This is the model being fitted if
#' the model has two mediators:
#'
#' \preformatted{"m1 ~ x
#'  m2 ~ x
#'  y ~ m2 + m1 + x"}
#'
#' # An Arbitrary Mediation Model
#'
#' The function [q_power_mediation()],
#' an advanced function,
#' can be used for the power analysis of
#' an arbitrary mediation model.
#' The model and the population effect
#' sizes are specified as in [power4test()].
#'
#' This is an example of a model with
#' both parallel paths and serial paths:
#'
#' \preformatted{model <-
#'  "
#'  m1 ~ x
#'  m21 ~ m1
#'  m22 ~ m1
#'  y ~ m21 + m22 + x
#'  "}
#'
#' \preformatted{pop_es <-
#' "
#' m1 ~ x: m
#' m21 ~ m1: m
#' m22 ~ m1: m
#' y ~ m21: m
#' y ~ m22: m
#' "}
#'
#' Knowledge of using [power4test()]
#' is required to use this advanced
#' function.
#'
#' If this advanced function is used,
#' users need to specify `test_fun`
#' as when using [power4test()], and
#' need to set `test_args` correctly
#'
#' @return
#' If `mode` is `power`, then a
#' `power4test` object is returned.
#' If `mode` is `region`, then a
#' `n_region_from_power` object is
#' returned.
#'
#' @inheritParams power4test
#' @inheritParams n_region_from_power
#'
#' @param test_fun A function to do the
#' test. See 'Details' of [power4test()]
#' for the requirement of this function.
#'
#' @param test_more_args A named list of
#' additional arguments to be passed
#' to the test function
#' ([test_indirect_effect()] for
#' simple and serial mediation models,
#' and [test_k_indirect_effects()]
#' for parallel mediation models).
#' Similar to `test_args` in
#' [power4test()].
#'
#' @param iseed The seed for the random
#' number generator. Used by
#' [power4test()].
#'
#' @param seed The seed for the random
#' number generator. Used by
#' [n_region_from_power()].
#'
#' @param parallel If `TRUE`, parallel
#' processing will be used when calling
#' other functions, if appropriate.
#'
#' @param ... For `q_power_mediation_*`,
#' these are optional arguments to
#' be passed to [power4test()] and
#' [n_region_from_power()].
#' For the `print` method, these
#' are optional arguments to
#' be passed to other print methods
#' (see [print.power4test()] and
#' [print.n_region_from_power()]).
#' For the `plot` method, these
#' are optional arguments to
#' be passed to [plot.n_region_from_power()].
#' For the `summary` method, these
#' are optional arguments to
#' be passed to [summary.n_region_from_power()].
#'
#' @param mode If `"power"`, then
#' only [power4test()] will be called,
#' and the level of power will be
#' estimated. If `"region"`, then
#' the region of sample sizes with
#' levels of power not significantly
#' different from the target power will
#' be searched by calling
#' [n_region_from_power()].
#'
#' @seealso See [power4test()] and
#' [n_region_from_power()] for full
#' details on how these functions
#' work.
#'
#' @examples
#'
#' \dontrun{
#'
#' # An arbitrary mediation model
#'
#' model <-
#' "
#' m1 ~ x
#' m21 ~ m1
#' m22 ~ m1
#' y ~ m21 + m22
#' "
#' pop_es <-
#' "
#' m1 ~ x: m
#' m21 ~ m1: m
#' m22 ~ m1: m
#' y ~ m21: m
#' y ~ m22: m
#' "
#'
#' # NOTE: In real power analysis:
#' # - Set R to an appropriate value.
#' # - Remove nrep or set nrep to the desired value.
#' # - Remove parallel or set it to TRUE to enable parallel processing.
#' # - Remove progress or set it to TRUE to see the progress.
#'
#' outa1 <- q_power_mediation(
#'     model = model,
#'     pop_es = pop_es,
#'     n = 100,
#'     R = 199,
#'     test_fun = test_k_indirect_effects,
#'     test_more_args = list(x = "x",
#'                           y = "y",
#'                           omnibus = "all"),
#'     seed = 1234,
#'     mode = "region",
#'     nrep = 20,
#'     parallel = FALSE,
#'     progress = FALSE
#'   )
#' outa1
#' summary(outa1)
#' plot(outa1)
#'
#' }
#'
#' @export
q_power_mediation <- function(
  model = NULL,
  pop_es = NULL,
  number_of_indicators = NULL,
  reliability = NULL,
  test_fun = NULL,
  test_more_args = list(),
  target_power = 0.8,
  nrep = 400,
  n = 100,
  R = 1000,
  ci_type = c("mc", "boot"),
  seed = NULL,
  iseed = NULL,
  parallel = TRUE,
  progress = TRUE,
  simulation_progress = TRUE,
  max_trials = 10,
  ...,
  mode = c("power", "region")
) {
  # A general quick functions for other
  # wrappers.
  ci_type <- match.arg(ci_type)
  mode <- match.arg(mode)

  if (progress) {
    tmp <- switch(
              mode,
              power = paste("Only the power for n =",
                            n,
                            "will be estimated."),
              region = paste("The region of sample sizes",
                             "for a power of",
                              sprintf("%3.2f", target_power),
                             "will be searched.")
    )
    cat("\n",
        tmp,
        "\n\n")
  }

  if (!parallel &&
      progress) {
    cat("\nIt is recommended to set 'parallel' to TRUE",
        "to speed up the computation.\n\n")
  }

  # model cannot be NULL for this general function.
  # The wrappers will be responsible for
  # setting model and pop_es.

  if (is.null(model)) {
    stop("'model' cannot be NULL calling this function directly")
  }
  if (is.null(pop_es)) {
    stop("'pop_es' cannot be NULL if calling this function directly")
  }

  if (is.null(test_fun)) {
    stop("'test_fun' cannot be NULL if calling this function directly")
  }

  ddd <- list(...)

  # ==== Process optional arguments =====

  power4test_args <- names(formals(power4test))
  ddd_power4test <- ddd[names(ddd) %in% power4test_args]

  n_region_from_power_args <- names(formals(n_region_from_power))
  ddd_n_from_region <- ddd[names(ddd) %in% n_region_from_power_args]

  # No need. This is the new default behavior of n_from_region
  # if (is.null(ddd_n_from_region$final_nrep)) {
  #   ddd_n_from_region$final_nrep <- nrep
  # }

  # ==== Set test arguments ====

  test_args0 <- list(
                    mc_ci = isTRUE(ci_type == "mc"),
                    boot_ci = isTRUE(ci_type == "boot")
                  )
  test_args0 <- utils::modifyList(
                  test_args0,
                  test_more_args
                )

  # ==== Call power4test() =====

  p4t_args <- list(
                nrep = nrep,
                model = model,
                pop_es = pop_es,
                n = n,
                number_of_indicators = number_of_indicators,
                reliability = reliability,
                R = R,
                ci_type = ci_type,
                test_fun = test_fun,
                test_args = test_args0,
                iseed = iseed %||% seed,
                parallel = parallel,
                progress = progress,
                ncores = max(1, parallel::detectCores(logical = FALSE) - 1)
              )

  p4t_args <- utils::modifyList(
                p4t_args,
                ddd_power4test
              )

  out <- do.call(
            power4test,
            p4t_args
          )

  # ==== Find the region ====

  if (mode == "region") {

    # Do this only if mode is not "power",
    # i.e., "region"

    n_reg_args <- list(
              object = out,
              target_power = target_power,
              progress = progress,
              simulation_progress = simulation_progress,
              max_trials = max_trials,
              seed = seed
            )

    n_reg_args <- utils::modifyList(
                  n_reg_args,
                  ddd_n_from_region
                )

    out2 <- do.call(
              n_region_from_power,
              n_reg_args
            )

  } else {
    out2 <- NULL
  }

  outz <- list(power4test = out,
               n_region_from_power = out2)
  class(outz) <- c("q_power_mediation",
                   class(outz))
  outz
}

#' @param x The object
#' for the relevant methods.
#'
#' @param mode What to print. If `"region"`
#' and the output of [n_region_from_power()]
#' is available, it will print the results
#' of [n_region_from_power()].
#' If `"power"`, then the output of
#' [power4test()] will be printed. If
#' `"all"`, then all available output
#' will be printed.
#'
#' @return
#' The `print` method of `q_power_mediation`
#' returns `x` invisibly. Called for
#' its side effect.
#'
#' @rdname q_power_mediation
#' @export
print.q_power_mediation <- function(
  x,
  mode = c("all", "region", "power"),
  ...
) {
  mode <- match.arg(mode)

  # ==== Print power4test ====

  if (mode %in% c("power", "all")) {
    cat("\n========== power4test Results ==========\n\n")
    print(x$power4test,
          ...)
    cat("\n========== power4test Power ==========\n\n")
    print(rejection_rates(x$power4test),
          ...)
  }

  # ==== Print n_region_from_power ====

  if (mode %in% c("region", "all")) {
    cat("\n========== n_region_from_power Results ==========\n\n")
    if (is.null(x$n_region_from_power)) {
      cat("\n'mode' is not 'region' and results not available.\n\n")
    } else {
      print(x$n_region_from_power,
            ...)
    }
  }

  invisible(x)
}

#' @return
#' The `plot`-method of `q_power_mediation`
#' returns `x` invisibly.
#' It is called for its side effect.
#'
#' @rdname q_power_mediation
#' @export
plot.q_power_mediation <- function(
  x,
  ...
) {
  if (is.null(x$n_region_from_power)) {
    stop("'mode' is not 'region' and nothing to plot.")
  }
  plot(x$n_region_from_power,
       ...)
}

#' @param object For the `summary`
#' method of [q_power_mediation()]
#' outputs.
#'
#' @return
#' The `summary` method for
#' `q_power_mediation` objects returns
#' the output of [summary.n_region_from_power()].
#' An error is raised if the output
#' of [n_region_from_power()] is not
#' available.
#'
#' @rdname q_power_mediation
#' @export
summary.q_power_mediation <- function(
                                object,
                                ...) {
  if (is.null(object$n_region_from_power)) {
    stop("'mode' is not 'region' and summary cannot be generated.")
  }
  out <- summary(object$n_region_from_power,
                 ...)
  out
}

#' @param a For a simple mediation
#' model, this is the population effect
#' size for the path from `x` to `m` .
#'
#' @param b For a simple mediation
#' model, this is the population effect
#' size for the path from `m` to `y` .
#'
#' @param cp For a simple mediation
#' model, this is the population effect
#' size for the direct path from
#' `c` to `y` .
#'
#' @examples
#'
#' # Simple mediation model
#'
#' # NOTE: In real power analysis:
#' # - Set R to an appropriate value.
#' # - Remove nrep or set nrep to the desired value.
#' # - Remove parallel or set it to TRUE to enable parallel processing.
#' # - Remove progress or set it to TRUE to see the progress.
#'
#' out <- q_power_mediation_simple(
#'     a = "m",
#'     b = "m",
#'     cp = "n",
#'     n = 50,
#'     R = 199,
#'     seed = 1234,
#'     nrep = 20,
#'     parallel = FALSE,
#'     progress = FALSE
#'   )
#' out
#'
#' # If mode = "region" is added, can call the following
#' # summary(out)
#' # plot(out)
#'
#' @rdname q_power_mediation
#' @export
q_power_mediation_simple <- function(
  a = "m",
  b = "m",
  cp = "n",
  number_of_indicators = NULL,
  reliability = NULL,
  test_more_args = list(),
  target_power = 0.8,
  nrep = 400,
  n = 100,
  R = 1000,
  ci_type = c("mc", "boot"),
  seed = NULL,
  iseed = NULL,
  parallel = TRUE,
  progress = TRUE,
  simulation_progress = TRUE,
  max_trials = 10,
  ...,
  mode = c("power", "region")
) {

  # ==== Setup the model =====

  model <- paste0(
            c("m ~ x",
              "y ~ m + x",
              ""),
            collapse = "\n"
          )
  pop_es <-
    sprintf(
      paste0(
        c(
          "m ~ x: %1$s",
          "y ~ m: %2$s",
          "y ~ x: %3$s",
          ""
        ),
        collapse = "\n"
      ),
      a, b, cp
    )

  # ==== Process number of indicators and reliability =====

  # Do not do the test for now because some of the variables
  # may be observed variables

  # if (length(number_of_indicators) > 1) {
  #     if (length(number_of_indicators) != 3) {
  #       stop("number_of_indicators must be one or three values")
  #     }
  #     if (!setequal(
  #           names(number_of_indicators),
  #           c("x", "m", "y"))) {
  #       stop("Names of number_of_indicators must 'x', 'm', and 'y'")
  #     }
  #   }
  # if (length(reliability) > 1) {
  #   if (length(reliability) != 3) {
  #     stop("reliability must be one or three values")
  #   }
  #   if (!setequal(
  #         names(reliability),
  #         c("x", "m", "y"))) {
  #     stop("Names of reliability must 'x', 'm', and 'y'")
  #   }
  # }

  # ==== Set test arguments ====

  # Set model specific arguments

  test_args0 <- utils::modifyList(
                  test_more_args,
                  list(x = "x",
                       m = "m",
                       y = "y")
                )

  # ==== Call the general function ====

  out <- q_power_mediation(
    model = model,
    pop_es = pop_es,
    number_of_indicators = number_of_indicators,
    reliability = reliability,
    test_fun = "test_indirect_effect",
    test_more_args = test_args0,
    target_power = target_power,
    nrep = nrep,
    n = n,
    R = R,
    ci_type = ci_type,
    seed = seed,
    iseed = iseed,
    parallel = parallel,
    progress = progress,
    simulation_progress = simulation_progress,
    max_trials = max_trials,
    ...,
    mode = mode
  )
  out
}

#' @param ab For a serial mediation
#' model, this is a numeric vector
#' of the population effect
#' sizes along the path
#' `x->m1->m2->...->y`.
#'
#' @param ab_other Should be one single
#' value. This is the population effect
#' sizes of all other paths not along
#' `x->m1->m2->...->y`, except for the
#' direct path from `x` to `y`.
#'
#' @examples
#'
#' \dontrun{
#' # Serial mediation model
#'
#' # NOTE: In real power analysis:
#' # - Set R to an appropriate value.
#' # - Remove nrep or set nrep to the desired value.
#' # - Remove parallel or set it to TRUE to enable parallel processing.
#' # - Remove progress or set it to TRUE to see the progress.
#'
#' outs <- q_power_mediation_serial(
#'     ab = c("s", "m", "l"),
#'     ab_others = "n",
#'     cp = "s",
#'     n = 50,
#'     R = 199,
#'     seed = 1234,
#'     mode = "region",
#'     nrep = 20,
#'     parallel = FALSE,
#'     progress = FALSE
#'   )
#' outs
#' summary(outs)
#' plot(outs)
#' }
#'
#' @rdname q_power_mediation
#' @export
q_power_mediation_serial <- function(
  ab = c("m", "m"),
  ab_other = "n",
  cp = "n",
  number_of_indicators = NULL,
  reliability = NULL,
  test_more_args = list(),
  target_power = 0.8,
  nrep = 400,
  n = 100,
  R = 1000,
  ci_type = c("mc", "boot"),
  seed = NULL,
  iseed = NULL,
  parallel = TRUE,
  progress = TRUE,
  simulation_progress = TRUE,
  max_trials = 10,
  ...,
  mode = c("power", "region")
) {

  # ==== Setup the model =====

  p <- length(ab)

  m_names <- c(paste0("m", seq_len(p - 1)),
               "y")

  all_m_paths <- character(0)
  x_previous <- "x"
  for (i in m_names) {
    all_m_paths <- c(all_m_paths,
                  paste0(i,
                         " ~ ",
                         x_previous)
                )
    x_previous <- union(x_previous, i)
  }

  model_m <- character(0)
  m_previous <- character(0)
  for (i in m_names) {
    model_m <- c(model_m,
                 paste0(i,
                        " ~ ",
                        paste0(c(m_previous,
                                 "x"),
                               collapse = " + "))
                )
    m_previous <- union(m_previous, i)
  }
  model <- paste0(
            model_m,
            collapse = "\n"
          )

  m_paths <- paste0(m_names,
         " ~ ",
         c("x", m_names[-p])
        )
  m_other_paths <- setdiff(
                      all_m_paths,
                      m_paths
                    )
  es_ab <- paste0(
            m_paths,
            ": ",
            ab
          )
  es_m_other <- paste0(
            setdiff(m_other_paths,
                    "y ~ x"),
            ": ",
            ab_other
          )
  es_y <- paste0(
            "y ~ x: ",
            cp
          )
  pop_es <- paste0(
      c(es_ab,
        es_m_other,
        es_y),
      collapse = "\n"
    )

  # ==== Process number of indicators and reliability =====

  # Do not do the test for now because some of the variables
  # may be observed variables

  # if (length(number_of_indicators) > 1) {
  #     if (length(number_of_indicators) != 3) {
  #       stop("number_of_indicators must be one or the number of factors")
  #     }
  #     if (!setequal(
  #           names(number_of_indicators),
  #           c("x", "m", "y"))) {
  #       stop("Names of number_of_indicators must 'x', 'm', and 'y'")
  #     }
  #   }
  # if (length(reliability) > 1) {
  #   if (length(reliability) != 3) {
  #     stop("reliability must be one or three values")
  #   }
  #   if (!setequal(
  #         names(reliability),
  #         c("x", "m", "y"))) {
  #     stop("Names of reliability must 'x', 'm', and 'y'")
  #   }
  # }


  # ==== Set the test arguments ====

  test_more_args <- utils::modifyList(
                      test_more_args,
                      list(x = "x",
                           y = "y",
                           m = c(m_names[-p]))
                    )

  # ==== Call the general function ====

  out <- q_power_mediation(
    model = model,
    pop_es = pop_es,
    number_of_indicators = number_of_indicators,
    reliability = reliability,
    test_fun = "test_indirect_effect",
    test_more_args = test_more_args,
    target_power = target_power,
    nrep = nrep,
    n = n,
    R = R,
    ci_type = ci_type,
    seed = seed,
    iseed = iseed,
    parallel = parallel,
    progress = progress,
    simulation_progress = simulation_progress,
    max_trials = max_trials,
    ...,
    mode = mode
  )
  out
}

#' @param as For a parallel mediation
#' model, this is a numeric vector
#' of the population effect
#' sizes for the paths from `x` to
#' the mediators: `x->m1`, `x->m2`,
#' ... `x->mp`, for a parallel mediation
#' model with `p` mediators.
#'
#' @param bs For a parallel mediation
#' model, this is a numeric vector
#' of the population effect
#' sizes for the paths from the
#' mediators to `y`: `m1->y`, `m2->y`,
#' ... `mp->y`, for a parallel mediation
#' model with `p` mediators.
#'
#' @param omnibus `"all_sig"`, the
#' default, then
#' the test is declared significant if
#' *all* paths are significant. If
#' `"at_least_one_sig"`, then only
#' one row of test is stored, and the
#' test is declared significant if
#' at least one of the paths is
#' significant. If `"at_least_k_sig"`,
#' then only one row of test is stored,
#' and the test is declared significant
#' if at least `k` of the paths is
#' significant, `k` determined by the
#' argument `at_least_k`.
#'
#' @param at_least_k The minimum number
#' of paths required to be significant
#' for the omnibus test to be considered
#' significant. Used when
#' `omnibus` is `"at_least_k_sig"`.
#'
#' @examples
#'
#' \dontrun{
#' # Parallel mediation model
#'
#' # NOTE: In real power analysis:
#' # - Set R to an appropriate value.
#' # - Remove nrep or set nrep to the desired value.
#' # - Remove parallel or set it to TRUE to enable parallel processing.
#' # - Remove progress or set it to TRUE to see the progress.
#'
#' outp <- q_power_mediation_parallel(
#'     as = c("s", "m"),
#'     bs = c("m", "s"),
#'     cp = "n",
#'     n = 100,
#'     R = 199,
#'     seed = 1234,
#'     mode = "region",
#'     nrep = 20,
#'     parallel = FALSE,
#'     progress = FALSE
#'   )
#' outp
#' summary(outp)
#' plot(outp)
#' }
#'
#' @rdname q_power_mediation
#' @export
q_power_mediation_parallel <- function(
  as = c("m", "m"),
  bs = c("m", "m"),
  cp = "n",
  number_of_indicators = NULL,
  reliability = NULL,
  omnibus = c("all_sig", "at_least_one_sig", "at_least_k_sig"),
  at_least_k = 1,
  test_more_args = list(),
  target_power = 0.8,
  nrep = 400,
  n = 100,
  R = 1000,
  ci_type = c("mc", "boot"),
  seed = NULL,
  iseed = NULL,
  parallel = TRUE,
  progress = TRUE,
  simulation_progress = TRUE,
  max_trials = 10,
  ...,
  mode = c("power", "region")
) {

  omnibus <- match.arg(omnibus)

  # ==== Setup the model =====

  if (length(as) != length(bs)) {
    stop("'as' and 'bs' must have the same number of elements.")
  }

  p <- length(bs)

  m_names <- paste0("m", seq_len(p))
  model_m <- paste0(m_names, " ~ x")
  model_y <- paste0("y ~ ",
                    paste0(m_names, collapse = " + "),
                    " + x")
  model <- paste0(
            c(model_m,
              model_y),
            collapse = "\n"
          )
  f <- function(m, a) {
      paste0(m, " ~ x: ",
             a)
    }
  es_m <- mapply(
              f,
              m = m_names,
              a = as,
              SIMPLIFY = TRUE
            )
  es_y <- paste0(
            "y ~ ",
            m_names,
            ": ",
            bs
          )
  es_cp <- paste0("y ~ x: ", cp)
  pop_es <- paste0(
      c(es_m,
        es_y,
        es_cp),
      collapse = "\n"
    )

# ==== Process number of indicators and reliability =====

  # Do not do the test for now because some of the variables
  # may be observed variables

  # if (length(number_of_indicators) > 1) {
  #     if (length(number_of_indicators) != 3) {
  #       stop("number_of_indicators must be one or three values")
  #     }
  #     if (!setequal(
  #           names(number_of_indicators),
  #           c("x", "m", "y"))) {
  #       stop("Names of number_of_indicators must 'x', 'm', and 'y'")
  #     }
  #   }
  # if (length(reliability) > 1) {
  #   if (length(reliability) != 3) {
  #     stop("reliability must be one or three values")
  #   }
  #   if (!setequal(
  #         names(reliability),
  #         c("x", "m", "y"))) {
  #     stop("Names of reliability must 'x', 'm', and 'y'")
  #   }
  # }

  # ==== Set the test arguments ====

  test_more_args <- utils::modifyList(
                      test_more_args,
                      list(x = "x",
                           y = "y",
                           omnibus = omnibus,
                           at_least_k = at_least_k)
                    )

  # ==== Call the general function ====

  out <- q_power_mediation(
    model = model,
    pop_es = pop_es,
    number_of_indicators = number_of_indicators,
    reliability = reliability,
    test_fun = "test_k_indirect_effects",
    test_more_args = test_more_args,
    omnibus = omnibus,
    at_least_k = at_least_k,
    target_power = target_power,
    nrep = nrep,
    n = n,
    R = R,
    ci_type = ci_type,
    seed = seed,
    iseed = iseed,
    parallel = parallel,
    progress = progress,
    simulation_progress = simulation_progress,
    max_trials = max_trials,
    ...,
    mode = mode
  )
  out
}