#' @noRd
# Work-In-Progress

q_power_region_mediation_simple <- function(
    a = "m",
    b = "m",
    cp = "n",
    number_of_indicators = NULL,
    reliability = NULL,
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
    ...
) {
  ci_type <- match.arg(ci_type)

  ddd <- list(...)

  power4test_args <- names(formals(power4test))
  ddd_power4test <- ddd[names(ddd) %in% power4test_args]

  n_region_from_power_args <- names(formals(n_region_from_power))
  ddd_n_from_region <- ddd[names(ddd) %in% n_region_from_power_args]


  # ==== Setup the model =====

  model <- paste0(
            c("m ~ x",
              "y ~ m + x",
              ""),
            collapse = "\n"
          )
  model_es <-
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

  if (length(number_of_indicators) > 1) {
      if (length(number_of_indicators) != 3) {
        stop("number_of_indicators must be one or three values")
      }
      if (!setequal(
            names(number_of_indicators),
            c("x", "m", "y"))) {
        stop("Names of number_of_indicators must 'x', 'm', and 'y'")
      }
    }
  if (length(reliability) > 1) {
    if (length(reliability) != 3) {
      stop("reliability must be one or three values")
    }
    if (!setequal(
          names(reliability),
          c("x", "m", "y"))) {
      stop("Names of reliability must 'x', 'm', and 'y'")
    }
  }

  # ==== Call power4test() =====

  p4t_args <- list(
                nrep = nrep,
                model = model,
                pop_es = model_es,
                n = n,
                number_of_indicators = number_of_indicators,
                reliability = reliability,
                R = R,
                ci_type = ci_type,
                test_fun = test_indirect_effect,
                test_args = list(x = "x",
                                m = "m",
                                y = "y",
                                mc_ci = isTRUE(ci_type == "mc"),
                                boot_ci = isTRUE(ci_type == "boot")
                                ),
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

  out2
}

#' @noRd

q_power_region_mediation_parallel <- function(
    a = "m",
    bs = "m",
    cp = "n",
    number_of_indicators = NULL,
    reliability = NULL,
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
    ...
) {
  ci_type <- match.arg(ci_type)

  ddd <- list(...)

  power4test_args <- names(formals(power4test))
  ddd_power4test <- ddd[names(ddd) %in% power4test_args]

  n_region_from_power_args <- names(formals(n_region_from_power))
  ddd_n_from_region <- ddd[names(ddd) %in% n_region_from_power_args]


  # ==== Setup the model =====

  model <- paste0(
            c("m ~ x",
              "y ~ m + x",
              ""),
            collapse = "\n"
          )
  model_es <-
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

  if (length(number_of_indicators) > 1) {
      if (length(number_of_indicators) != 3) {
        stop("number_of_indicators must be one or three values")
      }
      if (!setequal(
            names(number_of_indicators),
            c("x", "m", "y"))) {
        stop("Names of number_of_indicators must 'x', 'm', and 'y'")
      }
    }
  if (length(reliability) > 1) {
    if (length(reliability) != 3) {
      stop("reliability must be one or three values")
    }
    if (!setequal(
          names(reliability),
          c("x", "m", "y"))) {
      stop("Names of reliability must 'x', 'm', and 'y'")
    }
  }

  # ==== Call power4test() =====

  p4t_args <- list(
                nrep = nrep,
                model = model,
                pop_es = model_es,
                n = n,
                number_of_indicators = number_of_indicators,
                reliability = reliability,
                R = R,
                ci_type = ci_type,
                test_fun = test_indirect_effect,
                test_args = list(x = "x",
                                m = "m",
                                y = "y",
                                mc_ci = isTRUE(ci_type == "mc"),
                                boot_ci = isTRUE(ci_type == "boot")
                                ),
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

  out2
}