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
  n_start = 100,
  R = 1000,
  ci_type = c("mc", "boot"),
  seed = NULL,
  iseed = NULL,
  parallel = FALSE,
  progress = TRUE,
  simulation_progress = TRUE,
  max_trials = 10,
  ...
) {
  ci_type <- match.arg(ci_type)
  model <-
  paste0(
  c("m ~ x",
    "y ~ m + x",
    ""),
  collapse = "\n")
  model_es <-
  sprintf(
  paste0(
  c(
  "m ~ x: %1$s",
  "y ~ m: %2$s",
  "y ~ x: %3$s",
  ""
  ),
  collapse = "\n"),
  a, b, cp)
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
  out <- power4test(
            nrep = nrep,
            model = model,
            pop_es = model_es,
            n = n_start,
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
            parallel = progress,
            ncores = max(1, parallel::detectCores(logical = FALSE) - 1)
          )
  out2 <- n_region_from_power(
            out,
            target_power = target_power,
            progress = progress,
            simulation_progress = simulation_progress,
            max_trials = max_trials,
            seed = seed,
            ...
          )
  out2
}