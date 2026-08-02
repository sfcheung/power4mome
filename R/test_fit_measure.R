#' @title Test Model Fit
#'
#' @description "Test" the model fit
#' of a model.
#'
#' @details
#' This function is to be used in
#' [power4test()] for "testing"
#' the model fit of a model
#' , by
#' setting it to the `test_fun`
#' argument.
#'
#' # What "Test" Means for This Function
#'
#' The term "test" is used in this function
#' merely to be consistent with other
#' test functions. What this function does
#' is to check whether a certain numeric
#' criterion based on a fit measure is met.
#'
#' If the fit measure is the model chi-square,
#' then this is a test in the conventional
#' sense, using the *p*-value of the chi-square.
#' Similarly, the test of close fit using
#' RMSEA is also a test.
#'
#' However, the fit measure can also be
#' a descriptive measure such as CFI or TLI.
#' A descriptive measure is not used to "test"
#' the goodness of fit of a model. Nevertheless,
#' we can still estimate the probability
#' that this measure meets a criterion
#' (e.g., CFI less than .90). The function
#' [test_fit_measure()] can be used for
#' this purpose. The empirical "rejection rate" is
#' then the proportion of replications
#' with this fit measure meeting the criterion.
#'
#' # Typical Scenarios
#'
#' ## The Fit Measure "Test" for the Fitted Model
#'
#' When used with [power4test()], this function
#' can be used to estimate the "rejection rate"
#' of a criterion (e.g., the *p*-value of the
#' model chi-square less than .05, or the CFI
#' less than .90) for the model fitted when
#' calling [power4test()].
#'
#' ## The Fit Measure "Test" for an Alternative Model
#'
#' This function can also be used to estimate
#' the "rejection rate" of a criterion when
#' a model different from the stored fitted model.
#' For example, the data generation model is
#' a simple mediation model with a non-nil direct path
#' from the independent variable to the outcome.
#' However, we want to estimate the rejection
#' rate using "CFI<.90" when a complete mediation
#' model (the direct path is fixed to zero) is
#' fitted. This can be done by setting this
#' model to the argument `model_to_fit`.
#'
#' @return
#' In its normal usage, it returns
#' a one-row data frame with the
#' following columns:
#'
#' - `est`: The fit measure used for the
#'   "test", such as the model chi-square
#'   or the CFI.
#'
#' - `cilo` and `cihi`: `NA`. Not used.
#'
#' - `sig`: Whether the "test" is significant.
#'   That is, whether the criterion
#'   is met (e.g., the *p*-value of the model
#'   chi-square is less than .05, or the
#'   CFI is less than .90).
#'
#' - `test_label`: An automatically generated
#'   label for the test.
#'
#' @inheritParams test_k_indirect_effects
#'
#' @inheritParams test_indirect_effect
#'
#' @param fit The fit object. Must be
#' the output of [lavaan::lavaan()] or
#' its wrappers, such as [lavaan::sem()]
#' and [lavaan::cfa()].
#'
#' @param model_to_fit The model to be
#' fitted, specified by `lavaan` model
#' syntax. The full model must be specified
#' if it is a model with indicators.
#' It can also be a parameter
#' table. If `NULL`, then the model
#' used by `fit_model()` will be used.
#'
#' @param fit_measure The name of the
#' fit measure to be used to do the test.
#' Must be a name that can be accepted by
#' [lavaan::fitMeasures()]. It can also
#' be the test statistic, such as the model
#' chi-square.
#'
#' @param sig_value The name of the element
#' of the output of [lavaan::fitMeasures()]
#' to be used to do the test.
#' Must be a name that can be accepted by
#' [lavaan::fitMeasures()]. Used when
#' the value used to do the test (e.g., a p-value)
#' is different from the value specified
#' in `fit_measure` (e.g., `"pvalue"` is
#' the p-value for model chi-square, `"chisq"`).
#' It can be
#' a named character vector, with the names
#' being possible values for `fit_measure`.
#' The name to be used will then be retrieved
#' based on `fit_measure`.
#'
#' @param sig_if The criterion for doing
#' the test, as a character string.
#' For example, it is `"<.05"` if the test
#' is "significant" when the p-value is less
#' than .05. It is `"<.95"` if the "test"
#' is "significant" when the fit measure
#' (e.g., CFI) is less than .95. It must
#' be a string that, if appended to the
#' value retrieved by `sig_value`, is
#' an expression that can be evaluated
#' in R (e.g., ".023<.05").
#'
#' @param refit_args A named list
#' of arguments to be passed to [fit_model()]
#' if `model_to_fit` is set. If `model_to_fit`
#' is `NULL` and `always_refit` is `TRUE`,
#' this list of argument values will
#' be used when calling [lavaan::sem()],
#' overriding values stored, if any.
#'
#' @param always_refit Whether the model
#' will always be fitted again. If `TRUE`
#' and `model_to_fit` is `NULL`, then the
#' stored model will be fitted again,
#' but with `refit_args` used. Ignored
#' if `model_to_fit` is explicitly set to
#' a `lavaan` model because this model
#' will always be fitted to the data. Useful
#' when the same stored model is fitted
#' but with different argument values.
#'
#' @param fitmeasures_args A named list
#' of arguments to be passed to [lavaan::fitMeasures()].
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#' # Specify the model
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
#' y ~ m: l
#' m ~ x: s
#' y ~ x: m
#' "
#'
#' # Simulate the data
#'
#' sim_only <- power4test(
#'   nrep = 2,
#'   model = mod,
#'   pop_es = mod_es,
#'   n = 100,
#'   iseed = 1234
#' )
#'
#' # Do the tests in each replication
#'
#' mod_complete <-
#' "
#' m ~ x
#' y ~ m
#' "
#'
#' test_out <- power4test(
#'   object = sim_only,
#'   test_fun = test_fit_measure,
#'   test_args = list(
#'     model_to_fit = mod_complete,
#'     fit_measure = "cfi",
#'     sig_if = "<.90"
#'   )
#' )
#'
#' print(test_out,
#'       test_long = TRUE)
#'
#'
#' @export
test_fit_measure <- function(
  fit = fit,
  model_to_fit = NULL,
  fit_measure = "chisq",
  sig_value = c(
    "chisq" = "pvalue",
    "chisq.scaled" = "pvalue.scaled",
    "rmsea" = "rmsea.pvalue",
    "rmsea.scaled" = "rmsea.pvalue.scaled",
    "rmsea.robust" = "rmsea.pvalue.robust"
  ),
  sig_if = "<.05",
  check_post_check = TRUE,
  refit_args = list(se = "none"),
  always_refit = FALSE,
  fitmeasures_args = list(),
  fit_name = "fit",
  get_map_names = FALSE,
  get_test_name = FALSE
) {
  map_names <- c(fit = fit_name)

  if (length(fit_measure) != 1) {
    stop("'fit_measure' must has exactly one string.")
  }

  if (get_map_names) {

    # ==== Return map_names ====

    return(map_names)
  }
  if (get_test_name) {

    # ==== Return test_name ====

    return("test_fit_measure")

  }

  # ==== Prepare test_label ====

  if (length(sig_value) == 1) {
    # sig_value set by user
    sig_value_name <- unname(sig_value)
  } else {
    sig_value_name <- unname(sig_value[fit_measure])
    if (is.na(sig_value_name)) {
      sig_value_name <- fit_measure
    }
  }

  tmp <- paste0(fit_measure,
                "(",
                sig_value_name,
                sig_if,
                ")")

  test_label <- tmp

  # ==== Check the type of fit ====

  if (inherits(fit, "lavaan")) {
    fit_type <- "lavaan"
  } else {
    stop("fit is not a supported object.")
  }

  if (is.character(model_to_fit) ||
      is.data.frame(model_to_fit)) {
    # TODO:
    # - Allow users to specify only the structural model
    # Always refit
    always_refit <- TRUE
  } else {
    # Use stored model
    model_to_fit <- lavaan::parameterTable(fit)
  }

  # ==== Refit? ====

  if (always_refit) {
    # TDDO:
    # - Check if it is a parameter table

    slot_opt <- fit@Options
    slot_dat <- fit@Data
    refit_args <- list(
      model = model_to_fit,
      slotOptions = slot_opt,
      slotData = slot_dat
    )
    refit_args <- utils::modifyList(
      refit_args,
      refit_args,
      keep.null = TRUE
    )
    suppressWarnings(
      fit_out <- do.call(
        lavaan::sem,
        refit_args
      )
    )
  } else {
    fit_out <- fit
  }

  # ==== Is the lavaan fit OK? ====

  fit_ok <- lavaan::lavInspect(fit_out, "converged") &&
            (suppressWarnings(lavaan::lavInspect(fit_out, "post.check") ||
              !check_post_check))

  out_error <- data.frame(
    test_label = test_label,
    est = NA,
    cilo = NA,
    cihi = NA,
    sig = NA,
    pvalue = NA
  )

  # ==== Fit not OK. Return NAs ====

  if (!fit_ok) {
    return(out_error)
  }

  # ==== Do the Test ====

  fitmeasures_args0 <- utils::modifyList(
    fitmeasures_args,
    list(
      fit_measures = unique(c(fit_measure, sig_value_name)),
      object = fit_out
    )
  )
  fm_out <- suppressWarnings(
    tryCatch(do.call(
        lavaan::fitMeasures,
        fitmeasures_args0
      ),
    error = function(e) e)
  )
  if (inherits(fm_out, "error")) {
    return(out_error)
  }

  fm0 <- unname(fm_out[fit_measure])
  sig0 <- unname(fm_out[sig_value_name])

  if (is.na(sig0)) {
    # TODO:
    # - Check. NA is meaningful for some fit measures
    return(out_error)
  }

  tmp <- paste(
            sig0,
            sig_if
          )
  sig_out <- eval(parse(text = tmp))

  out1 <- data.frame(
    test_label = test_label,
    est = fm0,
    cilo = NA,
    cihi = NA,
    sig = as.numeric(sig_out),
    pvalue = NA
  )

  if (grepl("pvalue", sig_value_name, fixed = TRUE)) {
    out1$pvalue <- sig0
  }

  # ==== Prepare the output ====

  attr(out1, "test_label") <- "test_label"
  return(out1)

}
