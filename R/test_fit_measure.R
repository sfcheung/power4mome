#' @title Test Group Constraints
#'
#' @description Test the model fit change
#' when one or more between-group
#' constraints are imposed.
#'
#' @details
#' This function is to be used in
#' [power4test()] for testing
#' the difference in model fit
#' when one or more between-group
#' constraints are imposed
#' , by
#' setting it to the `test_fun`
#' argument.
#'
#'
#' @return
#' In its normal usage, it returns
#' a one-row data frame with the
#' following columns:
#'
#' - `est`: The chi-square difference.
#'
#' - `cilo` and `cihi`: `NA`. Not used.
#'
#' - `sig`: Whether the chi-square
#' difference test is significant
#'
#' - `test_label`: The constraints
#'  imposted.
#'
#' @inheritParams test_k_indirect_effects
#'
#' @inheritParams test_indirect_effect
#'
#' @param fit The fit object. Must be
#' the output of [lavaan::lavaan()] or
#' its wrappers, such as [lavaan::sem()]
#' and [lavaan::cfa()]. The model must
#' be a multigroup model.
#'
#' @param model_to_fit The model to be
#' fitted, specified by `lavaan` model
#' syntax. If `NULL`, then the model
#' used by `fit_model()` will be used.
#'
#' @param fit_measure The name of the
#' fit measure to be used to do the test.
#' Must be a name that can be accepted by
#' [lavaan::fitMeasures()].
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
#' (e.g., CFI) is less than .95.
#'
#' @param refit_args A named list
#' of arguments to be passed to [fit_model()]
#' if `model_to_fit` is set.
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
#' m ~ x:
#'   - nil
#'   - s
#' y ~ x: nil
#' "
#'
#' # Simulate the data
#'
#' sim_only <- power4test(nrep = 2,
#'                        model = mod,
#'                        pop_es = mod_es,
#'                        n = 100,
#'                        iseed = 1234)
#'
#' # Do the tests in each replication
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_group_equal,
#'                        test_args = list(group.equal = "regressions"))
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
    "chisq.scale" = "pvalue.scaled",
    "rmsea" = "rmsea.pvalue",
    "rmsea.scaled" = "rmsea.pvalue.scaled",
    "rmsea.robust" = "rmsea.pvalue.robust"
  ),
  sig_if = "<.05",
  check_post_check = TRUE,
  refit_args = list(se = "none"),
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

    # TDDO:
    # - Check if it is a parameter table
    # ==== Refit the model if model_to_fit set ====

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
        lavaan::lavaan,
        refit_args
      )
    )
  } else {
    # ==== No Refit ====
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
