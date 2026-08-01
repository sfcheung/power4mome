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
#' @param group.equal The same argument
#' used by `lavaan`. A character vector
#' with one or more of these values:
#' `"regressions"`, `"loadings"`,
#' `"lv.covariances"`, `"lv.variances"`,
#' `"intercepts"`, `"means"`, `"thresholds"`,
#' `"residual.covariances"`, `"composite.weights"`,
#' and `"residuals"`.
#'
#' @param model_to_fit The model to be
#' fitted, specified by `lavaan` model
#' syntax. If `NULL`, then the model
#' used by `fit_model()` will be used.
#'
#' @param ... Optional arguments to be
#' passed to [lavaan::lavTestLRT()].
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
  refit_arguments = list(se = "none"),
  ...,
  fit_name = "fit",
  get_map_names = FALSE,
  get_test_name = FALSE
) {
  map_names <- c(fit = fit_name)
  args <- list(...)
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
      refit_arguments,
      keep.null = TRUE
    )
    suppressWarnings(
      fit_out <- do.call(
        lavaan::lavaan,
        refit_args
      )
    )

    # suppressWarnings(
    #   fit_update <- methods::getMethod("update",
    #               signature = "lavaan",
    #               where = asNamespace("lavaan"))(
    #                 fit,
    #                 group.equal = group.equal,
    #                 group.partial = group.partial
    #               )
    # )
    # update_ok <- lavaan::lavInspect(fit_update, "converged") &&
    #           (suppressWarnings(lavaan::lavInspect(fit_update, "post.check") ||
    #             !check_post_check))
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

  fm_out <- suppressWarnings(
    tryCatch(lavaan::fitMeasures(
        fit_out
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
