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
#' @param group.partial The same argument
#' used by `lavaan`. The parameters that
#' should be free across groups. Used with
#' `group.equal` to exclude some parameters
#' from those requested to be equal
#' across groups by `group.equal`.
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
#' m ~ x: c(m, s)
#' y ~ x: n
#' "
#'
#' # Simulate the data
#'
#' sim_only <- power4test(nrep = 2,
#'                        model = mod,
#'                        pop_es = mod_es,
#'                        n = 100,
#'                        do_the_test = FALSE,
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
#' @noRd
test_group_equal <- function(fit = fit,
                             group.equal = NULL,
                             group.partial = NULL,
                             check_post_check = TRUE,
                             ...,
                             fit_name = "fit",
                             get_map_names = FALSE,
                             get_test_name = FALSE) {
  map_names <- c(fit = fit_name)
  args <- list(...)
  if (get_map_names) {

    # ==== Return map_names ====

    return(map_names)
  }
  if (get_test_name) {

    # ==== Return test_name ====

    return("test_group_equal")

  }

  # ==== Prepare test_label ====

  tmp <- character(0)
  if (!is.null(group.equal)) {
    tmp <- c(paste0("equal(",
                    paste0(group.equal, collapse = ","),
                    ")"))
  }
  if (!is.null(group.partial)) {
    tmp <- c(tmp,
             c(paste0("partial(",
                    paste0(group.partial, collapse = ","),
                    ")")))
  }
  tmp <- paste0(tmp,
                collapse = ";")

  test_label <- tmp

  # ==== Check the type of fit ====

  if (inherits(fit, "lavaan")) {
    fit_type <- "lavaan"
  } else {
    stop("fit is not a supported object.")
  }

  ngroups <- lavaan::lavInspect(fit,
                                "ngroups")
  if (ngroups == 1) {
    stop("Only support multigroup models.")
  }

  # ==== Is the lavaan fit OK? ====

  fit_ok <- lavaan::lavInspect(fit, "converged") &&
            (suppressWarnings(lavaan::lavInspect(fit, "post.check") ||
              !check_post_check))

  # ==== Refit the model ====

  if (fit_ok) {
    suppressWarnings(
      fit_update <- methods::getMethod("update",
                  signature = "lavaan",
                  where = asNamespace("lavaan"))(
                    fit,
                    group.equal = group.equal,
                    group.partial = group.partial
                  )
    )
    update_ok <- lavaan::lavInspect(fit_update, "converged") &&
              (suppressWarnings(lavaan::lavInspect(fit_update, "post.check") ||
                !check_post_check))
  } else {
    update_ok <- FALSE
  }

  # ==== Fit not OK. Return NAs ====

  if (!update_ok) {
    out1 <- data.frame(test_label = test_label,
                       est = NA,
                       cilo = NA,
                       cihi = NA,
                       sig = NA,
                       pvalue = NA)
    return(out1)
  }

  # ==== Compare the models ====

  out0 <- lavaan::lavTestLRT(
              fit,
              fit_update,
              ...
            )

  out1 <- data.frame(test_label = test_label,
                     est = out0[2, "Chisq diff"],
                     cilo = NA,
                     cihi = NA,
                     sig = NA,
                     pvalue = out0[2, "Pr(>Chisq)"])

  tmp_level <- args$level %||% .95
  out1$sig <- ifelse(out1$pvalue < (1 - tmp_level),
                     yes = 1,
                     no = 0)

  # ==== Prepare the output ====

  attr(out1, "test_label") <- "test_label"
  return(out1)

}
