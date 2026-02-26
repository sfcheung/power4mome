#' @title Test a Moderated Mediation Effect
#'
#' @description Test a moderated
#' mediation effect for a `power4test`
#' object.
#'
#' @details
#' This function is to be used in
#' [power4test()] for testing a
#' moderated mediation effect, by
#' setting it to the `test_fun`
#' argument.
#'
#' It uses [manymome::index_of_mome()]
#' to do the test. It can be used on
#' models fitted by [lavaan::sem()]
#' or fitted by a sequence of calls
#' to [stats::lm()], although only
#' nonparametric bootstrap confidence
#' interval is supported for models
#' fitted by regression using
#' [stats::lm()].
#'
#' @return
#' In its normal usage, it returns
#' a named numeric vector with the
#' following elements:
#'
#' - `est`: The mean of the estimated
#'  indirect effect across datasets.
#'
#' - `cilo` and `cihi`: The means of the
#'  lower and upper limits of the
#'  confidence interval (95% by
#'  default), respectively.
#'
#' - `sig`: Whether a test by confidence
#'  interval is significant (`1`) or
#'  not significant (`0`).
#'
#' @inheritParams test_indirect_effect
#'
#' @param fit The fit object, to be
#' passed to [manymome::index_of_mome()].
#'
#' @param w The name of the moderator.
#'
#' @param ... Additional arguments to
#' be passed to [manymome::index_of_mome()].
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#' # Specify the model
#'
#' mod <-
#' "
#' m ~ x + w + x:w
#' y ~ m
#' "
#'
#' # Specify the population values
#'
#' mod_es <-
#' "
#' m ~ x: n
#' y ~ x: m
#' m ~ w: l
#' m ~ x:w: l
#' "
#'
#' # Simulate the data
#'
#' sim_only <- power4test(nrep = 2,
#'                        model = mod,
#'                        pop_es = mod_es,
#'                        n = 100,
#'                        R = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' # Do the test in each replication
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_index_of_mome,
#'                        test_args = list(x = "x",
#'                                         m = "m",
#'                                         y = "y",
#'                                         w = "w",
#'                                         mc_ci = TRUE))
#'
#' print(test_out,
#'       test_long = TRUE)
#'
#' @export
test_index_of_mome <- function(fit = fit,
                      x = NULL,
                      m = NULL,
                      y = NULL,
                      w = NULL,
                      mc_ci = TRUE,
                      mc_out = NULL,
                      boot_ci = FALSE,
                      boot_out = NULL,
                      check_post_check = TRUE,
                      test_method = NULL,
                      ...,
                      fit_name = "fit",
                      get_map_names = FALSE,
                      get_test_name = FALSE) {

  # ==== Enable pvalue? ====

  args <- list(...)
  if (!is.null(mc_out)) {
    R <- length(mc_out)
  } else if (!is.null(boot_out)) {
    R <- length(boot_out)
  } else {
    R <- NULL
  }
  R <- args$R %||% formals(manymome::index_of_mome)$R
  ci_level <- args$level %||% formals(manymome::index_of_mome)$level
  R_bz_ok <- isTRUE(R %in% R_extrapolate(alpha = 1 - ci_level))
  bz_not_FALSE <- !isFALSE(options("power4mome.bz"))
  test_method_NULL <- is.null(test_method)
  if (R_bz_ok &&
      bz_not_FALSE &&
      test_method_NULL) {
    test_method <- "pvalue"
  } else {
    test_method <- match.arg(test_method,
                             c("ci", "pvalue"))
  }

  internal_options <- list()
  if (test_method == "pvalue") {
    internal_options <- utils::modifyList(internal_options,
                                          list(skip_ci = TRUE,
                                               pvalue_min_size = -Inf))
  }
  if (test_method == "ci") {
    internal_options <- utils::modifyList(internal_options,
                                          list(skip_ci = FALSE))
  }
  if (fit_name != "fit") {
    mc_name <- paste0(fit_name, "_mc_out")
    boot_name <- paste0(fit_name, "_boot_out")
  } else {
    mc_name <- "mc_out"
    boot_name <- "boot_out"
  }
  map_names <- c(fit = fit_name,
                 mc_out = mc_name,
                 boot_out = boot_name)
  if (get_map_names) {
    return(map_names)
  }
  if (get_test_name) {
    tmp <- paste0(c(x, m, y),
                  collapse = "->")
    tmp <- paste0(tmp,
                  ", moderated by ",
                  w)
    return(paste0("test_index_of_mome: ", tmp, collapse = ""))
  }
  if (boot_ci) mc_ci <- FALSE
  if (inherits(fit, "lavaan")) {
    fit_ok <- lavaan::lavInspect(fit, "converged") &&
              (suppressWarnings(lavaan::lavInspect(fit, "post.check") ||
               !check_post_check))
  } else {
    fit_ok <- TRUE
  }
  if (fit_ok) {
    out <- tryCatch(manymome::index_of_mome(
                                 x = x,
                                 y = y,
                                 m = m,
                                 w = w,
                                 fit = fit,
                                 mc_ci = mc_ci,
                                 mc_out = mc_out,
                                 boot_ci = boot_ci,
                                 boot_out = boot_out,
                                 progress = FALSE,
                                 ...,
                                 internal_options = internal_options),
                   error = function(e) e)
  } else {
    out <- NA
  }
  if (inherits(out, "error") ||
      identical(out, NA)) {
    out2 <- c(est = as.numeric(NA),
              cilo = as.numeric(NA),
              cihi = as.numeric(NA),
              sig = as.numeric(NA),
              pvalue = as.numeric(NA))
    return(out2)
  }
  if (test_method == "ci") {
    ci0 <- stats::confint(out)
    out1 <- ifelse((ci0[1, 1] > 0) || (ci0[1, 2] < 0),
                    yes = 1,
                    no = 0)
  }
  if (test_method == "pvalue") {
    ci0 <- matrix(
              as.numeric(NA),
              nrow = 1,
              ncol = 2
            )
    out1 <- ifelse(
                out$pvalue < (1 - out$level),
                yes = 1,
                no = 0
              )
    bz_alpha_ok <- isTRUE(all.equal(1 - out$level,
                                    getOption("power4mome.bz.alpha",
                                                        default = .05)))
    if (out$ci_type %in% c("mc", "boot")) {
      diff_name <- switch(out$ci_type,
                          mc = "mc_diff",
                          boot = "boot_diff")
      est_diff <- out[[diff_name]]
      if (bz_alpha_ok) {
        est_sig <- bz_sig_partition(
                      est_diff,
                      alpha = 1 - out$level
                    )
      } else {
        est_sig <- as.numeric()
      }
      R <- length(est_diff)
      nlt0 <- sum(as.numeric(est_diff < 0))
    } else {
      R <- as.numeric(NA)
      nlt0 <- as.numeric(NA)
      est_sig <- as.numeric()
    }
  }
  out2 <- c(est = unname(stats::coef(out)),
            cilo = ci0[1, 1],
            cihi = ci0[1, 2],
            sig = out1,
            pvalue = out$pvalue %||% as.numeric(NA))
  if (test_method == "pvalue") {
    # For Boos & Zhang (2000)
    out2 <- c(out2, R = R, nlt0 = nlt0,
              alpha = 1 - out$level,
              est_sig)
  }
  return(out2)
}
