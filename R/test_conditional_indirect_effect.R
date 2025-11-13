#' @title Test a Conditional Indirect Effect
#'
#' @description Test a conditional
#' indirect effect
#' for a `power4test` object.
#'
#' @details
#' This function is to be used in
#' [power4test()] for testing a
#' conditional
#' indirect effect, by setting it
#' to the `test_fun` argument.
#'
#' It uses [manymome::cond_indirect()]
#' to do the test. It can be used on
#' models fitted by [lavaan::sem()]
#' or fitted by a sequence of calls
#' to [stats::lm()], although only
#' nonparametric bootstrap confidence
#' interval is supported for models
#' fitted by regression using
#' [stats::lm()].
#'
#' It can also be used to test
#' a conditional effect on a direct path
#' with no mediator. Just omit `m` when
#' calling the function.
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
#' passed to [manymome::cond_indirect()].
#'
# @param x <- Inherited
#'
# @param m <- Inherited
#'
# @param y <- Inherited
#'
#' @param wvalues A numeric vector of
#' named elements. The names are the
#' variable names of the moderators,
#' and the values are the values to
#' which the moderators will be set to.
#' Default is `NULL`.
#'
# @param mc_ci <- Inherited
#'
# @param mc_out <- Inherited
#'
# @param boot_ci <- Inherited
#'
# @param boot_out <- Inherited
#'
#' @param ... Additional arguments to
#' be passed to [manymome::cond_indirect()].
#'
# @param fit_name <- Inherited
#'
# @param get_map_names <- Inherited
#'
# @param get_test_name <- Inherited
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#' # Specify the model
#'
#' model_simple_mod <-
#' "
#' m ~ x + w + x:w
#' y ~ m + x
#' "
#'
#' # Specify the population values
#'
#' model_simple_mod_es <-
#' "
#' y ~ m: l
#' y ~ x: n
#' m ~ x: m
#' m ~ w: n
#' m ~ x:w: l
#' "
#'
#' # Simulate the data
#'
#' sim_only <- power4test(nrep = 5,
#'                        model = model_simple_mod,
#'                        pop_es = model_simple_mod_es,
#'                        n = 100,
#'                        R = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' # Do the test in each replication
#'
#' test_ind <- power4test(object = sim_only,
#'                        test_fun = test_cond_indirect,
#'                        test_args = list(x = "x",
#'                                         m = "m",
#'                                         y = "y",
#'                                         wvalues = c(w = 1),
#'                                         mc_ci = TRUE))
#' print(test_ind,
#'       test_long = TRUE)
#'
#' @export

test_cond_indirect <- function(fit = fit,
                               x = NULL,
                               m = NULL,
                               y = NULL,
                               wvalues = NULL,
                               mc_ci = TRUE,
                               mc_out = NULL,
                               boot_ci = FALSE,
                               boot_out = NULL,
                               check_post_check = TRUE,
                               test_method = c("ci", "pvalue"),
                               ...,
                               fit_name = "fit",
                               get_map_names = FALSE,
                               get_test_name = FALSE) {
  test_method <- match.arg(test_method)
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
    tmp_w <- cond_name(wvalues)
    tmp <- paste0(c(x, m, y),
                  collapse = "->")
    tmp <- paste0(tmp,
                  " (",
                  tmp_w,
                  ")")
    args <- as.list(match.call())
    tmp2 <- character(0)
    if (isTRUE(args$standardized_x) && !isTRUE(args$standardized_y)) {
      tmp <- paste0(tmp, " ('x' standardized)")
    }
    if (!isTRUE(args$standardized_x) && isTRUE(args$standardized_y)) {
      tmp <- paste0(tmp, " ('y' standardized)")
    }
    if (isTRUE(args$standardized_x) && isTRUE(args$standardized_y)) {
      tmp <- paste0(tmp, " ('x' and 'y' standardized)")
    }
    return(paste0("test_cond_indirect: ", tmp, collapse = ""))
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
    out <- tryCatch(manymome::cond_indirect(
                                 x = x,
                                 y = y,
                                 m = m,
                                 wvalues = wvalues,
                                 fit = fit,
                                 mc_ci = mc_ci,
                                 mc_out = mc_out,
                                 boot_ci = boot_ci,
                                 boot_out = boot_out,
                                 progress = FALSE,
                                 ...),
                    error = function(e) e)
  } else {
    out <- NA
  }
  if (inherits(out, "error") ||
      identical(out, NA)) {
    out2 <- c(est = as.numeric(NA),
              cilo = as.numeric(NA),
              cihi = as.numeric(NA),
              sig = as.numeric(NA))
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
    boot_est <- out$boot_indirect %||% out$mc_indirect
    boot_sig <- bz_sig_partition(
                  boot_est,
                  alpha = 1 - out$level
                )
    if (isTRUE(boot_ci)) {
      out1a <- out$boot_p %||% as.numeric(NA)
      R <- length(out$boot_indirect)
      nlt0 <- sum(as.numeric(out$boot_indirect < 0))
    } else if (isTRUE(mc_ci)) {
      out1a <- out$mc_p %||% as.numeric(NA)
      R <- length(out$mc_indirect)
      nlt0 <- sum(as.numeric(out$mc_indirect < 0))
    } else {
      out1a <- as.numeric(NA)
      R <- as.numeric(NA)
      nlt0 <- as.numeric(NA)
    }
    out1 <- ifelse(
                out1a < (1 - out$level),
                yes = 1,
                no = 0
              )
  }
  out2 <- c(est = unname(stats::coef(out)),
            cilo = ci0[1, 1],
            cihi = ci0[1, 2],
            sig = out1)
  if (test_method == "pvalue") {
    # For Boos & Zhang (2000)
    out2 <- c(out2, R = R, nlt0 = nlt0,
              boot_sig)
  }
  return(out2)
}

#' @noRd

cond_name <- function(wvalues) {
  w1 <- names(wvalues)
  out <- paste0(w1, " = ", wvalues)
  out <- paste(out,
               collapse = "; ")
  return(out)
}