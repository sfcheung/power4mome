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
                      ...,
                      fit_name = "fit",
                      get_map_names = FALSE,
                      get_test_name = FALSE) {
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
    fit_ok <- lavaan::lavInspect(fit, "converged")
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
                                 ...),
                   error = function(e) e)
  } else {
    out <- NA
  }
  if (inherits(out, "error") ||
      identical(out, NA)) {
    out2 <- c(est = NA,
              cilo = NA,
              cihi = NA,
              sig = NA)
    return(out2)
  }
  ci0 <- stats::confint(out)
  out1 <- ifelse((ci0[1, 1] > 0) || (ci0[1, 2] < 0),
                  yes = 1,
                  no = 0)
  out2 <- c(est = unname(stats::coef(out)),
            cilo = ci0[1, 1],
            cihi = ci0[1, 2],
            sig = out1)
  return(out2)
}
