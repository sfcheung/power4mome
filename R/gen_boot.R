#' @title Generate Bootstrap Estimates
#'
#' @description Get a list of the output
#' of [lavaan::sem()] or [lmhelprs::many_lm()]
#' and generate
#' bootstrap estimates of model
#' parameters.
#'
#' @details
#' It simply calls [manymome::do_boot()]
#' on each output of
#' [lavaan::sem()] or [lmhelprs::many_lm()]
#' in `fit_all`. The
#' simulated
#' estimates can then be used to test
#' effects such as indirect effects,
#' usually by functions from the
#' `manymome` package, such as
#' [manymome::indirect_effect()].
#'
#' This function is used by the
#' all-in-one function [power4test()].
#' Users usually do not call this
#' function directly.
#'
#' @seealso [power4test()]
#'
#' @param fit_all The output of
#' [fit_model()] or an object of the
#' class `fit_out`.
#'
#' @param R The number of replications
#' to generate the bootstrap estimates
#' for each fit output.
#'
#' @param ... Optional arguments to be
#' passed to [manymome::do_boot()] when
#' generating the bootstrap estimates.
#'
#' @param iseed The seed for the random
#' number generator. Default is `NULL`
#' and the seed is not changed.
#'
#' @param parallel If `TRUE`, parallel
#' processing will be used to generate
#' bootstrap estimates for the
#' fit outputs. Default is `FALSE`.
#'
#' @param progress If `TRUE`, the progress
#' will be displayed.
#' Default is `FALSE.
#'
#' @param ncores The number of CPU
#' cores to use if parallel processing
#' is used.
#'
#' @return
#' A `boot_list` object, which is a list
#' of the output of [manymome::do_boot()].
#'
#' @examples
#' mod <-
#' "m ~ x
#'  y ~ m + x"
#' es <-
#' c("y ~ m" = "m",
#'   "m ~ x" = "m",
#'   "y ~ x" = "n")
#' data_all <- sim_data(nrep = 2,
#'                      model = mod,
#'                      pop_es = es,
#'                      n = 50,
#'                      iseed = 1234)
#'
#' fit_all <- fit_model(data_all)
#' boot_all <- gen_boot(fit_all,
#'                      R = 10,
#'                      iseed = 4567)
#' boot_all
#'
#'
#' @export
#'
gen_boot <- function(fit_all,
                     R = 100,
                     ...,
                     iseed = NULL,
                     parallel = FALSE,
                     progress = FALSE,
                     ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = fit_all,
                FUN = gen_boot_i,
                R = R,
                ...,
                iseed = iseed,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  class(out) <- c("boot_list", class(out))
  return(out)
}

#' @noRd
gen_boot_i <- function(fit_i,
                       R = 100,
                       seed = NULL,
                       ...) {
  boot_out <- tryCatch(manymome::do_boot(fit = fit_i,
                                         R = R,
                                         ...,
                                         parallel = FALSE,
                                         progress = FALSE),
                      error = function(e) e)
  boot_out
}
