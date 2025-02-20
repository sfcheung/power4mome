#' @title Generate Monte Carlo Estimates
#'
#' @description Get a list of the output
#' of [lavaan::sem()] and generate
#' Monte Carlo estimates of model
#' parameters.
#'
#' @details
#' It simply calls [manymome::do_mc()]
#' on each output of
#' [lavaan::sem()] in `fit_all`. The
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
#' to generate the Monte Carlo estimates
#' for each fit output.
#'
#' @param ... Optional arguments to be
#' passed to [manymome::do_mc()] when
#' generating the Monte Carlo estimates.
#'
#' @param iseed The seed for the random
#' number generator. Default is `NULL`
#' and the seed is not changed.
#'
#' @param parallel If `TRUE`, parallel
#' processing will be used to generate
#' Monte Carlo estimates for the
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
#' A `mc_list` object, which is a list
#' of the output of [manymome::do_mc()].
#'
#' @examples
#' mod <-
#' "m ~ x
#'  y ~ m + x"
#' es <-
#' c("y ~ m" = "m",
#'   "m ~ x" = "m",
#'   "y ~ x" = "n")
#' data_all <- sim_data(nrep = 5,
#'                      model = mod,
#'                      pop_es = es,
#'                      n = 100,
#'                      iseed = 1234)
#'
#' fit_all <- fit_model(data_all)
#' mc_all <- gen_mc(fit_all,
#'                  R = 100,
#'                  iseed = 4567)
#'
#'
#' @export
#'
gen_mc <- function(fit_all,
                   R = 100,
                   ...,
                   iseed = NULL,
                   parallel = FALSE,
                   progress = FALSE,
                   ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = fit_all,
                FUN = gen_mc_i,
                R = R,
                ...,
                iseed = iseed,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  class(out) <- c("mc_list", class(out))
  return(out)
}

#' @title Title In Title Case
#'
#' @description One paragraph description.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @examples
#' \donttest{
#' }
#'
#' @noRd
gen_mc_i <- function(fit_i,
                     R = 100,
                     seed = NULL,
                     ...) {
  if (inherits(fit_i, "lavaan")) {
    mc_out <- tryCatch(manymome::do_mc(fit = fit_i,
                                       R = R,
                                       ...,
                                       parallel = FALSE,
                                       progress = FALSE),
                       error = function(e) e)
  } else {
    mc_out <- tryCatch(stop("The fit is not a lavaan object, ",
                            "probably an error in model fitting."),
                       error = function(e) e)
  }
  mc_out
}
