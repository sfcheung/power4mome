#' @title Generate Monte Carlo Estimates
#'
#' @description Get a list of the output
#' of [lavaan::sem()] and generate
#' Monte Carlo estimates of model
#' parameters.
#'
#' @details
#' It simply call [manymome::do_mc()]
#' on each of the fit output of
#' [lavaan::sem()]. The simulated
#' estimates can then be used to test
#' effects such as indirect effects.
#'
#' @return
#' An `mc_list` object, which is a list
#' of the output of [manymome::do_mc()].
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
#'
gen_mc <- function(fit_all,
                   R = 100,
                   seed = NULL,
                   ...,
                   parallel = FALSE,
                   progress = FALSE,
                   ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = fit_all,
                FUN = gen_mc_i,
                ...,
                R = R,
                seed = seed,
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
  mc_out <- manymome::do_mc(fit = fit_i,
                            R = R,
                            ...,
                            parallel = FALSE,
                            progress = FALSE)
  mc_out
}
