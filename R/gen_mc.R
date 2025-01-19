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
#' @export
#'
gen_mc_i <- function(fit_i,
                     R = 100,
                     seed = NULL,
                     ...) {
  mc_out <- manymome::do_mc(fit = fit_i,
                            R = 100,
                            ...,
                            parallel = FALSE,
                            progress = FALSE)
  mc_out
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
                seed = seed,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  return(out)
}
