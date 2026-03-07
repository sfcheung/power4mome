#' @title Helpers for the Boos-and-Zhang (2000) Method
#'
#' @description Helpers for the method
#' by Boos and Zhang (2000) for estimating
#' rejection rate for resampling-based
#' tests.
#'
#' @details Boos and Zhang (2000) proposed
#' a method to estimate the rejection
#' rate (power, if the null hypothesis is false)
#' for methods based on resampling, such
#' as nonparametric bootstrapping. This
#' method is used by some functions
#' in `power4mome`, such as [x_from_power()].
#'
#' This method is implemented internally.
#' Some helper functions regarding this
#' method is exported for users.
#'
#' @references
#' Boos, D. D., & Zhang, J. (2000).
#' Monte Carlo evaluation of
#' resampling-based hypothesis tests.
#' *Journal of the American Statistical Association*,
#' *95*(450), 486--492.
#' \doi{10.1080/01621459.2000.10474226}
#'
#' @seealso [x_from_power()]
#'
#' @name bz_helpers
NULL

#' @details
#' The function [Rs_bz_supported()] returns
#' the number of bootstrap samples (for bootstrapping)
#' or simulated samples (for Monte Carlo),
#' both called resamples below for brevity,
#' usually specified by the argument `R`,
#' that can be
#' used for the Boos-Zhang-2000 method,
#' given the desired two-tailed level of significance,
#' (.05 by default).
#' If possible, setting the number of resamples.
#' (e.g., setting `R` when calling [x_from_power()])
#' will automatically enable the Boos-Zhang-2000
#' method (unless explicitly turned off
#' by setting the option `"power4mome.bz"`
#' to `FALSE` by `options("power4mome.bz") <- FALSE`),
#' substantially reducing the processing
#' time. For now, only two-tailed tests are supported.
#'
#' @return
#' The function [Rs_bz_supported()] returns
#' a numeric vector of the numbers of
#' resamples supported.
#'
#' @param alpha The level of significance,
#' two-tailed.
#'
#' @param Rmax The maximum number of resamples
#' to be returned. Default is 359.
#' Though it is possible to use 1999
#' resamples or even more with Boos-Zhang-2000,
#' using such a large number of resamples defeats
#' the goal to reduce processing time.
#'
#'
#' @examples
#' # === Rs_bz_supported ===
#'
#' # alpha = .05
#' Rs_bz_supported()
#'
#' # alpha = .01
#' Rs_bz_supported(alpha = .01)
#'
#' @rdname bz_helpers
#' @export
Rs_bz_supported <- function(
  alpha = .05,
  Rmax = getOption(
            "power4mome.bz_Rmax",
            default = 359)
) {
  R_extrapolate(alpha = alpha,
                Rmax = Rmax)
}

#' @details
#' Given a target maximum number of
#' resamples and a level of significance,
#' the function [R_for_bz()] returns
#' largest number of resamples that can be
#' used for the Boos-Zhang-2000 method.
#' This function can be used for
#' arguments such as `R` in [x_from_power()]
#' to automatically find the largest
#' value supported by the Boos-Zhang-2000
#' method.
#'
#' @param R_target The target maximum
#' number of resamples.
#'
#' @return
#' The function [R_for_bz()] returns
#' a scalar.
#'
#' @examples
#' # === R_for_bz ===
#'
#' R_for_bz(200)
#' R_for_bz(500)
#'
#' @rdname bz_helpers
#' @export
R_for_bz <- function(
  R_target,
  alpha = .05
) {
  Rs <- R_extrapolate(
          alpha = alpha,
          Rmax = R_target
        )
  if (length(Rs) > 0) {
    return(max(Rs))
  } else {
    stop("No supported values")
  }
}
