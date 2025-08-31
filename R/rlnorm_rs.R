#' @title Random Variable From a Lognormal Distribution
#'
#' @description Generate random numbers
#' from a lognormal distribution,
#' rescaled to have user-specified
#' population mean and standard
#' deviation.
#'
#' @details
#' First, specify the parameter,
#' `mui` and `sigma`, and the
#' desired population mean and standard
#' deviation. The random numbers, drawn
#' from a lognormal distribution by
#' [stats::rlnorm()], will then be
#' rescaled with the desired population
#' mean and standard.
#'
#' @param n The number of random numbers
#' to generate.
#'
#' @param mui The parameter `mui` to
#' be used by [stats::rlnorm()].
#'
#' @param sigma The parameter `sigma` to
#' be used by [stats::rlnorm()].
#'
#' @param rev If TRUE, the distribution
#' is revered to generate a negatively
#' skewed distribution. Default is
#' FALSE.
#'
#' @param pmean Population mean.
#'
#' @param psd Population standard deviation.
#'
#' @return
#' A vector of the generated
#' random numbers.
#'
#' @examples
#' set.seed(90870962)
#' x <- rlnorm_rs(n = 5000,
#'                mui = 0,
#'                sigma = 1,
#'                pmean = 0,
#'                psd = 1)
#' mean(x)
#' sd(x)
#' hist(x)
#'
#' @export

rlnorm_rs <- function(n = 10,
                      mui = 0,
                      sigma = 1,
                      rev = FALSE,
                      pmean = 0,
                      psd = 1) {
  sigmasq <- sigma^2
  bmean <- exp(mui + sigmasq / 2)
  bsd   <- sqrt((exp(sigmasq) - 1) * exp(2 * mui + sigmasq))
  x <- stats::rlnorm(n, mui, sigma)
  if (!rev) {
    x <- pmean + psd * (x - bmean) / bsd
  } else {
    x <- pmean - psd * (x - bmean) / bsd
  }
  x
}