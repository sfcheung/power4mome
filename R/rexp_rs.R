#' @title Random Exponential Variable
#'
#' @description Generate random numbers
#' from an exponential distribution,
#' rescaled to have user-specified
#' population mean and standard
#' deviation.
#'
#' @details
#' First, specify the parameter,
#' `rate`, and the
#' desired population mean and standard
#' deviation. The random numbers, drawn
#' from an exponential distribution by
#' [stats::rexp()], will then be
#' rescaled with the desired population
#' mean and standard.
#'
#' @param n The number of random numbers
#' to generate. Default is 10.
#'
#' @param rate `rate` for [stats::rexp()].
#'
#' @param pmean Population mean.
#'
#' @param psd Population standard
#' deviation.
#'
#' @param rev If TRUE, the distribution
#' is revered to generate a negatively
#' skewed distribution. Default is
#' FALSE.
#'
#' @return A vector of the generated
#' random numbers.
#'
#' @examples
#' set.seed(90870962)
#' x <- rexp_rs(n = 5000,
#'              rate = 4,
#'              pmean = 3,
#'              psd = 1)
#' mean(x)
#' sd(x)
#' hist(x)
#'
#' @export

rexp_rs <- function(n = 10,
                    rate = 1,
                    pmean = 0,
                    psd = 1,
                    rev = FALSE) {
  bmean <- 1 / rate
  bsd   <- 1 / rate
  x <- stats::rexp(n, rate)
  if (!rev) {
    x <- pmean + psd * (x - bmean) / bsd
  } else {
    x <- pmean - psd * (x - bmean) / bsd
  }
  x
}