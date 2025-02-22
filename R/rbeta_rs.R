#' @title Random Beta Variable
#'
#' @description Generate random numbers
#' from a beta distribution, rescaled to
#' have user-specified population mean
#' and standard deviation.
#'
#' @details
#' First, specify the two parameters,
#' `shape1` and `shape2`, and the
#' desired population mean and standard
#' deviation. The random numbers, drawn
#' from a beta distribution by
#' [stats::rbeta()] will then be
#' rescaled with the desired population
#' mean and standard.
#'
#' @param n The number of random numbers
#' to generate. Default is 10.
#'
#' @param shape1 `shape1` for
#' [stats::rbeta()].
#'
#' @param shape2 `shape2` for
#' [stats::rbeta()].
#'
#' @param pmean Population mean.
#'
#' @param psd Population standard
#' deviation.
#'
#' @return A vector of the generated
#' random numbers.
#'
#' @examples
#' set.seed(90870962)
#' x <- rbeta_rs(n = 5000,
#'               shape1 = .5,
#'               shape2 = .5,
#'               pmean = 3,
#'               psd = 1)
#' mean(x)
#' sd(x)
#' hist(x)
#'
#' @export

rbeta_rs <- function(n = 10,
                     shape1 = .5,
                     shape2 = .5,
                     pmean = 0,
                     psd = 1) {
  bmean <- shape1 / (shape1 + shape2)
  bsd   <- sqrt((shape1 * shape2) / ((shape1 + shape2 + 1) * (shape1 + shape2)^2))
  x <- stats::rbeta(n,
                    shape1,
                    shape2)
  x <- pmean + psd * (x - bmean) / bsd
  x
}