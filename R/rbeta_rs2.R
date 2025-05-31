#' @title Random Variable From a Beta Distribution (User Range)
#'
#' @description Generate random numbers
#' from a beta distribution, rescaled to
#' have user-specified population mean
#' and standard deviation, and within a
#' specific range.
#'
#' @details
#' First, specify the two parameters,
#' `shape1` and `shape2`, and the
#' desired population mean and standard
#' deviation. The random numbers, drawn
#' from a beta distribution by
#' [stats::rbeta()] will then be
#' rescaled to the desired population range.
#'
#' @param n The number of random numbers
#' to generate.
#'
#' @param bmean The population mean.
#'
#' @param bsd The population standard
#' deviation. If `bsd` is zero or
#' negative, all random numbers will be
#' equal to `bmean`.
#'
#' @param blow The lower bound of the
#' target range.
#'
#' @param bhigh The upper bound of the
#' target range.
#'
#' @return A vector of the generated
#' random numbers.
#'
#' @examples
#' set.seed(90870962)
#' x <- rbeta_rs2(n = 5000,
#'                bmean = .80,
#'                bsd = .10,
#'                blow = .00,
#'                bhigh = .95)
#' mean(x)
#' sd(x)
#' hist(x)
#' y <- rbeta_rs2(n = 5000,
#'                bmean = 4,
#'                bsd = 3,
#'                blow = -10,
#'                bhigh = 10)
#' mean(y)
#' sd(y)
#' hist(y)
#'
#' @export

rbeta_rs2 <- function(n = 10,
                      bmean,
                      bsd,
                      blow = 0,
                      bhigh = 1) {
  if (bsd > 0) {
    brange <- bhigh - blow
    mean1 <- (bmean - blow) / brange
    var1 <- (bsd^2) / (brange^2)
    d <- (1 - mean1) / mean1
    myalpha <- mean1 * (mean1 * (1 - mean1) / var1 - 1)
    mybeta <- myalpha * d
    results <- stats::rbeta(n,
                            myalpha,
                            mybeta) * brange + blow
  }

  if (bsd <= 0) {
    results <- rep(bmean, n)
  }

  results
}