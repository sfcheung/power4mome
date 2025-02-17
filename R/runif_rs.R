#' @title Generate random numbers from a
#' uniform distribution with desired
#' population mean and SD
#'
#' @description Generate random numbers
#' from a uniform distribution, with
#' user-specified population mean and
#' standard deviation.
#'
#' @details First, the user specifies
#' the parameters, min and max, and the
#' desired population mean and standard
#' deviation. Then the random numbers
#' will be generated and rescaled with
#' the desired population mean and
#' standard.
#'
#' @param n The number of random numbers
#' to generate. Default is 10.
#'
#' @param min min for runif.
#'
#' @param max max for runif.
#'
#' @param pmean Population mean.
#'
#' @param psd Population standard
#' deviation.
#'
#' @return
#'  A vector of the generated random numbers.
#'
#' @examples
#' set.seed(90870962)
#' x <- runif_rs(n = 5000, min = 2, max = 4, pmean = 3, psd = 1)
#' mean(x)
#' sd(x)
#' hist(x)
#'
#' @export

runif_rs <- function(n = 10,
                     min = 0,
                     max = 1,
                     pmean = 0,
                     psd = 1) {
    #arg0 <- match.call()
    min0 <- min
    max0 <- max
    bmean <- (min0 + max0) / 2
    bsd   <- (max0 - min0) / sqrt(12)
    x <- stats::runif(n, min0, max0)
    x <- pmean + psd * (x - bmean) / bsd
    x
  }