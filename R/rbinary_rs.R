#' @title Random Binary Variable
#'
#' @description Generate random numbers
#' from a distribution of 0 or 1,
#' rescaled to have user-specified
#' population mean and standard
#' deviation.
#'
#' @details
#' First, specify probability of 1
#' (`p1`), and the desired population
#' mean and standard deviation. The
#' random numbers, drawn from a
#' distribution of 0 (`1 - p1`
#' probability) and 1 (`p1`
#' probability), will then be rescaled
#' with the desired population mean and
#' standard.
#'
#' @param n The number of random numbers
#' to generate.
#'
#' @param p1 The probability of being
#' 1, before rescaling.
#'
#' @param pmean Population mean.
#'
#' @param psd Population standard
#' deviation.
#'
#' @return
#' A vector of the generated
#' random numbers.
#'
#' @examples
#' set.seed(90870962)
#' x <- rbinary_rs(n = 5000,
#'                 p1 = .5,
#'                 pmean = 3,
#'                 psd = 1)
#' mean(x)
#' sd(x)
#' hist(x)
#'
#' @export

rbinary_rs <- function(n = 10,
                    p1 = .50,
                    pmean = 0,
                    psd = 1) {
  bmean <- p1
  bsd   <- sqrt(p1 * (1 - p1))
  x <- sample.int(2,
                  size = n,
                  replace = TRUE,
                  prob = c(1 - p1, p1)) - 1
  x <- pmean + psd * (x - bmean) / bsd
  x
}
