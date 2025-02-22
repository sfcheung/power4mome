#' @title Random Generalized Normal Variable
#'
#' @description Generate random numbers
#' from generalized normal distribution,
#' rescaled to have user-specified
#' population mean and standard
#' deviation.
#'
#' @details
#' First, specify the parameter `p`
#' and the desired population
#' mean and standard deviation. The
#' random numbers, drawn from the
#' generalized normal distribution by
#' [pgnorm::rpgnorm()], will then be
#' rescaled with the desired population
#' mean and standard.
#'
#' @param n The number of random numbers
#' to generate. Default is 10.
#'
#' @param p The parameter of the
#' distribution. Must be a positive
#' non-zero number. Default is 2,
#' resulting in a normal distribution.
#' Higher than 2 results in negative
#' excess kurtosis. Lower than 2 results
#' in positive excess kurtosis.
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
#' x <- rpgnorm_rs(n = 5000,
#'                 p = 2,
#'                 pmean = 0,
#' psd = 1)
#' mean(x)
#' sd(x)
#' hist(x)
#' x_kurt <- function(p) {gamma(5/p)*gamma(1/p)/(gamma(3/p)^2) - 3}
#' p <- 6
#' x <- rpgnorm_rs(n = 50000, p = p, pmean = 0, psd = 1)
#' mean(x)
#' sd(x)
#' x_kurt(p)
#' qqnorm(x); qqline(x)
#' p <- 1
#' x <- rpgnorm_rs(n = 50000, p = p, pmean = 0, psd = 1)
#' mean(x)
#' sd(x)
#' x_kurt(p)
#' qqnorm(x); qqline(x)
#' @export

rpgnorm_rs <- function(n = 10,
                       p = 2,
                       pmean = 0,
                       psd = 1) {
    t_sd <- sqrt(gamma(3 / p) / gamma(1 / p)) * (p ^ (1 / p))
    pmean + psd * pgnorm::rpgnorm(n, p) / t_sd
  }