#' @title Random Variable From a t Distribution
#'
#' @description Generate random numbers
#' from a *t* distribution, rescaled to
#' have user-specified population mean
#' and standard deviation.
#'
#' @details
#' First, specify the parameter `df`
#' and the desired population
#' mean and standard deviation. The
#' random numbers, drawn from the
#' generalized normal distribution by
#' [stats::rt()], will then be
#' rescaled with the desired population
#' mean and standard.
#'
#' @param n The number of random numbers
#' to generate.
#'
#' @param df `df` for [stats::rt()].
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
#' x <- rt_rs(n = 5000,
#'            df = 5,
#'            pmean = 3,
#'            psd = 1)
#' mean(x)
#' sd(x)
#' hist(x)
#'
#' @export

rt_rs <- function(n = 10,
                  df = 5,
                  pmean = 0,
                  psd = 1) {
  #  df0 <- match.call()$df
    df0 <- df
    bmean <- 0
    bsd   <- sqrt(df0 / (df0 - 2))
    x <- stats::rt(n, df0)
    x <- pmean + psd * (x - bmean) / bsd
    x
  }