#' @title Random Variables Using the IG method.
#'
#' @description Generate multivariate
#' random numbers
#' using the IG method.
#'
#' @details
#' First, random numbers will be generated
#' using [covsim::rIG()], with the specified
#' target population marginal skewness and
#' excess kurtosis.
#' The random numbers will then be
#' rescaled with the desired population
#' means and standard deviations.
#'
#' @param n The number of cases
#' to generate.
#'
#' @param sigma The population
#' covariance matrix. It is
#' recommended to supply a correlation matrix
#' (with diagonal elements equal to one).
#' The numbers will be rescaled to have
#' population means and SDs specified
#' by `pmean` and `psd`, respectively.
#'
#' @param skew A numeric vector of the
#' population skewness coefficients.
#' If it is a scalar,
#' its value will be used as the population
#' skewness of all variables to be generated.
#'
#' @param kurt A numeric vector of the
#' population excess kurtosis coefficients.
#' If it is a scalar,
#' its value will be used as the population
#' excess kurtosis coefficients. of all
#' variables to be generated.
#'
#' @param rIG_args A list of extra
#' arguments to be passed to
#' [covsim::rIG()].
#'
#' @param pmean A numeric vector
#' population means. If it is a scalar,
#' its value will be used as the population
#' means of all variables to be generated.
#'
#' @param psd A numeric vector of
#' population standard
#' deviations. If it is a scalar,
#' its value will be used as the population
#' standard deviations of all variables
#' to be generated.
#'
#' @return A two-dimension matrix of the
#' generated
#' random numbers.
#'
#' @references
#' Foldnes, N. and Olson, U. H. (2016).
#' A simple simulation technique for
#' nonnormal data with prespecified
#' skewness, kurtosis, and covariance
#' matrix.
#' *Multivariate Behavioral Research*,
#' *51*(2--3), 207--219.
#' \doi{10.1080/00273171.2015.1133274}
#'
#' @examples
#' set.seed(90870962)
#' sigma <- matrix(c(1, .3, .3, 1), 2, 2)
#' sigma
#' x <- rig_rs(n = 5000,
#'             sigma = sigma,
#'             skew = 2,
#'             kurt = 6
#' )
#' colMeans(x)
#' apply(x, 2, sd)
#' hist(x[, 1])
#'
#' @export

rig_rs <- function(
          n = 10,
          sigma = diag(1),
          skew = 0,
          kurt = 0,
          rIG_args = list(),
          pmean = 0,
          psd = 1
) {
  p <- ncol(sigma)
  if (length(skew) == 1) {
    skew <- rep(skew, p)
  } else if (length(skew) != p) {
    stop("The length of skew is not 1 or the number of variables")
  }
  if (length(kurt) == 1) {
    kurt <- rep(kurt, p)
  } else if (length(kurt) != p) {
    stop("The length of kurt is not 1 or the number of variables")
  }
  if (length(pmean) == 1) {
    pmean <- rep(pmean, p)
  } else if (length(pmean) != p) {
    stop("The length of pmean is not 1 or the number of variables")
  }
  if (length(psd) == 1) {
    psd <- rep(psd, p)
  } else if (length(psd) != p) {
    stop("The length of psd is not 1 or the number of variables")
  }
  x0 <- tryCatch(do.call(
            covsim::rIG,
            c(list(
              N = n,
              sigma = sigma,
              skewness = skew,
              excesskurtosis = kurt,
              reps = 1),
              rIG_args)
          ),
          error = function(e) e
        )
  if (inherits(x0, "error")) {
    # Try typeA = "symm"
    rIG_args <- utils::modifyList(
                  rIG_args,
                  list(typeA = "symm")
                )
    x0 <- tryCatch(do.call(
          covsim::rIG,
          c(list(
            N = n,
            sigma = sigma,
            skewness = skew,
            excesskurtosis = kurt,
            reps = 1),
            rIG_args)
        ),
        error = function(e) e
      )
    if (inherits(x0, "error")) {
      stop("rig_rs failed. Please check the values of the arguments. covsim::rIG message: ",
      x0$message)
    }
  }
  x0 <- x0[[1]]
  x <- scale(
    x0,
    center = -pmean / psd,
    scale = 1 / psd
  )
  x
}
