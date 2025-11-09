# Fucntions for the Boos-Zhang method

#' @noRd
R_extrapolate <- function(alpha = .05,
                          Rmax = 300) {
  # Find supported values of R
  # alpha is two-tailed alpha
  alpha2 <- alpha / 2
  tmp1 <- seq(1, Rmax)
  tmp2 <- (tmp1 + 1) * alpha2
  tmp1[round(tmp2) == tmp2]
}
