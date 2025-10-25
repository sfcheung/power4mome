#' @noRd

reject_ci <- function(
      nreject,
      nvalid,
      level = .95,
      method = c("norm", "wilson")
    ) {
  method <- match.arg(method)
  out <- switch(method,
          norm = reject_ci_norm(
                    nreject = nreject,
                    nvalid = nvalid,
                    level = level
                  ),
          wilson = reject_ci_wilson(
                    nreject = nreject,
                    nvalid = nvalid,
                    level = level
                  ))
  out
}

#' @noRd
reject_ci_norm <- function(
      nreject,
      nvalid,
      level
    ) {
  reject <- nreject / nvalid
  reject_se <- sqrt(reject * (1 - reject) / nvalid)
  a <- abs(stats::qnorm(1 - (1 - level) / 2))
  reject_ci_lo <- reject - a * reject_se
  reject_ci_hi <- reject + a * reject_se
  cbind(cilo = reject_ci_lo,
        cihi = reject_ci_hi)
}

#' @noRd
reject_ci_wilson <- function(
      nreject,
      nvalid,
      level
    ) {
  stop("Wilson CI not yet supported")
}
