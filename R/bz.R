# Functions for the Boos-Zhang method

#' @noRd
R_case <- function(R) {
  Rext <- R_extrapolate()
  Rext_cm <- cumsum(Rext)
  out <- ""
  if (R %in% Rext[-1]) {
    out <- "one"
  }
  if ((R %in% Rext_cm[-1])) {
    out <- "cum"
  }
  out
}

#' @noRd
bz_rr <- function(out) {
  tmp <- names(out)
  tmp <- tmp[grepl("bz_",
                   tmp,
                   fixed = TRUE)]
  rr_mean <- out[tmp]
  tmp2 <- gsub("bz_",
               "",
               tmp,
               fixed = TRUE)
  Rk <- as.numeric(tmp2)
  lm_out <- stats::lm(rr_mean ~ 1 / Rk)
  rr_inf <- unname(stats::coef(lm_out)["(Intercept)"])
  rr_inf
}

#' @noRd
add_rr_ext <- function(
                outi,
                R,
                Rk) {
  # Add extrapolated values to a vector
  out <- outi
  nlt0 <- unname(out["nlt0"])
  for (Ri in Rk) {
    tmp1 <- rr_extrapolated(
                x = R,
                nlt0 = nlt0,
                k = Ri
              )
    tmp2 <- paste0("bz_",
                   as.character(Ri))
    names(tmp1) <- tmp2
    out <- c(out,
             tmp1)
  }
  tmp1 <- unname(out["sig"])
  tmp2 <- paste0("bz_",
                 as.character(R))
  names(tmp1) <- tmp2
  out <- c(out,
            tmp1)
  out
}

#' @noRd
rr_extrapolated <- function(
      x,
      nlt0,
      k,
      alpha = .05
    ) {
  if (length(x) == 0) {
    # x is the bootstrap estimates
    x <- x[!is.na(x)]
    R <- length(x)
    i <- as.numeric(x < 0)
    cm <- sum(i)
  } else {
    # Assume x is R
    R <- x
    cm <- nlt0
  }
  cn <- R - cm
  alpha2 <- alpha / 2
  p1 <- stats::phyper(
            floor(k * alpha2),
            m = cm,
            n = cn,
            k = k)
  p2 <- stats::phyper(
            ceiling(k * (1 - alpha2)),
            m = cm,
            n = cn,
            k = k,
            lower.tail = FALSE)
  p <- p1 + p2
  p
}

#' @noRd
R_extrapolate <- function(
                  alpha = .05,
                  Rmax = getOption(
                            "power4mome.bz_Rmax",
                            default = 359)
) {
  # Find supported values of R
  # alpha is two-tailed alpha
  alpha2 <- alpha / 2
  tmp1 <- seq(1, Rmax)
  tmp2 <- (tmp1 + 1) * alpha2
  tmp1[round(tmp2) == tmp2]
}
