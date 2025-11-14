# Functions for the Boos-Zhang method

#' @noRd
add_bz_i <- function(outi) {
  # Receive a table or a vector
  # Add bz_pvalues from R and nlt0
  out_type <- ifelse(is.null(dim(outi)),
                     yes = "vector",
                     no = "table")
  if (out_type == "vector") {
    outz1 <- rbind(outi)
  } else {
    outz1 <- outi
  }
  outz1 <- split(outz1,
                 seq_len(nrow(outz1)),
                 drop = FALSE)
  R <- sapply(outz1,
              \(x) x[,"R"])
  if (length(unique(R)) != 1) {
    return(outi)
  } else {
    R <- unique(R)
  }
  R_case0 <- sapply(R,
                    R_case)
  if (!all(R_case0 == "one")) {
    return(outi)
  }
  R_case0 <- R_case0[1]
  Rk <- Rext[seq(1, which(Rext == R) - 1)]
  for (j1 in seq_along(outz1)) {
    # Need to keep the colnames
    tmp <- unlist(outz1[[j1]][, c("sig", "R", "nlt0")])
    tmp2 <- add_rr_ext(
                tmp,
                R = R,
                Rk = Rk
              )
    tmp2 <- tmp2[-which(names(tmp2) %in% c("sig", "R", "nlt0"))]
    tmp3 <- cbind(outz1[[j1]], rbind(tmp2))
    outz1[[j1]] <- tmp3
  }
  if (out_type == "vector") {
    # TODO:
    # - Check
    return((unlist(outz1[[1]])))
  } else {
    return(do.call(rbind,
                   outz1))
  }
}

#' @noRd
bz_sig_partition <- function(boot_est,
                             alpha = .05) {
  R <- length(boot_est)
  R_case0 <- R_case(R)
  if (R_case0 == "cum") {
    Rs <- R_indices(R)
    boot_ps <- sapply(Rs,
                  \(x) est2p(boot_est[x],
                             min_size = -Inf)
                )
    boot_sig <- as.numeric(boot_ps < alpha)
    names(boot_sig) <- paste0("bz_", names(boot_ps))
  } else {
    boot_sig <- numeric()
  }
  boot_sig
}

#' @noRd
R_indices <- function(R) {
  Rext <- R_extrapolate()
  Rext_cm <- cumsum(Rext)
  i <- which(R == Rext_cm)
  if (length(i) == 0) stop("R not supported.")
  Rs <- Rext[seq(from = 1, to = i)]
  ik <- split(seq_len(R),
              rep(seq_along(Rs),
              times = Rs))
  names(ik) <- as.character(Rs)
  ik
}

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
  lm_out <- stats::lm(rr_mean ~ I(1 / Rk))
  rr_inf <- unname(stats::coef(lm_out)["(Intercept)"])
  rr_inf <- min(max(rr_inf, 0), 1)
  attr(rr_inf, "bz_model") <- lm_out
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
