# Functions for the Boos-Zhang method

# Functions affected by this methods
# - summarize_one_test_vector()
# - rejection_rates_i_vector()
# - test_indirect_effect()
# - test_index_of_mome()
# - test_cond_indirect()
#
# - summarize_one_test_data_frame()
# - rejection_rates_i_data_frame()
# - test_k_indirect_effects()
# - test_cond_indirect_effects()

#' @noRd
merge_for_collapse <- function(out0) {
  outnames <- colnames(out0[[1]])
  a <- lapply(out0,
              \(x) split(x, seq_len(nrow(x))))
  f <- function(...,
                cnames = NULL) {
    args <- list(...)
    out <- do.call(
            rbind,
            args)
    colnames(out) <- cnames
    out
  }
  b <- do.call(mapply,
               c(list(FUN = f),
                 a,
                 list(MoreArgs = list(cnames = outnames),
                      SIMPLIFY = FALSE)))
}

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
    if (!inherits(outz1, "data.frame")) {
      outz1 <- as.data.frame(outz1)
    }
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
  R_case <- sapply(R,
                    bz_case)
  if (!all(R_case == "one")) {
    return(outi)
  }
  Rext <- R_extrapolate()
  R_case <- R_case[1]
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
    out <- do.call(rbind,
                   outz1)
    if (inherits(outi, "matrix")) {
      out <- as.matrix(out)
    }
    rownames(out) <- rownames(outi)
    return(out)
  }
}

#' @noRd
bz_sig_partition <- function(boot_est,
                             alpha = .05) {
  # If R denotes a set of Rs,
  # compute the rejection rate for each partition and
  # return a vector of these rates
  R <- length(boot_est)
  R_case <- bz_case(R)
  if (R_case == "cum") {
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
  # Generate a list of indices
  # for a set of Rs
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
bz_case <- function(R) {
  # Check whether the R is one of those
  # for one-R extrapolation ("one"), or
  # for a set of Rs ("cum")
  # Default is ""
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
  # Do simple linear regression to
  # estimate the rejection rate when R == Inf
  # Input:
  # - A vector with "bz_*" elements,
  #   "*" the number of resamples.
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
  # R is the number of resamples
  # Rk is the values of R to extrapolate the
  # rejection rates
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
  # Compute the rejection rates for
  # a value of R (k), given
  # - the number of estimates, or
  # - the number of resamples
  if (length(x) == 0) {
    # TODO:
    # - This section does not work,
    #   but it is not used.
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
