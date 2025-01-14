#' @noRd
# Input:
# - A list of model matrices
# - The corresponding parameter table.
# Output:
# - The parameter table with start values updated
start_from_mm <- function(ptable,
                          mm) {
  ptable_out <- ptable
  fit <- lavaan::sem(ptable,
                     do.fit = FALSE)
  mm0 <- lavaan::lavInspect(fit,
                            "partable")
  for (k in seq_along(mm0)) {
    mmi <- mm0[[k]]
    mmi1 <- mm[[k]]
    i <- which(mmi > 0)
    j <- mmi[i]
    mmic <- col(mmi)
    mmir <- row(mmi)
    for (x in seq_along(j)) {
      ptable_out[j[x], "start"] <- mmi1[mmir[i[x]], mmic[i[x]]]
    }
  }
  ptable_out
}

#' @noRd
# Input:
# - mm_lm object
# Output:
# - Generated data
mm_lm_data <- function(object,
                       n) {
  lm_y <- object$lm_y
  psi <- object$psi
  all_vars <- colnames(psi)
  i <- grepl("[:]", all_vars)
  p <- length(lm_y)
  q <- ncol(psi)
  x_raw <- gen_all_x(psi = psi,
                     n = n,
                     all_x = all_vars)
  dat_all <- matrix(NA,
                    nrow = n,
                    ncol = length(all_vars))
  colnames(dat_all) <- all_vars
  dat_all[, colnames(x_raw)] <- x_raw
  for (y in names(lm_y)) {
    lm_y_i <- lm_y[[y]]
    beta_i <- lm_y_i$beta
    e_i <- dat_all[, y]
    y_i <- dat_all[, colnames(beta_i), drop = FALSE] %*% t(beta_i) + e_i
    # Replace the errors by the generated values
    dat_all[, y] <- as.vector(y_i)
    dat_all <- update_p_terms(dat_all)
  }
  return(as.data.frame(dat_all))
}

#' @noRd
# Input:
# - One lm model
# Output:
# - Generated data
psi_std <- function(object,
                    n_std = 100000) {
  object <- mm_lm(object)
  lm_y <- object$lm_y
  psi <- object$psi
  all_vars <- colnames(psi)
  i <- grepl("[:]", all_vars)
  p <- length(lm_y)
  q <- ncol(psi)
  x_raw <- gen_all_x(psi = psi,
                     n = n_std,
                     all_x = all_vars)
  dat_all <- matrix(NA,
                    nrow = n_std,
                    ncol = length(all_vars))
  colnames(dat_all) <- all_vars
  dat_all[, colnames(x_raw)] <- x_raw
  for (y in names(lm_y)) {
    lm_y_i <- lm_y[[y]]
    beta_i <- lm_y_i$beta
    y_hat <- dat_all[, colnames(beta_i), drop = FALSE] %*% t(beta_i)
    y_hat_var <- stats::var(y_hat[, 1])
    y_e_var <- 1 - y_hat_var
    # Update psi
    psi[y, ] <- psi[y, ] * sqrt(y_e_var)
    psi[, y] <- psi[, y] * sqrt(y_e_var)
    # Rescale the errors
    y_e <- dat_all[, y, drop = TRUE] * sqrt(y_e_var) / stats::sd(dat_all[, y])
    # Replace the errors by the generated values
    # Update product terms
    dat_all <- update_p_terms(dat_all)
    dat_all[, y] <- y_e + y_hat
  }
  return(psi)
}


#' @noRd
# Generate all x's
# TODO:
# - Allow for categorical variables
# - Allow for nonnormal variables
gen_all_x <- function(psi,
                      n,
                      all_x) {
  x_raw <- gen_pure_x(psi = psi,
                      n = n)
  x_raw <- add_p_terms(x_raw,
                       all_x = all_x)
  x_raw
}

#' @noRd
# Compute the product term
update_p_terms <- function(x) {
  out <- x
  for (xx in colnames(out)) {
    if (!grepl("[:]", xx)) next
    a <- strsplit(xx, ":")[[1]]
    b <- apply(out[, a],
               MARGIN = 1,
               prod)
    out[, xx] <- b
  }
  out
}

#' @noRd
# Compute the product term
add_p_terms <- function(x,
                        all_x) {
  n <- nrow(x)
  p1 <- length(all_x)
  x0 <- colnames(x)
  out <- matrix(NA,
                ncol = p1,
                nrow = n)
  colnames(out) <- all_x
  out[, x0] <- x
  for (xx in colnames(out)) {
    if (!grepl("[:]", xx)) next
    a <- strsplit(xx, ":")[[1]]
    b <- apply(out[, a],
                MARGIN = 1,
                prod)
    out[, xx] <- b
  }
  out
}

#' @noRd
# TODO:
# - Allow for nonnormal variables.
gen_pure_x <- function(psi,
                       n) {
  p <- ncol(psi)
  psi_q <- chol(psi)
  x0 <- matrix(stats::rnorm(n * p),
               nrow = n,
               ncol = p)
  x <- x0 %*% psi_q
  colnames(x) <- colnames(psi)
  x
}

#' @noRd
implied_sigma <- function(mm) {
  beta <- mm$beta
  psi <- mm$psi
  p <- ncol(psi)
  sigma <- solve(diag(p) - beta) %*% psi %*% t(solve(diag(p) - beta))
  return(sigma)
}
