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
                            "partable",
                            drop.list.single.group = FALSE)
  for (k0 in seq_along(mm0)) {
    for (k in seq_along(mm0[[k0]])) {
      mmi <- mm0[[k0]][[k]]
      mmi1 <- mm[[k0]][[k]]
      i <- which(mmi > 0)
      j <- mmi[i]
      mmic <- col(mmi)
      mmir <- row(mmi)
      for (x in seq_along(j)) {
        ptable_out[j[x], "start"] <- mmi1[mmir[i[x]], mmic[i[x]]]
      }
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
                       n,
                       number_of_indicators = NULL,
                       reliability = NULL,
                       keep_f_scores = FALSE,
                       x_fun = list(),
                       e_fun = list(),
                       process_data = NULL) {
  lm_y <- object$lm_y
  psi <- object$psi
  all_vars <- colnames(psi)
  i <- grepl("[:]", all_vars)
  p <- length(lm_y)
  q <- ncol(psi)
  x_raw <- gen_all_x(psi = psi,
                     n = n,
                     all_x = all_vars,
                     x_fun = x_fun)
  dat_all <- matrix(NA,
                    nrow = n,
                    ncol = length(all_vars))
  colnames(dat_all) <- all_vars
  dat_all[, colnames(x_raw)] <- x_raw
  if (!is.null(lm_y)) {
    # Update product terms
    for (y in names(lm_y)) {
      lm_y_i <- lm_y[[y]]
      beta_i <- lm_y_i$beta
      e_i <- dat_all[, y]
      y_i <- dat_all[, colnames(beta_i), drop = FALSE] %*% t(beta_i) + e_i
      # Replace the errors by the generated values
      dat_all[, y] <- as.vector(y_i)
      dat_all <- update_p_terms(dat_all)
    }
  }
  if (!is.null(number_of_indicators)) {
    dat_all <- add_indicator_scores(dat_all,
                                    ps = number_of_indicators,
                                    rels = reliability,
                                    e_fun = e_fun,
                                    keep_f_scores = keep_f_scores)
  }
  if (is.list(process_data)) {
    process_data_fun <- match.fun(process_data$fun)
    tmp <- list(dat_all)
    names(tmp) <- process_data$sim_data_name
    if (is.null(process_data$args)) {
      process_data$args <- list()
    }
    process_data_args <- utils::modifyList(process_data$args,
                                          tmp)
    dat_all_amp <- do.call(process_data_fun,
                           process_data_args)
    m_name <- process_data$processed_data_name
    if (!is.null(m_name)) {
      dat_all <- dat_all_amp[[m_name]]
    } else {
      dat_all <- dat_all_amp
    }
  }
  return(as.data.frame(dat_all))
}

#' @noRd
# Input:
# - Original model syntax
# - number_of_indicators
# - reliability
# Note:
# - To be used internally. Assume that the argument values are valid.
# - Assume the prefix of indicators are equal to the factor names.
#   - E.g., x1, x2, ... for the factor x.
# Output:
# - Modified model syntax with the measurement part
add_indicator_syntax <- function(model,
                                 number_of_indicators = NULL,
                                 reliability = NULL) {
  f_names <- names(number_of_indicators)
  reliability <- reliability[f_names]
  if (!is.null(number_of_indicators)) {
    for (i in seq_along(number_of_indicators)) {
      i_name <- names(number_of_indicators)[i]
      model <- c(model,
                paste0(i_name, " =~ ",
                       paste0(i_name,
                              seq_len(number_of_indicators[i]),
                              collapse = " + ")))
    }
  }
  return(model)
}


#' @noRd
# Input:
# - A matrix of scores (e.g., from mm_lm_data()).
# - ps: A named vector of the number of indicators.
# - rels: A named vector of reliability coefficients.
# - keep_f_scores: If TRUE, factor scores are ratained.
# Output:
# - A matrix with indicator scores added.
add_indicator_scores <- function(x,
                                 ps,
                                 rels,
                                 keep_f_scores = FALSE,
                                 e_fun = NULL) {
  if (!setequal(names(ps), names(rels))) {
    stop("'ps' and 'rels' do not match in names.")
  }
  f_names <- names(ps)
  if (length(setdiff(f_names, colnames(x))) != 0) {
    stop("Some factors not found in the data.")
  }
  rels <- rels[f_names]
  f_scores <- sapply(f_names,
                     function(xx) x[, xx, drop = TRUE],
                     simplify = FALSE)
  if (is.list(e_fun) && length(e_fun) > 0) {
    e_fun_names <- names(e_fun)
    e_fun1 <- sapply(f_names,
                     function(x, e_fun) {
                       if (x %in% e_fun_names) {
                         return(e_fun[[x]])
                       } else {
                         return(list())
                       }
                     },
                     e_fun = e_fun,
                     simplify = FALSE,
                     USE.NAMES = TRUE)
  } else {
    e_fun1 <- vector("list", length(f_names))
    names(e_fun1) <- f_names
  }
  prefixes <- f_names
  out0 <- mapply(gen_indicator_scores,
                 f_score = f_scores,
                 p = ps,
                 omega = rels,
                 prefix = prefixes,
                 e_fun = e_fun1,
                 SIMPLIFY = FALSE)
  out1 <- do.call(cbind,
                  out0)
  out2 <- cbind(x, out1)
  if (!keep_f_scores) {
    # TODO:
    # - Should also remove product terms
    i <- match(f_names, colnames(x))
    out2 <- out2[, -i, drop = FALSE]
  }
  return(out2)
}

#' @noRd
# Input:
# - f_score: A vector or column vector of factor scores.
# - p: The number of indicators.
# - omega: The reliability.
# - prefix: The prefix for naming the indicators.
# Ouput:
# - A matrix of indicator scores
gen_indicator_scores <- function(f_score,
                                 p,
                                 omega,
                                 prefix = "x",
                                 e_fun = list()) {
  # e_fun is of this form
  # list(rexp_rs, ....)
  f_score <- matrix(as.vector(f_score),
                    ncol = 1)
  n <- nrow(f_score)
  lambda0 <- lambda_from_reliability(p = p,
                                    omega = omega)
  lambda1 <- matrix(lambda0,
                    nrow = 1,
                    ncol = p)
  e_sd <- sqrt(1 - lambda0^2)
  if (length(e_fun) > 0) {
    ee_fun <- e_fun[[1]]
    ee_fun <- match.fun(ee_fun)
    ee_args <- e_fun[-1]
    ee_args <- utils::modifyList(ee_args,
                                 list(n = n * p))
    e <- do.call(ee_fun,
                 ee_args)
    e <- e * e_sd
  } else {
    e <- matrix(stats::rnorm(n * p,
                             mean = 0,
                             sd = e_sd),
                nrow = n,
                ncol = p)
  }

  x <- f_score %*% lambda1 + e
  colnames(x) <- paste0(prefix, seq(from = 1,
                                    to = p))
  return(x)
}

#' @noRd
# Check product terms
# Input:
# - Output of model_matrices_pop()
# Output:
# - Logical. TRUE if the model has one or more product terms
has_p_terms <- function(object) {
  vnames <- colnames(object$beta)
  return(any(grepl(":", vnames)))
}

#' @noRd
# Input:
# - Output of model_matrices_pop()
# Output:
# - Psi matrix if all variables standardized
#
psi_std <- function(object,
                    n_std = 100000) {
  if (has_p_terms(object)) {
    out <- psi_std_monte_carlo(object = object,
                               n_std = n_std)
  } else {
    out <- tryCatch(psi_std_analytic(object = object),
                    error = function(e) e)
    if (inherits(out, "error")) {
      out <- psi_std_monte_carlo(object = object,
                                 n_std = n_std)
    }
  }
  if (any(diag(out) < 0)) {
    tmp <- colnames(out)[(diag(out) < 0)]
    msg <- paste0("Negative model or implied variance(s) for ",
                  paste0(tmp, collapse = ", "),
                  ". ",
                  "Please check the model.")
    stop(msg)
  }
  if (any(diag(out) > 1)) {
    # TODO:
    # - Should skip all derived terms, such as x^2.
    # - Find a better way to identify derived terms.

    # Skip product terms
    tmp0 <- colnames(out)
    tmp1 <- sapply(strsplit(tmp0, ":", fixed = TRUE),
                   length)
    tmp2 <- diag(out)[(tmp1 == 1)]
    if (any(tmp2 > 1)) {
      tmp <- colnames(out)[(tmp2 >= 1)]
      msg <- paste0("Model or implied variance(s) for ",
                    paste0(tmp, collapse = ", "),
                    " greater than 1 when standardized. ",
                    "Please check the model.")
      stop(msg)
    }
  }
  return(out)
}

#' @noRd
# Input:
# - Output of model_matrices_pop()
# Output:
# - Psi matrix if all variables standardized
# Note:
# - Determine by Monte Carlo
# - Can have product terms
psi_std_analytic <- function(object) {
  vnames <- colnames(object$beta)
  p <- length(vnames)
  object_lm <- mm_lm(object)
  lm_y <- object_lm$lm_y
  mm_tmp <- object
  for (yy in names(lm_y)) {
    sigma <- implied_sigma(mm_tmp)
    mm_tmp$psi[yy, yy] <- 2 - sigma[yy, yy]
  }
  # Check
  if (all.equal(rep(1, p),
                diag(implied_sigma(mm_tmp)),
                tolerance = 1e-4,
                check.attributes = FALSE,
                check.class = FALSE)) {
    return(mm_tmp$psi)
  } else {
    stop("Analytical standardization failed. ",
         "Please set std_force_monte_carlo to TRUE ",
         "to standardize by simulation.")
  }
}

#' @noRd
endo_beta <- function(object) {
  beta <- object$beta
  vnames <- colnames(beta)
  exo <- apply(beta,
               MARGIN = 1,
               function(xx) all(xx == 0))
  vnames[!exo]
}

#' @noRd
# Input:
# - Output of model_matrices_pop()
# Output:
# - Psi matrix if all variables standardized
# Note:
# - Determine by Monte Carlo
# - Can have product terms
psi_std_monte_carlo <- function(object,
                                n_std = 100000,
                                x_fun = list()) {
  object <- mm_lm(object)
  lm_y <- object$lm_y
  psi <- object$psi
  all_vars <- colnames(psi)
  i <- grepl("[:]", all_vars)
  p <- length(lm_y)
  q <- ncol(psi)
  x_raw <- gen_all_x(psi = psi,
                     n = n_std,
                     all_x = all_vars,
                     x_fun = list())
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
gen_all_x <- function(psi,
                      n,
                      all_x,
                      x_fun = list()) {
  x_raw <- gen_pure_x(psi = psi,
                      n = n,
                      x_fun = x_fun)
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
gen_pure_x <- function(psi,
                       n,
                       x_fun = list()) {
  p <- ncol(psi)
  psi_q <- chol(psi)
  x0 <- matrix(stats::rnorm(n * p),
               nrow = n,
               ncol = p)
  x <- x0 %*% psi_q
  colnames(x) <- colnames(psi)

  # Generate data using x_fun functions
  if (length(x_fun) > 0) {
    xnames <- colnames(x)
    x_fun_names <- names(x_fun)
    if (!all(x_fun_names %in% xnames)) {
      tmp <- setdiff(x_fun_names,
                     xnames)
      msg <- paste0(paste0(tmp, collapse = ", "),
                    " in x_fun but not in the model.")
      stop(msg)
    }
    for (xx in x_fun_names) {
      psi_xx <- psi[xx, ]
      psi_xx <- psi_xx[-which(xnames == xx)]
      if (!isTRUE(all.equal(psi_xx,
                            rep(0, length(psi_xx)),
                            tolerance = 1e-6,
                            check.attributes = FALSE))) {
        tmp <- paste0(xx, " must have zero covariance with other variables",
                      " to be eligible for using x_fun.")
        stop(tmp)
      }
    }
    tmpfct <- function(xx,
                       n) {
      xx_fun <- xx[[1]]
      xx_fun <- match.fun(xx_fun)
      xx <- xx[-1]
      xx <- utils::modifyList(xx,
                              list(n = n))
      out <- do.call(xx_fun,
                     xx)
      out
    }
    x_fun_out <- sapply(x_fun,
                        tmpfct,
                        n = n,
                        simplify = FALSE,
                        USE.NAMES = TRUE)
    for (xx in x_fun_names) {
      x[, xx] <- x_fun_out[[xx]]
    }
  }

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

#' @noRd

miss_pattern <- function(data) {
  # It is an internal function
  # Assume data is either a data frame or a matrix
  n <- nrow(data)
  p <- ncol(data)
  if (sum(stats::complete.cases(data)) == n) {
    # Complete data
    out <- matrix(1,
                  ncol = p,
                  nrow = 1)
    out[1, ] <- 1
    colnames(out) <- colnames(data)
    rownames(out) <- nrow(data)
    nvalid <- rep(n, p)
    names(nvalid) <- colnames(data)
    attr(out, "nvalid") <- nvalid
    return(out)
  }
  data_na <- !is.na(data)
  pattern <- apply(data_na,
                   MARGIN = 1,
                   \(x) paste0(as.numeric(x), collapse = ""))
  pattern_n0 <- table(pattern)
  pattern_n <- as.numeric(pattern_n0)
  names(pattern_n) <- names(pattern_n0)
  pattern_n <- pattern_n[order(names(pattern_n), decreasing = TRUE)]
  n_pattern <- length(pattern_n)
  tmpfct <- function(x) {
    as.numeric(strsplit(x, "")[[1]])
  }
  out <- do.call(rbind,
                 lapply(names(pattern_n),
                        tmpfct))
  colnames(out) <- colnames(data)
  rownames(out) <- pattern_n

  nvalid <- colSums(diag(pattern_n) %*% out)
  names(nvalid) <- colnames(data)
  attr(out, "nvalid") <- nvalid
  out
}

#' @noRd
print_miss_pattern <- function(mp,
                               digits = 2) {
  mp_out <- mp
  mp_count <- as.numeric(rownames(mp))
  n <- sum(as.numeric(rownames(mp)))
  mp_prop <- mp_count / n
  mp_prop_str <- formatC(mp_prop,
                          digits = digits + 2,
                          format = "f")
  mp_prop_str <- gsub("0.", ".", mp_prop_str,
                      fixed = TRUE)
  mp_nvalid <- attr(mp, "nvalid")
  mp_prop_valid_str <- formatC(mp_nvalid / n,
                                digits = digits,
                                format = "f")
  mp_prop_valid_str <- gsub("0.", ".", mp_prop_valid_str,
                            fixed = TRUE)
  mp_out[] <- ifelse(as.character(mp) == 1,
                      yes = "O",
                      no = "-")
  mp_p <- rowSums(mp)
  mp_out <- cbind(`P Prop` = mp_prop_str,
                  mp_out,
                  `# V` = mp_p)
  mp_out <- rbind(mp_out,
                  c("V Prop", mp_prop_valid_str, ""))
  rownames(mp_out) <- NULL
  print(as.data.frame(mp_out),
        quote = FALSE,
        row.names = FALSE,
        right = TRUE)
  cat("\nNote:\n")
  catwrap("- 'O': A variable has data in a pattern.",
          exdent = 2)
  catwrap("- '-': A variable has missing data in a pattern.",
          exdent = 2)
  catwrap("- P Prop: Proportion of each missing pattern.",
          exdent = 2)
  catwrap("- # V: Number of non-missing variable(s) in a pattern.",
          exdent = 2)
  catwrap("- V Prop: Proportion of non-missing data of each variables.",
          exdent = 2)
  cat("\n")
}

#' @noRd
pure_x <- function(fit) {
  union(lavaan::lavNames(fit, "ov.x"),
        lavaan::lavNames(fit, "lv.x"))
}

#' @noRd
pure_y <- function(fit) {
  union(lavaan::lavNames(fit, "ov.y"),
        lavaan::lavNames(fit, "lv.y"))
}