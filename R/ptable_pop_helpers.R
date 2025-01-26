#' @title Set Population Values
#'
#' @description Return a parameter table
#' with the population values of selected
#' parameters.
#'
#' @details
#'
#' Input:
#'
#' - par_es: The effect sizes for selected coefficients
#'
#' - es1: Effect sizes for normal coefficients
#'
#' - es2: Effect sizes for moderation (product terms)
#'
#' Output:
#'
#' - A lhs-op-rhs data frame with population values (pop)
#'
#' @return
#' A data frame with these columns: `lhs`,
#' `op`, `rhs`, `pop`, and `es`. The first
#' three columns correspond to those in
#' a `lavaan` parameter table. The column
#' `pop` stores the population values.
#' The column `es` stores the original
#' labels, for reference.
#'
#' @param par_es A named character
#' vector. The names are `lavaan` for
#' the seleted parameters. For example,
#' `m ~ x` denotes the path from `x` to
#' `m`. Can specify more than one
#' parameters. For example, `y ~ m + x`
#' denotes the two paths from `m` and
#' `x` to `y`. The value is the label
#' for the effect size: `s` for small,
#' `m` for medium, and `l` for large.
#' There are two possible values, one
#' set, `es1`, for correlations and
#' regression coefficients, the other
#' set, `es2`, for standardized
#' moderation effect, the coefficients
#' of a product term.
#'
#' @param es1 A named vector to set the
#' values for each label of the effect
#' size of correlations and regression
#' paths.
#' Default is `c("n" = .00, "s" = .10, "m" = .30, "l" = .50)`.
#'
#' @param es2 A named vector to set the
#' values for each label of the effect
#' size of product term.
#' Default is `c("n" = .00, "s" = .05, "m" = .10, "l" = .15)`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' \donttest{
#' }
#'
#' @noRd
set_pop <- function(par_es,
                    es1 = c("n" = .00,
                            "s" = .10,
                            "m" = .30,
                            "l" = .50),
                    es2 = c("n" = .00,
                            "s" = .05,
                            "m" = .10,
                            "l" = .15)) {
  es10 <- es_long(es1)
  es20 <- es_long(es2)
  to_set <- lavaan::lavParseModelString(names(par_es),
                                        as.data.frame. = TRUE)
  to_set$pop <- NA
  for (x in seq_along(par_es)) {
    y <- match(par_es[x], names(es10))
    if (is.na(y)) {
      es_num <- suppressWarnings(as.numeric(par_es[x]))
      if (!is.na(es_num)) {
        # Effect size specified numerically
        to_set[x, "pop"] <- es_num
        next
      } else {
        next
      }
    }
    # Effect size label found
    is_inter <- isTRUE(grepl(":", to_set$rhs[x], fixed = TRUE))
    to_set[x, "pop"] <- ifelse(is_inter,
                               es20[y],
                               es10[y])
  }
  to_set$es <- par_es
  to_set[, c("lhs", "op", "rhs", "pop", "es")]
}

#' @noRd
# Input:
# - A names vector of effect sizes
# Output:
# - A expanded named vector with names for negative effects.
#   E.g., "m" to "-m".

es_long <- function(es) {
  es10 <- stats::setNames(es,
                          gsub(".",
                               "",
                               names(es),
                               fixed = TRUE))
  es10n <- stats::setNames(-1 * es,
                           paste0("-",
                                  names(es10)))
  est1x <- c(es10, es10n)
  est1x
}


#' @noRd
# Input:
# - The effect sizes for selected coefficients
# Output:
# - The expanded named vectors

fix_par_es <- function(par_es,
                       model) {
  par_es_org <- par_es
  i <- match(c(".beta.", ".cov."), names(par_es))
  par_es_def <- par_es[i]
  par_es_def <- par_es_def[!is.na(par_es_def)]
  all_beta_es <- character(0)
  all_cov_es <- character(0)
  if (!all(is.na(i))) {
    par_es <- par_es[-i[!is.na(i)]]
    ptable <- lavaan::parTable(lavaan::sem(model = model,
                                           do.fit = FALSE))
    if (".beta." %in% names(par_es_def)) {
      all_beta <- ptable[ptable$op == "~", c("lhs", "op", "rhs")]
      all_beta <- apply(all_beta, 1, paste, collapse = " ")
      all_beta_es <- rep(par_es_def[".beta."], length(all_beta))
      names(all_beta_es) <- all_beta
    }
    if (".cov." %in% names(par_es_def)) {
      all_cov <- ptable[ptable$op == "~~", ]
      all_cov <- all_cov[all_cov$lhs != all_cov$rhs, ]
      all_cov <- all_cov[all_cov$exo == 1, ]
      tmp <- lavaan::lavNames(ptable, "ov.interaction")
      all_cov <- all_cov[!((all_cov$lhs == tmp) |
                           (all_cov$rhs == tmp)), ]
      all_cov <- all_cov[, c("lhs", "op", "rhs")]
      all_cov <- apply(all_cov, 1, paste, collapse = " ")
      all_cov_es <- rep(par_es_def[".cov."], length(all_cov))
      names(all_cov_es) <- all_cov
    }
  }
  out <- character(0)
  for (i in seq_along(par_es)) {
    x_name <- names(par_es[i])
    tmp1 <- lavaan::lavParseModelString(x_name,
                                        as.data.frame. = TRUE)
    tmp2 <- paste(tmp1$lhs,
                  tmp1$op,
                  tmp1$rhs)
    tmp3 <- rep(par_es[i], length(tmp2))
    names(tmp3) <- tmp2
    out <- c(out, tmp3)
  }
  # Add all_beta_es
  all_def <- c(all_beta_es, all_cov_es)
  tmp <- setdiff(names(all_def), names(out))
  out <- c(out, all_def[tmp])
  out
}

#' @noRd
# Copied from another project of sfcheung
lambda_from_reliability <- function(p = 3,
                                    omega = .70) {
  lambda <- sqrt(omega / (p - omega * (p - 1)))
  lambda
}