#' @title Generate the Population Model
#'
#' @description Generate the complete
#' population model using the model
#' syntax and user-specified effect
#' sizes.
#'
#' @details
#' The function [ptable_pop()] generates a `lavaan`
#' parameter table that can be used
#' to generate data based on the population
#' values of model parameters.
#'
#' ## Setting `pop_es` to a named vector
#'
#' If `pop_es` is specified by a named
#' vector, it must follow the convention
#' below.
#'
#' - The names of the vectors are
#'   `lavaan` names for the selected
#'   parameters. For example, `m ~ x`
#'   denotes the path from `x` to `m`.
#'
#' - Alternatively, the names can be
#'   either `".beta."` or `".cov."`.
#'   Use `".beta."` to set the default
#'   values for all regression coefficients.
#'   Use `".cov."` to set the default
#'   values for all correlations of
#'   exogenous variables (e.g., predictors).
#'
#' - If using `lavaan` names, can
#'   specify more than one parameters
#'   using `+`. For example, `y ~ m + x`
#'   denotes the two paths from `m` and
#'   `x` to `y`.
#'
#' - The value of each element can be
#'   the label for the effect size: `n`
#'   for nil, `s` for small, `m` for
#'   medium, and `l` for large. The
#'   value for each label is determined
#'   by `es1` and `es2`.
#'
#' - The value of `pop_es` can also be
#'   set to a value, but it must be
#'   quoted as a string, such as `"y ~
#'   x" = ".31"`.
#'
#' The vector `es1` is for correlations
#' and regression coefficients, and the
#' vector `es2` is for for standardized
#' moderation effect, the coefficients
#' of a product term.
#'
#' @return
#' The function [ptable_pop()] returns
#' a `lavaan` parameter table of the
#' model, with `start` set to the
#' population values.
#'
#' @param model String. The model defined
#' by `lavaan` model syntax.
#'
#' @param pop_es It can a data frame
#' with these columns: `lhs`, `op`,
#' `rhs`, and `pop`. The first three
#' columns correspond to those in a
#' `lavaan` parameter table. The column
#' `pop` stores the population values.
#' The column `es` stores the original
#' labels, for reference. It can also be
#' A named character vector. See 'Details'
#' on how to specify this vector.
#'
#' @param es1 A named vector to set the
#' values for each label of the effect
#' size of correlations and regression
#' paths.
#' Default is `c("n" = .00, "s" = .10, "m" = .30, "l" = .50)`.
#' Used only if `pop_es` is a named
#' vector.
#'
#' @param es2 A named vector to set the
#' values for each label of the effect
#' size of product term.
#' Default is `c("n" = .00, "s" = .05, "m" = .10, "l" = .15)`.
#' Used only if `pop_es` is a named
#' vector.
#'
#' @param standardized Logical. If
#' `TRUE`, the default, variances and
#' error variances are scaled to ensure
#' the population variances of the
#' endogenous variables are close to
#' one, and hence the effect sizes are
#' standardized effect sizes if the
#' variances of the continuos exogenous
#' variables are also equal to one.
#'
#' @param n_std The sample size used to
#' determine the error variances by
#' simulation. Default is 100000.
#'
#' @param std_force_monte_carlo Logical.
#' If `FALSE`, the default,
#' standardization is done analytically
#' if the model has no product terms,
#' and by simulation if the model has
#' product terms. If `TRUE`, simulation
#' will be used whether the model has
#' product terms or not. Always fall
#' back to standardization if
#' analytical standardization failed.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @examples
#'
#' model1 <-
#' "
#' m1 ~ x + c1
#' m2 ~ m1 + x2 + c1
#' y ~  m2 + m1 + x + w + x:w + c1
#' "
#'
#' model1_es <- c("m1 ~ x" = "-m",
#'                "m2 ~ m1" = "s",
#'                "y ~ m2" = "l",
#'                "y ~ x" = "m",
#'                "y ~ w" = "s",
#'                "y ~ x:w" = "s",
#'                "x ~~ w" = "s")
#'
#' ptable_final1 <- ptable_pop(model1,
#'                             pop_es = model1_es)
#' ptable_final1
#'
#' @export
# Input:
# - model: The model syntax
# - pop_es: The parameter table of parameters with population values OR
#           A named vector of the effect size labels for selected parameters
# Output:
# - The parameter table with population values
ptable_pop <- function(model,
                       pop_es,
                       es1 = c("n" = .00,
                               "s" = .10,
                               "m" = .30,
                               "l" = .50),
                       es2 = c("n" = .00,
                               "s" = .05,
                               "m" = .10,
                               "l" = .15),
                       standardized = TRUE,
                       n_std = 100000,
                       std_force_monte_carlo = FALSE) {
  # TODO:
  # - MG: Call fix_par_es() and set_pop() for each group
  # - MG: But we can also use lavaan convention, e.g.,
  #       "y ~ c('n', 'm')*x"
  if (is.character(pop_es)) {
    pop_es <- fix_par_es(pop_es,
                         model = model)
    par_pop <- set_pop(pop_es,
                       es1 = es1,
                       es2 = es2)
  } else {
    par_pop <- pop_es
  }
  par_pop <- dup_cov(par_pop)
  # TODO:
  # - MG: Merge the list of par_pop to one table
  # TODO:
  # - MG: Need fake data to force a MG parameter table
  fit0 <- lavaan::sem(model,
                      do.fit = FALSE)
  ptable0 <- lavaan::parTable(fit0)
  # TODO:
  # - MG: Include group id
  # - MG: id may not match
  par_pop2 <- merge(par_pop,
                    ptable0[, c("lhs", "op", "rhs", "id")],
                    all.x = TRUE,
                    all.y = FALSE)
  if (any(is.na(par_pop2$id))) {
    tmp1 <- is.na(par_pop2$id)
    tmp2 <- apply(par_pop2[tmp1, c("lhs", "op", "rhs"), drop = FALSE],
                  MARGIN = 1,
                  paste0,
                  collapse = " ")
    tmp2 <- paste(tmp2, collapse = ",")
    # warning("One or more parameters in 'pop_es' is not in the model: ",
    #         tmp2)
  }
  # TODO:
  # - MG: Include group id
  par_pop2 <- par_pop2[, c("id", "lhs", "op", "rhs", "pop")]

  ptable1 <- merge(ptable0,
                   par_pop2,
                   all.x = TRUE)
  ptable1[!is.na(ptable1$pop), "start"] <- ptable1[!is.na(ptable1$pop), "pop"]
  ptable1$pop <- NULL
  # TODO:
  # - Check equality constraints
  attr(ptable1, "model") <- model
  if (standardized) {
    # TODO:
    # - MG: Revise model_matrices_pop for MG
    mm <- model_matrices_pop(ptable1)
    # TODO:
    # - MG: Revise the following for MG
    if (ncol(mm$psi) != 0) {
      mm$psi <- psi_std(mm,
                        n_std = n_std)
    } else {
      # CFA model or correlation only
      # Make sure theta is a covariance matrix,
      # in case variances accidentally set.
      mm$theta <- stats::cov2cor(mm$theta)
    }
    # TODO:
    # - MG: Revise start_from_mm for MG
    ptable1 <- start_from_mm(ptable1,
                             mm)
    attr(ptable1, "model") <- model
  }
  ptable1
}

#' @describeIn ptable_pop
#'
#' @details
#' The function [model_matrices_pop()]
#' generates models matrices with
#' population values.
#'
#' @return
#' The function [model_matrices_pop()]
#' returns a `lavaan` LISREL-style model
#' matrices (like the output of
#' [lavaan::lavInspect()] with `what`
#' set to `free`), with matrix elements
#' set to the population values. If
#' `x` is the model syntax, it will be
#' stored in the attributes `model`.
#'
#' @param x It can be 'lavaan' model
#' syntax, to be passed to [ptable_pop()],
#' or a parameter table with the column
#' `start` set to the population values,
#' such as the output of [ptable_pop()].
#'
#' @param ... If `x` is a model syntax,
#' these are arguments to be passed to
#' [ptable_pop()].
#'
#' @examples
#'
#' model_matrices_pop(ptable_final1)
#'
#' model_matrices_pop(model1,
#'                    pop_es = model1_es)
#'
#' @export

model_matrices_pop <- function(x,
                               ...) {
  if (is.character((x))) {
    ptable <- ptable_pop(model = x,
                         ...)
  } else {
    ptable <- x
  }
  # TODO:
  # - MG: Revise for MG
  # - MG: Store the MG parameter table
  # - MG: If x is an output of ptable_pop,
  #       it should be ready for lavInspect().
  fit1 <- lavaan::sem(ptable,
                      do.fit = FALSE)
  mm <- lavaan::lavInspect(fit1,
                           "partable")
  # TODO:
  # - MG: Revise set_start for MG
  mm2 <- set_start(mm = mm,
                   ptable = ptable)
  attr(mm2, "header") <- NULL
  if (is.character((x))) {
    attr(mm2, "model") <- x
  } else (
    attr(mm2, "model") <- attr(x, "model")
  )
  mm2
}


#' @noRd
# Set the starting values in the model matrix
# Input:
# - The list of model matrices in lavaan
# Output:
# - The parameter table with starting values (in "start")

set_start <- function(mm,
                      ptable) {
  for (k in seq_along(mm)) {
    mmi <- mm[[k]]
    i <- which(mmi > 0)
    j <- mmi[i]
    mmic <- col(mmi)
    mmir <- row(mmi)
    for (x in seq_along(j)) {
      mmi[mmir[i[x]], mmic[i[x]]] <- ptable[j[x], "start"]
    }
    mm[[k]] <- mmi
  }
  mm
}

#' @noRd
# Input:
# - Lavaan model matrices
# Output:
# - A list of:
#   - regression models
#   - covariance matrix of x-variables
mm_lm <- function(mm) {
  # TODO:
  # - Check whether the transpose of nox-beta is in echelon form.
  # - Handle models with no y-variables (e.g., CFA).
  # - MG: Revise for MG
  model <- attr(mm, "model")
  if (is.null(model)) {
    stop("Model syntax not found")
  }
  fit1 <- lavaan::sem(model,
                      do.fit = FALSE)
  mm1 <- lavaan::lavInspect(fit1,
                            "partable")
  beta1 <- mm1$beta
  psi1 <- mm1$psi
  if (is.null(beta1)) {
    # No regression paths. A CFA model.
    # Set psi to theta, the covariance
    # matrix of factor scores to be
    # generated.
    out <- list(lm_y = NULL,
                psi_pure_x = NULL,
                psi = mm$theta)
    return(out)
  }
  ix <- apply(beta1,
              MARGIN = 1,
              function(x) all(x == 0))
  x_names <- names(ix)[ix]
  y_names <- setdiff(colnames(beta1), x_names)
  beta <- mm$beta
  psi <- mm$psi
  gen_lm <- function(y) {
    beta1_i <- beta1[y, , drop = FALSE]
    beta_i <- beta[y, beta1_i > 0, drop = FALSE]
    psi_i <- psi[beta1_i > 0, beta1_i > 0, drop = FALSE]
    list(beta = beta_i,
         psi = psi_i)
  }
  lm_y <- sapply(y_names,
                 gen_lm,
                 simplify = FALSE)
  psi_pure_x <- psi[x_names, x_names, drop = FALSE]
  out <- list(lm_y = lm_y,
              psi_pure_x = psi_pure_x,
              psi = psi)
  return(out)
}

#' @noRd
# Fix covariances in a parameter table
dup_cov <- function(ptable) {
  i <- ptable$op == "~~"
  if (any(i)) {
    j <- match(c("rhs", "op", "lhs"),
               colnames(ptable))
    j <- c(j,
           sort(setdiff(seq_len(ncol(ptable)), j)))
    ptable_rev <- ptable[i, j]
    colnames(ptable_rev) <- colnames(ptable)
    ptable <- rbind(ptable,
                    ptable_rev)
    rownames(ptable) <- NULL
  }
  ptable
}