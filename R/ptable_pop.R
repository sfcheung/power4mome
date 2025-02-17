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
#' ## Multigroup Models
#'
#' The function also support multigroup
#' models.
#'
#' Because the model is the population
#' model, equality constraints are
#' irrelevant and the model syntax
#' specifies only the *form* of the
#' model. Therefore, `model` is
#' specified as in the case of single
#' group models.
#'
#' For `pop_es`, instead of using a
#' named vectors, use as named *list*.
#'
#' - The names are the parameters, or
#'   keywords such as `.beta.` and
#'   `.cov.`, like specifying the
#'   population values for a single
#'   group model.
#'
#' - The elements are character vectors.
#'   If it has only one element (e.g.,
#'   a single string), then it is the
#'   the population value for all groups.
#'   If it has more than one element
#'   (e.g., a vector of three strings),
#'   then they are the population values
#'   of the groups. For a model of *k*
#'   groups, each vector must have
#'   either *k* elements or one element.
#'
#' This is an example:
#'
#' `list("m ~ x" = "m", "y ~ m" = c("s", "m", "l"))`
#'
#' In this model, the population value
#' of the path `m ~ x` is medium for
#' all groups, while the population
#' values for the path `y ~ m` are
#' small, medium, and large,
#' respectively.
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
  par_pop <- pop_es2par_pop(pop_es = pop_es,
                            es1 = es1,
                            es2 = es2,
                            model = model)
  # par_pop <- dup_cov(par_pop)
  par_pop <- lapply(par_pop,
                    dup_cov)
  ngroups <- length(par_pop)
  for (i in seq_along(par_pop)) {
    par_pop[[i]]$group <- i
  }
  par_pop <- do.call(rbind,
                     par_pop)
  # Single group ptable
  fit0 <- lavaan::sem(model,
                      do.fit = FALSE)
  ptable0 <- lavaan::parTable(fit0)

  # Use fake data to create the target parameter table
  if (ngroups > 1) {
    ptable0_ng <- ptable0
    gpnames <- paste0("gp", seq_len(ngroups))
    vnames <- lavaan::lavNames(ptable0,
                               type = "ov")
    p <- length(vnames)
    d1 <- diag(p)
    colnames(d1) <- rownames(d1) <- vnames
    dat_cov <- lapply(seq_len(ngroups),
                      function(x) d1)
    fit0 <- lavaan::sem(model,
                        sample.cov = dat_cov,
                        sample.nobs = rep(10000, ngroups),
                        do.fit = FALSE,
                        group.label = gpnames)
    ptable0 <- lavaan::parTable(fit0)

    # Fix starting values from MG
    ptable0$tmp1 <- paste0(ptable0$lhs,
                           ptable0$op,
                           ptable0$rhs)
    ptable0_ng$tmp1 <- paste0(ptable0_ng$lhs,
                              ptable0_ng$op,
                              ptable0_ng$rhs)
    ptable0_ng$start_ng <- ptable0_ng$start
    ptable0_ng$est_ng <- ptable0_ng$est
    pt_tmp <- merge(ptable0,
                    ptable0_ng[, c("tmp1", "start_ng", "est_ng")],
                    all.x = TRUE,
                    all.y = FALSE,
                    sort = FALSE)
    i <- !is.na(pt_tmp$start_ng)
    pt_tmp$start[i] <- pt_tmp$start_ng[i]
    i <- !is.na(pt_tmp$est_ng)
    pt_tmp$est[i] <- pt_tmp$est_ng[i]
    ptable0 <- pt_tmp[, colnames(ptable0)]
    ptable0 <- ptable0[order(ptable0$id), ]
    ptable0$tmp1 <- NULL
    rownames(ptable0) <- NULL
  }

  # Check par_pop
  vnames <- lavaan::lavNames(ptable0,
                             type = "ov")
  vnamee_par_pop <- lavaan::lavNames(par_pop,
                                     type = "ov")
  chk_names <- setdiff(vnamee_par_pop,
                       vnames)
  if (length(chk_names) > 0) {
    tmp <- paste0(chk_names,
                  collapse = ", ")
    msg <- paste(tmp,
                 "is/are in pop_es but not in the model,",
                 "and parameter(s) involved is/are ignored.")
    warning(msg)
  }

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
  par_pop2 <- par_pop2[, c("id", "lhs", "op", "rhs", "group", "pop")]

  ptable1 <- merge(ptable0,
                   par_pop2,
                   all.x = TRUE)
  ptable1[!is.na(ptable1$pop), "start"] <- ptable1[!is.na(ptable1$pop), "pop"]
  ptable1$pop <- NULL
  # TODO:
  # - Check equality constraints
  attr(ptable1, "model") <- model
  if (standardized) {
    mm <- model_matrices_pop(ptable1,
                             drop_list_single_group = FALSE)
    if (ncol(mm[[1]]$psi) != 0) {
      for (i in seq_along(mm)) {
        mm[[i]]$psi <- psi_std(mm[[i]],
                               n_std = n_std)
      }
    } else {
      # CFA model or correlation only
      # Make sure theta is a covariance matrix,
      # in case variances accidentally set.
      for (i in seq_along(mm)) {
        mm[[i]]$theta <- stats::cov2cor(mm[[i]]$theta)
      }
    }
    ptable1 <- start_from_mm(ptable1,
                             mm)
    attr(ptable1, "model") <- model
  }

  # It is intentional not saving the call
  # saving the argument values.
  attr(ptable1, "pop_es") <- pop_es
  attr(ptable1, "es1") <- es1
  attr(ptable1, "es2") <- es2
  attr(ptable1, "par_pop") <- par_pop

  class(ptable1) <- c("ptable1", class(ptable1))
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
#' If the model is a multigroup model
#' with *k* groups (*k* greater than 1),
#' then it returns a list of *k* lists
#' of `lavaan` LISREL-style model
#' matrices.
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
#' @param drop_list_single_group If
#' `TRUE` and the number groups is
#' equal to one, the output will be
#' a list of matrices of one group
#' only. Default if `TRUE`.
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
                               ...,
                               drop_list_single_group = TRUE) {
  if (is.character((x))) {
    ptable <- ptable_pop(model = x,
                         ...)
  } else {
    ptable <- x
  }
  fit1 <- lavaan::sem(ptable,
                      do.fit = FALSE)
  ngroups <- lavaan::lavInspect(fit1,
                                "ngroups")
  mm <- lavaan::lavInspect(fit1,
                           "partable",
                           drop.list.single.group = FALSE)
  mm2 <- sapply(mm,
                set_start,
                ptable = ptable,
                simplify = FALSE)
  attr(mm2, "header") <- NULL
  if (is.character((x))) {
    attr(mm2, "model") <- x
    for (i in seq_along(mm2)) {
      attr(mm2[[i]], "model") <- x
    }
  } else {
    attr(mm2, "model") <- attr(x, "model")
    for (i in seq_along(mm2)) {
      attr(mm2[[i]], "model") <- attr(x, "model")
    }
  }
  if (drop_list_single_group && (ngroups == 1)) {
    mm2 <- mm2[[1]]
  }
  mm2
}

#' @noRd
# Update par_pop
# Input:
# - par_pop
# - pop_es


#' @noRd
pop_es2par_pop <- function(pop_es,
                           es1,
                           es2,
                           model) {
  if (is.character(pop_es)) {
    pop_es <- fix_par_es(pop_es,
                         model = model)
    par_pop <- set_pop(pop_es,
                       es1 = es1,
                       es2 = es2)
    par_pop <- list(par_pop)
  } else if (is.list(pop_es)) {
    pop_es <- split_par_es(pop_es)
    pop_es <- lapply(pop_es,
                     FUN = fix_par_es,
                     model = model)
    par_pop <- lapply(pop_es,
                      set_pop,
                      es1 = es1,
                      es2 = es2)
  } else {
    par_pop <- pop_es
  }
  par_pop
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
mm_lm <- function(mm,
                  drop_list_single_group = TRUE) {
  # TODO:
  # - Find a more robust way to check
  #   the number of groups.
  if (is.null(attr(mm[[1]], "model"))) {
    out <- mm_lm_i(mm)
    if (!drop_list_single_group) {
      out <- list(out)
    }
  } else {
    out <- lapply(mm,
                  mm_lm_i)
  }
  return(out)
}

#' @noRd
mm_lm_i <- function(mm) {
  # TODO:
  # - Check whether the transpose of nox-beta is in echelon form.
  # - Handle models with no y-variables (e.g., CFA).
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