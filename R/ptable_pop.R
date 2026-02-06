#' @title Generate the Population Model
#'
#' @description Generate the complete
#' population model using the model
#' syntax and user-specified effect
#' sizes (population parameter values).
#'
#' @details
#' The function [ptable_pop()] generates a `lavaan`
#' parameter table that can be used
#' to generate data based on the population
#' values of model parameters.
#'
#' # The role of `ptable_pop()`
#'
#' The function [ptable_pop()] is used by
#' the all-in-one function
#' [power4test()]. Users usually do not
#' call this function directly, though
#' developers can use this function to
#' develop other functions for power
#' analysis, or to build their own
#' workflows to do the power analysis.
#'
#' # Specify the Population Model by 'model'
#'
#' ## Single-Group Model
#'
#' For a single-group model, `model`
#' should be a `lavaan` model syntax
#' string of the *form* of the model.
#' The population values of the model
#' parameters are to be determined by
#' `pop_es`.
#'
#' If the model has latent factors,
#' the syntax in `model` should specify
#' only the *structural model* for the
#' *latent factors*. There is no
#' need to specify the measurement
#' part. Other functions will generate
#' the measurement part on top of this
#' model.
#'
#' For example, this is a simple mediation
#' model:
#'
#' \preformatted{"m ~ x
#'  y ~ m + x"}
#'
#' Whether `m`, `x`, and `y` denote
#' observed variables or latent factors
#' are determined by other functions,
#' such as [power4test()].
#'
#' ## Multigroup Model
#'
#' Because the model is the population
#' model, equality constraints are
#' irrelevant and the model syntax
#' specifies only the *form* of the
#' model. Therefore, `model` is
#' specified as in the case of single
#' group models.
#'
#' # Specify 'pop_es' Using Named Vectors
#'
#' The argument `pop_es` is for specifying
#' the population values of model
#' parameters. This section describes
#' how to do this using named vectors.
#'
#' ## Single-Group Model
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
#' - The names can also be of this form:
#'   `".ind.(<path>)"`, whether `<path>`
#'   denote path in the model. For
#'   example, `".ind.(x->m->y)"` denotes
#'   the path from `x` through `m` to
#'   `y`. Alternatively, the `lavaan`
#'   symbol `~` can also be used:
#'   `".ind.(y~m~x)"`. This form is used
#'   to set the indirect effect (standardized,
#'   by default) along this path. The
#'   value for this name will override
#'   other settings.
#'
#' - If using `lavaan` names, can
#'   specify more than one parameter
#'   using `+`. For example, `y ~ m + x`
#'   denotes the two paths from `m` and
#'   `x` to `y`.
#'
#' - The value of each element can be
#'   the label for the effect size: `n`
#'   for nil, `s` for small, `m` for
#'   medium, and `l` for large. The
#'   value for each label is determined
#'   by `es1` and `es2`. See the section
#'   on specifying these two arguments.
#'
#' - The value of `pop_es` can also be
#'   set to a value, but it must be
#'   quoted as a string, such as `"y ~
#'   x" = ".31"`.
#'
#' This is an example:
#'
#' \preformatted{c(".beta." = "s",
#'   "m1 ~ x" = "-m",
#'   "m2 ~ m1" = "l",
#'   "y ~ x:w" = "s")}
#'
#' In this example,
#'
#' - All regression coefficients are
#'  set to small (`s`) by default, unless
#'  specified otherwise.
#'
#' - The path from `x` to `m1` is
#'  set to medium and negative (`-m`).
#'
#' - The path from `m1` to `m2` is set
#'  to large (`l`).
#'
#' - The coefficient of the product
#'  term `x:w` when predicting `y` is
#'  set to small (`s`).
#'
#' ### Indirect Effect
#'
#' When setting an indirect effect to
#' a symbol (default: `"si"`, `"mi"`,
#' `"li"`, with `"i"` added to differentiate
#' them from the labels for a direct path),
#' the corresponding value is used to
#' determine the population values of
#' *all* component paths along the pathway.
#' All the values are assumed to be equal.
#' Therefore, `".ind.(x->m->y)" = ".20"`
#' is equivalent to setting `m ~ x`
#' and `y ~ m` to the square root of
#' .20, such that the corresponding
#' indirect effect is equal to the
#' designated value.
#'
#' This behavior, though restricted,
#' is for quick manipulation of the
#' indirect effect. If different values
#' along a pathway, set the value for
#' each path directly.
#'
#' Only nonnegative value is supported.
#' Therefore, `".ind.(x->m->y)" = "-si"`
#' and `".ind.(x->m->y)" = "-.20"` will
#' throw an error.
#'
#' ## Multigroup Model
#'
#' The argument `pop_es` also supports multigroup
#' models.
#'
#' For `pop_es`, instead of
#' named vectors, named *list* of
#' named vectors should be used.
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
#' \preformatted{list("m ~ x" = "m",
#'      "y ~ m" = c("s", "m", "l"))}
#'
#' In this model, the population value
#' of the path `m ~ x` is medium (`m`) for
#' all groups, while the population
#' values for the path `y ~ m` are
#' small (`s`), medium (`m`), and large (`l`),
#' respectively.
#'
#' # Specify 'pop_es' Using a Multiline String
#'
#' When setting the argument `pop_es`,
#' instead of using a named vector
#' or named list for
#' `pop_es`, the population values of
#' model parameters can also be
#' specified using a multiline string,
#' as illustrated below, to be parsed
#' by [pop_es_yaml()].
#'
#' ## Single-Group Model
#'
#' This is an example of the multiline string
#' for a single-group model:
#'
#' \preformatted{y ~ m: l
#' m ~ x: m
#' y ~ x: nil}
#'
#' The string must follow this format:
#'
#' - Each line starts with `tag:`.
#'
#'   - `tag` can be the name of a
#'      parameter, in `lavaan` model
#'      syntax format.
#'
#'     - For example, `m ~ x`
#'       denotes the path from `x` to `m`.
#'
#'   - A tag in `lavaan` model syntax can
#'     specify more than one parameter
#'     using `+`.
#'
#'     - For example, `y ~ m + x`
#'       denotes the two paths from `m` and
#'       `x` to `y`.
#'
#'   - Alternatively, the `tag` can be
#'   either `.beta.` or `.cov.`.
#'
#'     - Use `.beta.` to set the default
#'       values for all regression coefficients.
#'
#'     - Use `.cov.` to set the default
#'       values for all correlations of
#'       exogenous variables (e.g., predictors).
#'
#' - After each tag is the value of the
#'   population value:
#'
#'     -`nil` for nil (zero),
#'
#'     - `s` for small,
#'
#'     - `m` for medium, and
#'
#'     - `l` for large.
#'
#'     - `si`, `mi`, and `li` for
#'       small, medium, and large a
#'       standardized indirect effect,
#'       respectively.
#'
#'   Note: `n` *cannot* be used in this mode.
#'
#'   The
#'   value for each label is determined
#'   by `es1` and `es2` as described
#'   in [ptable_pop()].
#'
#'   - The value can also be
#'     set to a numeric value, such as
#'     `.30` or `-.30`.
#'
#' This is another example:
#'
#' \preformatted{.beta.: s
#' y ~ m: l}
#'
#' In this example, all regression
#' coefficients are `small`, while
#' the path from `m` to `y` is large.
#'
#' ## Multigroup Model
#'
#' This is an example of the string
#' for a multigroup model:
#'
#' \preformatted{y ~ m: l
#' m ~ x:
#'   - nil
#'   - s
#' y ~ x: nil}
#'
#' The format is similar to that for
#' a single-group model. If a parameter
#' has the same value for all groups,
#' then the line can be specified
#' as in the case of a single-group
#' model: `tag: value`.
#'
#' If a parameter has different
#' values across groups, then it must
#' be in this format:
#'
#' - A line starts with the tag, followed
#'   by two or more lines. Each line
#'   starts with a hyphen `-` and the
#'   value for a group.
#'
#' For example:
#'
#' \preformatted{m ~ x:
#'   - nil
#'   - s}
#'
#' This denotes that the model has
#' two groups. The values of the path
#' from `x` to `m` for the two
#' groups are 0 (`nil`) and
#' small (`s`), respectively.
#'
#' Another equivalent way to specify
#' the values are using `[]`, on
#' the same line of a tag.
#'
#' For example:
#'
#' \preformatted{m ~ x: [nil, s]}
#'
#' The number of groups is inferred
#' from the number of values for
#' a parameter. Therefore, if a tag
#' has more than one value, each tag
#' must has the same number of value,
#' or only one value.
#'
#' The tag `.beta.` and `.cov.` can
#' also be used for multigroup models.
#'
#' ## Which Approach To Use
#'
#' Note that using named vectors or
#' named lists is more reliable. However,
#' using a multiline string is
#' more user-friendly. If this method
#' failed, please use named vectors or
#' named list instead.
#'
#' ## Technical Details
#'
#' The multiline string is parsed by [yaml::read_yaml()].
#' Therefore, the format requirement
#' is actually that of YAML. Users
#' knowledgeable of YAML can use other
#' equivalent way to specify the string.
#'
#' # Set the Values for Effect Size Labels ('es1' and 'es2')
#'
#' The vector `es1` is for correlations,
#' regression coefficients, and
#' indirect effect, and the
#' vector `es2` is for for standardized
#' moderation effect, the coefficients
#' of a product term. These labels
#' are to be used in interpreting
#' the specification in `pop_es`.
#'
#' @seealso [power4test()], and
#' [pop_es_yaml()] on an alternative
#' way to specify population values.
#'
#' @return
#' The function [ptable_pop()] returns
#' a `lavaan` parameter table of the
#' model, with the column `start` set to the
#' population values.
#'
#' @param model String. The model defined
#' by `lavaan` model syntax. See 'Details'.
#'
#' @param pop_es It can be a data frame
#' with these columns: `lhs`, `op`,
#' `rhs`, and `pop`. The first three
#' columns correspond to those in a
#' `lavaan` parameter table. The column
#' `pop` stores the population values.
#' The column `es` stores the original
#' labels, for reference. It can also be
#' a named character vector (named list
#' for multigroup models) or a multiline string,
#' which are
#' the preferred approaches. See the
#' help page
#' on how to specify this vector.
#'
#' @param es1 Set the
#' values for each label of the effect
#' size (population value) for correlations and regression
#' paths.
#' Used only if `pop_es` is a named
#' vector or a multiline string.
#' See the help page on how to specify
#' this argument.
#'
#' @param es2 Set the
#' values for each label of the effect
#' size (population value) for product term.
#' Used only if `pop_es` is a named
#' vector or a multiline string.
#' See the help page on how to specify
#' this argument.
#'
#' @param es_ind The names of labels
#' denoting the effect size of an
#' indirect effect. They will be
#' used to determine the population
#' values of the component paths along
#' an indirect path.
#'
#' @param standardized Logical. If
#' `TRUE`, the default, variances and
#' error variances are scaled to ensure
#' the population variances of the
#' endogenous variables are close to
#' one, and hence the effect sizes
#' (population values) are
#' standardized effect sizes if the
#' variances of the continuous exogenous
#' variables are also equal to one.
#'
#' @param n_std The sample size used to
#' determine the error variances by
#' simulation when `std_force_monte_carlo`
#' is `TRUE`.
#'
#' @param std_force_monte_carlo Logical.
#' If `FALSE`, the default,
#' standardization is done analytically
#' if the model has no product terms,
#' and by simulation if the model has
#' product terms. That is, error variances
#' required to ensure implied variances equal
#' to one are determined by simulation.
#' If `TRUE`, simulation
#' will be used whether the model has
#' product terms or not. Always fall
#' back to simulation if
#' analytical standardization failed.
#'
#' @param add_cov_for_moderation
#' Logical. If `TRUE`, the default, for
#' a model in which one or product terms
#' for moderation involve one or more
#' mediator, covariances between their
#' error terms and the product terms
#' will be added automatically. If these
#' covariances are not added, the model
#' may not be invariant to linear
#' transformation of some variables in
#' the model.
#'
#' @examples
#'
#' # Specify the model
#'
#' model1 <-
#' "
#' m1 ~ x + c1
#' m2 ~ m1 + x2 + c1
#' y ~  m2 + m1 + x + w + x:w + c1
#' "
#'
#' # Specify the population values
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
#' # Use a multiline string, illustrated by a simpler model
#'
#' model2 <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' model2_es_a <- c("m ~ x" = "s",
#'                  "y ~ m" = "m",
#'                  "y ~ x" = "nil")
#'
#' model2_es_b <-
#' "
#' m ~ x: s
#' y ~ m: m
#' y ~ x: nil
#' "
#'
#' ptable_model2_a <- ptable_pop(model2,
#'                               pop_es = model2_es_a)
#' ptable_model2_b <- ptable_pop(model2,
#'                               pop_es = model2_es_b)
#'
#' ptable_model2_a
#' ptable_model2_b
#'
#' identical(ptable_model2_a,
#'           ptable_model2_b)
#'
#' @export
# Input:
# - model: The model syntax
# - pop_es: The parameter table of parameters with population values OR
#           A named vector of the effect size labels for selected parameters
# Output:
# - The parameter table with population values
ptable_pop <- function(model = NULL,
                       pop_es = NULL,
                       es1 = c("n" = .00,
                               "nil" = .00,
                               "s" = .10,
                               "m" = .30,
                               "l" = .50,
                               "si" = .141,
                               "mi" = .361,
                               "li" = .510,
                               "sm" = .20,
                               "ml" = .40
                               ),
                       es2 = c("n" = .00,
                               "nil" = .00,
                               "s" = .05,
                               "m" = .10,
                               "l" = .15,
                               "sm" = .075,
                               "ml" = .125),
                       es_ind = c("si",
                                  "mi",
                                  "li"),
                       standardized = TRUE,
                       n_std = 100000,
                       std_force_monte_carlo = FALSE,
                       add_cov_for_moderation = TRUE) {
  if (is.null(model) || is.null(pop_es)) {
    stop("Both model and pop_es must be set.")
  }
  # pop_es a YAML string? If yes, convert it
  pop_es <- pop_es_yaml_check(pop_es)

  par_pop <- pop_es2par_pop(pop_es = pop_es,
                            es1 = es1,
                            es2 = es2,
                            model = model,
                            to_one_table = TRUE,
                            es_ind = es_ind)
  ngroups <- max(par_pop$group)
  # Single group ptable
  if (ngroups > 1) {
    # Handle labels in the syntax
    tmp <- lavaan::lavParseModelString(model,
                                       as.data.frame. = TRUE)
    tmp$label <- ""
    tmp$mod.idx <- 0
    attr(tmp, "modifiers") <- list()
    attr(tmp, "constraints") <- list()
    fit0 <- lavaan::sem(tmp,
                        do.fit = FALSE,
                        fixed.x = FALSE)
    ptable0 <- lavaan::parTable(fit0)
  } else {
    fit0 <- lavaan::sem(model,
                        do.fit = FALSE,
                        fixed.x = FALSE)
    ptable0 <- lavaan::parTable(fit0)
  }

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
                        group.label = gpnames,
                        fixed.x = FALSE)
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
    std_by_monte_carlo <- attr(mm[[i]]$psi, "std_by_monte_carlo")
    if (is.null(std_by_monte_carlo)) {
      # For backward compatibility
      std_by_monte_carlo <- NA
    }
    ptable1 <- start_from_mm(ptable1,
                             mm)
    attr(ptable1, "model") <- model
    attr(ptable1, "std_by_monte_carlo") <- std_by_monte_carlo
  }
  if ((length(m_moderated(model, ngroups = ngroups)) > 0) &&
      add_cov_for_moderation) {
    # Models with mediators involved in moderation
    ptable_fixed <- pt_with_int(
                      ptable = ptable1,
                      model_original = model
                    )
    ptable1 <- ptable_fixed
    if (utils::packageVersion("lavaan") > "0.6.19") {
      # lavaan 0.6.20+ should support "x:w ~~ y:z"
      attr(ptable1, "model") <- attr(ptable1, "model_fixed")
    }
  }

  # It is intentional not saving the call and
  # saving the argument values.
  # For the ease of updating.
  attr(ptable1, "pop_es") <- pop_es
  attr(ptable1, "es1") <- es1
  attr(ptable1, "es2") <- es2
  attr(ptable1, "es_ind") <- es_ind
  # par_pop is one single table with a group column
  attr(ptable1, "par_pop") <- par_pop
  attr(ptable1, "n_std") <- n_std
  attr(ptable1, "standardized") <- standardized
  attr(ptable1, "std_force_monte_carlo") <- std_force_monte_carlo

  class(ptable1) <- c("ptable_pop", class(ptable1))
  ptable1
}

#' @noRd
# Should only update pop_es
# Input:
# - A ptable object
# - new_pop_es: pop_es for parameters to be updated
# Output:
# - A ptable based on updated pop_es
update_ptable_pop <- function(object,
                              new_pop_es) {
  if (!inherits(object, "ptable_pop")) {
    stop("Can only update a ptable_pop object.")
  }
  es1 <- attr(object, "es1")
  es2 <- attr(object, "es2")
  es_ind <- attr(object, "es_ind")
  model <- attr(object, "model")
  old_par_pop <- attr(object, "par_pop")
  if (is.data.frame(old_par_pop)) {
    old_par_pop <- split_par_pop(old_par_pop)
  }
  new_par_pop <- pop_es2par_pop(new_pop_es,
                                es1 = es1,
                                es2 = es2,
                                model = model,
                                es_ind = es_ind)
  updated_par_pop <- update_par_pop(add = new_par_pop,
                                    par_pop = old_par_pop)
  updated_par_pop <- par_pop_to_one_table(updated_par_pop)
  out <- ptable_pop(model = model,
                    pop_es = updated_par_pop,
                    es1 = es1,
                    es2 = es2,
                    standardized = attr(object, "standardized"),
                    n_std = attr(object, "n_std"),
                    std_force_monte_carlo = attr(object, "std_force_monte_carlo"))
  out
}

#' @noRd
# Input:
# - par_pop
# Output:
# - A list of par_pops, one for each
#   group, even if the number of groups
#   is 1.
split_par_pop <- function(par_pop) {
  # Already a list of table(s)?
  if (is.list(par_pop)) {
    if (is.data.frame(par_pop[[1]])) {
      return(par_pop)
    }
  }
  out <- par_pop
  out$group <- NULL
  out1 <- split(out,
                f = par_pop$group)
  names(out1) <- NULL
  out1
}

#' @details
#'
#' # The role of `model_matrices_pop()`
#'
#' The function [model_matrices_pop()]
#' generates models matrices with
#' population values, used by [ptable_pop()].
#' Users usually do not
#' call this function directly, though
#' developers can use this build their own
#' workflows to generate the data.
#'
#' @return
#' The function [model_matrices_pop()]
#' returns a `lavaan` LISREL-style model
#' matrices (like the output of
#' [lavaan::lavInspect()] with `what`
#' set to `"free"`), with matrix elements
#' set to the population values. If
#' `x` is the model syntax, it will be
#' stored in the attributes `model`.
#' If the model is a multigroup model
#' with *k* groups (*k* greater than 1),
#' then it returns a list of *k* lists
#' of `lavaan` LISREL-style model
#' matrices unless `drop_list_single_group`
#' is `TRUE`.
#'
#' @param x It can be a 'lavaan' model
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
#' `TRUE` and the number of groups is
#' equal to one, the output will be
#' a list of matrices of one group
#' only. Default if `TRUE`.
#'
#' @examples
#' # model_matrices_pop
#'
#' model_matrices_pop(ptable_final1)
#'
#' model_matrices_pop(model1,
#'                    pop_es = model1_es)
#'
#' @rdname ptable_pop
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
# - es1
# - es2
# - model
# Output:
# - par_pop
update_par_pop <- function(add,
                           par_pop) {
  # par_pop should be a list of tables
  out <- par_pop
  ngroups <- length(out)
  for (i in seq_len(ngroups)) {
    # Can ignore group because
    # each group has its own table
    tmp <- merge(out[[i]],
                 add[[i]],
                 by = c("lhs", "op", "rhs"),
                 all.x = TRUE,
                 all.y = FALSE,
                 suffix = c("", ".add"))
    i2 <- !is.na(tmp$pop.add)
    i3 <- !is.na(tmp$es.add)
    tmp$pop[i2] <- tmp$pop.add[i2]
    tmp$es[i3] <- tmp$es.add[i3]
    tmp$pop.add <- NULL
    tmp$es.add <- NULL
    out[[i]] <- tmp
  }
  # The output is always a list
  out
}

#' @noRd
# Input:
# - pop_es
# - Other arguments needed.
# Output:
# - par_pop. Can be a list or a table.
pop_es2par_pop <- function(pop_es,
                           es1,
                           es2,
                           model,
                           to_one_table = FALSE,
                           es_ind) {
  # Always process par_pop as a list until existing
  if (is.character(pop_es) || is.numeric(pop_es)) {
    pop_es <- fix_par_es(pop_es,
                         model = model)
    par_pop <- set_pop(pop_es,
                       es1 = es1,
                       es2 = es2,
                       es_ind = es_ind)
    par_pop <- list(par_pop)
  } else if (is.list(pop_es)) {
    if (is.data.frame(pop_es)) {
      # A one-table par_pop
      # Convert to a list
      par_pop <- split_par_pop(pop_es)
    } else if (is.data.frame(pop_es[[1]])) {
      # Already a list of table(s)
      par_pop <- pop_es
    } else {
      # A list of character vectors
      pop_es <- split_par_es(pop_es)
      pop_es <- lapply(pop_es,
                      FUN = fix_par_es,
                      model = model)
      par_pop <- lapply(pop_es,
                        set_pop,
                        es1 = es1,
                        es2 = es2,
                        es_ind = es_ind)
    }
  }
  par_pop <- lapply(par_pop,
                    dup_cov)
  if (to_one_table) {
    par_pop <- par_pop_to_one_table(par_pop)
  }
  par_pop
}

#' @noRd
# Input:
# - par_pop
# Output:
# - One single table
par_pop_to_one_table <- function(par_pop) {
  # Already one table?
  if (is.data.frame(par_pop)) {
    return(par_pop)
  }
  ngroups <- length(par_pop)
  for (i in seq_along(par_pop)) {
    par_pop[[i]]$group <- i
  }
  par_pop <- do.call(rbind,
                     par_pop)
  rownames(par_pop) <- NULL
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
  # Handle labels
  tmp <- lavaan::lavParseModelString(model,
                                     as.data.frame. = TRUE)
  tmp$label <- ""
  tmp$mod.idx <- 0
  attr(tmp, "modifiers") <- list()
  attr(tmp, "constraints") <- list()
  fit1 <- lavaan::sem(tmp,
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