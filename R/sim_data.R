#' @title Simulate Datasets Based on a Model
#'
#' @description Get a model matrix and
#' effect size specification and
#' simulate a number of datasets,
#' along with other information.
#'
#' @details
#'
#' The function [sim_data()] generates
#' a list of datasets based on a population
#' model.
#'
#' # The role of `sim_data()`
#'
#' The function [sim_data()] is used by
#' the all-in-one function
#' [power4test()]. Users usually do not
#' call this function directly, though
#' developers can use this function to
#' develop other functions for power
#' analysis, or to build their own
#' workflows to do the power analysis.
#'
#' # Workflow
#'
#' The function [sim_data()] does two tasks:
#'
#' - Determine the actual population
#'  model with population values based
#'  on:
#'
#'    - A model syntax for the observed
#'      variables (for a path model)
#'      or the latent factors (for a
#'      latent variable model).
#'
#'    - A textual specification of the
#'      effect sizes of parameters.
#'
#'    - The number of indicators for
#'      each latent factor if the model
#'      is a latent variable model.
#'
#'    - The reliability of each latent
#'      factor as measured by the
#'      indicators if the model is a
#'      latent factor model.
#'
#' - Generate *nrep* simulated datasets
#'  from the population model.
#'
#' The simulated datasets can then be
#' used to fit a model, test
#' parameters, and estimate power.
#'
#' The output is usually used by
#' [fit_model()] to fit a target model,
#' by default the population model, to each
#' of the dataset.
#'
#' # Set 'number_of_indicators' and 'reliability'
#'
#' The arguments `number_of_indicators`
#' and `reliability` are used to
#' specify the number of indicators
#' (e.g., items) for each factor,
#' and the population reliability
#' coefficient of each factor,
#' if the variables in the model
#' syntax are latent variables.
#'
#' ## Single-Group Model
#'
#' If a variable in the model is to be
#' replaced by indicators in the generated
#' data, set
#' `number_of_indicators` to a named
#' numeric vector. The names are the
#' variables of variables with
#' indicators, as appeared in the
#' `model` syntax. The value of each
#' name is the number of indicators.
#'
#' The
#' argument `reliability` should then be
#' set a named numeric vector (or list,
#' see the section on multigroup models)
#' to specify the population reliability
#' coefficient ("omega") of each set of
#' indicators. The population standardized factor
#' loadings are then computed to ensure
#' that the population reliability
#' coefficient is of the target value.
#'
#' These are examples for a single group
#' model:
#'
#' \preformatted{number of indicator = c(m = 3, x = 4, y = 5)}
#'
#' The numbers of indicators for `m`,
#' `x`, and `y` are 3, 4, and 5,
#' respectively.
#'
#' \preformatted{reliability = c(m = .90, x = .80, y = .70)}
#'
#' The population reliability
#' coefficients of `m`, `x`, and `y` are
#' .90, .80, and .70, respectively.
#'
#' ## Multigroup Models
#'
#' Multigroup models are supported.
#' The number of groups is inferred
#' from `pop_es` (see the help page
#' of [ptable_pop()]), or directly
#' from `ptable`.
#'
#' For a multigroup model, the number
#' of indicators for each variable
#' must be the same across groups.
#'
#' However, the population reliability
#' coefficients can be different
#' across groups. For a multigroup model
#' of *k* groups,
#' with one or more population reliability
#' coefficients differ across groups,
#' the argument `reliability` should be
#' set to a named list. The names are
#' the variables to which the population
#' reliability coefficients are to be
#' set. The element for each name is
#' either a single value for the common
#' reliability coefficient, or a
#' numeric vector of the reliability
#' coefficient of each group.
#'
#' This is an example of `reliability`
#' for a model with 2 groups:
#'
#' \preformatted{reliability = list(x = .80, m = c(.70, .80))}
#'
#' The reliability coefficients of `x` are
#' .80 in all groups, while the
#' reliability coefficients of `m` are
#' .70 in one group and .80 in another.
#'
#' ## Equal Numbers of Indicators and/or Reliability Coefficients
#'
#' If all variables in the model has
#' the same number of indicators,
#' `number_of_indicators` can be set
#' to one single value.
#'
#' Similarly, if all sets of indicators
#' have the same population reliability
#' in all groups, `reliability` can also
#' be set to one single value.
#'
#' # Specify The Distributions of Exogenous Variables Or Error Terms Using 'x_fun'
#'
#' By default, variables and error
#' terms are generated
#' from a multivariate normal distribution.
#' If desired, users can supply the
#' function used to generate an exogenous
#' variable and error term by setting `x_fun` to
#' a named list.
#'
#' The names of the list are the variables
#' for which a user function will be used
#' to generate the data.
#'
#' Each element of the list must also
#' be a list. The first element of this
#' list, can be unnamed, is the
#' function to be used. If other arguments
#' need to be supplied, they should be
#' included as named elements of this list.
#'
#' For example:
#'
#' \preformatted{x_fun = list(x = list(power4mome::rexp_rs),
#'              w = list(power4mome::rbinary_rs,
#'                       p1 = .70)))}
#'
#' The variables `x` and `w` will be
#' generated by user-supplied functions.
#'
#' For `x`, the function is
#' `power4mome::rexp_rs`. No additional
#' argument when calling this function.
#'
#' For `w`, the function is
#' `power4mome::rbinary_rx`. The argument
#' `p1 = .70` will be passed to this
#' function when generating the values
#' of `w`.
#'
#' If a variable is an endogenous
#' variable (e.g., being predicted by
#' another variable in a model), then
#' `x_fun` is used to generate its
#' *error term*. Its implied population
#' distribution may still be different
#' from that generate by `x_fun` because
#' the distribution also depends on the
#' distribution of other variables
#' predicting it.
#'
#' These are requirements for the
#' user-functions:
#'
#' - They must return a numeric vector.
#'
#' - They mush has an argument `n` for
#'   the number of values.
#'
#' - The *population* mean and standard
#'   deviation of the generated values
#'   must be 0 and 1, respectively.
#'
#' The package `power4mome` has
#' helper functions for generating
#' values from some common nonnormal
#' distributions and then scaling them
#' to have population mean and standard
#' deviation equal to 0 and 1 (by default), respectively.
#' These are some of them:
#'
#' - [rbinary_rs()].
#'
#' - [rexp_rs()].
#'
#' - [rbeta_rs()].
#'
#' - [rlnorm_rs()].
#'
#' - [rpgnorm_rs()].
#'
#' To use `x_fun`, the variables must
#' have zero covariances with other
#' variables in the population. It is
#' possible to generate nonnormal
#' multivariate data but we believe this
#' is rarely needed when estimating
#' power *before* having the data.
#'
#' @inheritSection ptable_pop Specify the Population Model by 'model'
#'
#' @inheritSection ptable_pop Specify 'pop_es' Using Named Vectors
#'
#' @inheritSection ptable_pop Specify 'pop_es' Using a Multiline String
#'
#' @inheritSection ptable_pop Set the Values for Effect Size Labels ('es1' and 'es2')
#'
#' @seealso [power4test()]
#'
#'
#' @param nrep The number of replications
#' to generate the simulated datasets.
#' Default is 10.
#'
#' @param ptable The output of
#' [ptable_pop()], which is a
#' `ptable_pop` object, representing the
#' population model. If `NULL`, the
#' default, [ptable_pop()] will be
#' called to generate the `ptable_pop`
#' object, using arguments such as
#' `model` and `pop_es`.
#'
#' @param model The `lavaan` model
#' syntax of the population model.
#' Ignored if `ptable` is
#' specified. See [ptable_pop] on
#' how to specify this argument.
#'
#' @param pop_es The character to
#' specify population effect sizes.
#' See [ptable_pop] on
#' how to specify this argument.
#' Ignored if `ptable` is
#' specified.
#'
#' @param ... For [sim_data], parameters
#' to be passed to [ptable_pop()]. For
#' [print.sim_data()], these arguments
#' are ignored.
#'
#' @param n The sample size for each
#' dataset. Default is 100.
#'
#' @param iseed The seed for the random
#' number generator. Default is `NULL`
#' and the seed is not changed.
#'
#' @param number_of_indicators A named
#' vector to specify the number of
#' indicators for each factors. See
#' the help page on how to set this
#' argument. Default is `NULL` and all
#' variables in the model syntax are
#' observed variables.
#' See the help page on how
#' to use this argument.
#'
#' @param reliability A named vector
#' (for a single-group model) or a
#' named list of named vectors
#' (for a multigroup model)
#' to set the reliability coefficient
#' of each set of indicators. Default
#' is `NULL`.
#' See the help page on how
#' to use this argument.
#'
#' @param x_fun The function(s) used to
#' generate the exogenous variables or
#' error terms. If
#' not supplied, or set to `list()`, the
#' default, the variables are generated
#' from a multivariate normal
#' distribution. See the help page on how
#' to use this argument.
#'
#' @param e_fun The function(s) used to
#' generate the error terms of indicators,
#' if any. If
#' not supplied, or set to `list()`, the
#' default, the error terms of indicators
#' are generated
#' from a multivariate normal
#' distribution. Specify in the same
#' way as `x_fun`. Refer to the help
#' page on `x_fun` on how to use this
#' argument.
#'
#' @param parallel If `TRUE`, parallel
#' processing will be used to simulate
#' the datasets. Default is `FALSE`.
#'
#' @param progress If `TRUE`, the progress
#' of data simulation will be displayed.
#' Default is `FALSE.
#'
#' @param ncores The number of CPU
#' cores to use if parallel processing
#' is used.
#'
#' @return
#' The function [sim_out()] returns
#' a list of the class `sim_data`,
#' with length `nrep`. Each element
#' is a `sim_data_i` object, with
#' the following major elements:
#'
#' - `ptable`: A `lavaan` parameter
#'  table of the model, with population
#'  values set in the column `start`.
#'  (It is the output of the
#'  function [ptable_pop()].)
#'
#' - `mm_out`: The population model
#'  represented by model matrices
#'  as in `lavaan`. (It is the output
#'  of the function
#'  [model_matrices_pop()].)
#'
#' - `mm_lm_out`: A list of regression
#'  model formula, one for each
#'  endogenous variable. (It is the
#'  output of the internal function
#'  `mm_lm()`.)
#'
#' - `mm_lm_dat_out`: A simulated dataset
#'  generated from the population model.
#'  (It is the output of the internal
#'  function `mm_lm_data()`).
#'
#' - `model_original`: The original model
#'  syntax (i.e., the argument `model`).
#'
#' - `model_final`: A modified model
#'  syntax if the model is a latent
#'  variable model. Indicators are added
#'  to the syntax.
#'
#' - `fit0`: The output of [lavaan::sem()]
#'  with `ptable` as the model and
#'  `do.fit` set to `FALSE`. Used for the
#'  easy retrieval of information
#'  about the model.
#'
#' @examples
#'
#' # Specify the model
#'
#' mod <-
#' "m ~ x
#'  y ~ m + x"
#'
#' # Specify the population values
#'
#' es <-
#' "
#' y ~ m: m
#' m ~ x: m
#' y ~ x: n
#' "
#'
#' # Generate the simulated datasets
#'
#' data_all <- sim_data(nrep = 5,
#'                      model = mod,
#'                      pop_es = es,
#'                      n = 100,
#'                      iseed = 1234)
#'
#' data_all
#'
#' @export
sim_data <- function(nrep = 10,
                     ptable = NULL,
                     model = NULL,
                     pop_es = NULL,
                     ...,
                     n = 100,
                     iseed = NULL,
                     number_of_indicators = NULL,
                     reliability = NULL,
                     x_fun = list(),
                     e_fun = list(),
                     parallel = FALSE,
                     progress = FALSE,
                     ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {

  if (is.null(ptable)) {
    if (is.null(model) || is.null(pop_es)) {
      stop("Both model and pop_es must be set if ptable is not set.")
    }
    ptable <- ptable_pop(model = model,
                         pop_es = pop_es,
                         ...)
  }
  mm_out <- model_matrices_pop(ptable,
                               drop_list_single_group = FALSE)
  mm_lm_out <- mm_lm(mm_out,
                     drop_list_single_group = FALSE)

  out <- do_FUN(X = seq_len(nrep),
                FUN = sim_data_i,
                ptable = ptable,
                model = model,
                mm_out = mm_out,
                mm_lm_out = mm_lm_out,
                n = n,
                number_of_indicators = number_of_indicators,
                reliability = reliability,
                x_fun = x_fun,
                e_fun = e_fun,
                iseed = iseed,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  class(out) <- c("sim_data", class(out))
  return(out)
}

#' @param digits The numbers of digits
#' displayed after the decimal.
#'
#' @param digits_descriptive The
#' number of digits displayed after
#' the decimal for the descriptive
#' statistics table.
#'
#' @param x The `sim_data` object
#' to be printed.
#'
#' @param data_long If `TRUE`, detailed
#' information will be printed.
#'
#' @param fit_to_all_args A named list
#' of arguments to be passed to
#' [lavaan::sem()] when the model is
#' fitted to a sample combined from
#' all samples stored.
#'
#' @param est_type The type of estimates
#' to be printed. Can be a character
#' vector of one to two elements. If
#' only `"standardized"`, then the
#' standardized estimates are printed.
#' If only `"unstandardized"`, then the
#' unstandardized estimates are printed.
#' If a vector like
#' `c("standardized", "unstandardized")`,
#' then both unstandardized and
#' standardized estimates are printed.
#'
#' @param variances Logical. Whether
#' variances and error variances are printed.
#' Default depends on `est_type`. If
#' `"unstandardized"` is in `est_type`,
#' then default is `TRUE` If
#' only `"standardized"` is in `est_type`,
#' then default is `FALSE`.
#'
#'
#' @return
#' The `print` method of `sim_data`
#' returns `x` invisibly. It is called for
#' its side effect.
#'
#' @rdname sim_data
#' @export
print.sim_data <- function(x,
                           digits = 3,
                           digits_descriptive = 2,
                           data_long = TRUE,
                           fit_to_all_args = list(),
                           est_type = "standardized",
                           variances = NULL,
                           ...) {

  est_type <- match.arg(est_type,
                        choices = c("standardized", "unstandardized"),
                        several.ok = TRUE)

  if (is.null(variances)) {
    if (identical("standardized", est_type)) {
      variances <- FALSE
    } else {
      variances <- TRUE
    }
  }

  # This line needed for printing boot_out and mc_out
  requireNamespace("manymome", quietly = TRUE)

  x_i <- x[[1]]
  ptable <- x_i$ptable
  fit0 <- x_i$fit0

  group_labels <- x_i$group_labels
  ngroups <- length(group_labels)
  if (is.null(ngroups)) ngroups <- 1

  ptable0 <- lavaan::parameterTable(fit0)
  ptable <- set_user_pop(ptable,
                         fit = fit0)
  ptable1 <- ptable[, c("lhs",
                        "op",
                        "rhs",
                        "group",
                        "block",
                        "exo",
                        "label",
                        "start")]
  colnames(ptable1)[which(colnames(ptable1) == "start")] <- "Population"
  if (max(ptable1$group) == 1) {
    ptable1$group <- NULL
  }

  class(ptable1) <- c("lavaan.parameterEstimates", class(ptable1))
  if (ngroups > 1) {
    attr(ptable1, "group.label") <- group_labels
  }

  model_original <- x_i$model_original

  cat(header_str("Model Information",
                 prefix = "\n",
                 suffix = "\n"))

  cat(header_str("Model on Factors/Variables",
                 hw = .4,
                 prefix = "\n",
                 suffix = "\n"))
  cat(model_original)

  model_final <- x_i$model_final
  cat(header_str("Model on Variables/Indicators",
                 hw = .4,
                 prefix = "\n",
                 suffix = "\n"))
  cat(model_final, sep = "\n")

  cat(header_str("Population Values",
                 hw = .4,
                 prefix = "",
                 suffix = "\n"))
  print(ptable1,
        digits = digits)

  # Multigroup models automatically supported
  has_w <- FALSE
  all_ind <- tryCatch(pop_indirect(x),
                       warning = function(w) w)
  if (inherits(all_ind, "warning")) {
    if (grepl("moderator", all_ind$message)) {
      has_w <- TRUE
    }
    all_ind <- suppressWarnings(pop_indirect(x))
  }
  if (length(all_ind) > 0) {
    cat(header_str("Population Indirect Effect(s)",
                  hw = .4,
                  prefix = "",
                  suffix = "\n"))
    print(all_ind,
          digits = digits)
    if (has_w) {
      cat("\nNOTE: One or more path(s) is/are moderated.\n")
    }
    cat("\n")
  }

  k0 <- x_i$number_of_indicators

  rel0 <- x_i$reliability

  if (!is.null(rel0[[1]])) {
    rel <- do.call(rbind,
                  rel0)
    rel <- as.data.frame(rel)
    rel_n <- nrow(rel)
    rownames(rel) <- paste0("Group ", seq_len(rel_n))
    cat(header_str("Population Reliability",
                  hw = .4,
                  prefix = "",
                  suffix = "\n\n"))
    print(rel,
          row.names = isTRUE(rel_n > 1),
          digits = digits)
  }

  if (!is.null(k0[[1]])) {
    lambda0 <- mapply(function(xx, yy) {
                        out <- mapply(lambda_from_reliability,
                                      p = xx,
                                      omega = yy)
                        out
                      },
                      xx = k0,
                      yy = rel0,
                      SIMPLIFY = FALSE)
    lambda <- do.call(rbind,
                      lambda0)
    lambda <- as.data.frame(lambda)
    lambda_n <- nrow(lambda)
    rownames(lambda) <- paste0("Group ", seq_len(lambda_n))
    cat(header_str("Population Standardized Loadings",
                  hw = .4,
                  prefix = "\n",
                  suffix = "\n\n"))
    print(lambda,
          row.names = isTRUE(lambda_n > 1),
          digits = digits)
  }

  # Summarize data

    nrep <- length(x)
    n <- nrow(x_i$mm_lm_dat_out)

    cat(header_str("Data Information",
                  prefix = "",
                  suffix = "\n\n"))

    cat("Number of Replications: ", nrep, "\n")
    cat("Sample Sizes: ", paste0(n, collapse = ", "), "\n")

  if (data_long) {
    all_data <- pool_sim_data(x)

    fit_to_all_args0 <- list(model = model_final,
                             data = all_data,
                             se = "none",
                             test = "none",
                             group = x_i$group_name,
                             fixed.x = FALSE)
    fit_to_all_args1 <- utils::modifyList(fit_to_all_args0,
                                          fit_to_all_args)
    fit_all <- do.call(lavaan::sem,
                       fit_to_all_args1)

    if (isTRUE(identical(est_type, "standardized"))) {
      # Standardized Only
      est_all <- lavaan::standardizedSolution(fit_all,
                                              se = FALSE,
                                              pvalue = FALSE,
                                              ci = FALSE,
                                              output = "text")
      if (!variances) {
        i <- est_all$lhs == est_all$rhs
        est_all <- est_all[!i, ]
      }
      tmp_est_hdr <- "Standardized Estimates"
    }

    if (isTRUE(identical(est_type, "unstandardized"))) {
      # Unstandardized Only
      est_all <- lavaan::parameterEstimates(fit_all,
                                            se = FALSE,
                                            pvalue = FALSE,
                                            ci = FALSE,
                                            standardized = FALSE,
                                            output = "text")
      if (!variances) {
        i <- est_all$lhs == est_all$rhs
        est_all <- est_all[!i, ]
      }
      tmp_est_hdr <- "Unstandardized Estimates"
    }

    if (isTRUE(all(c("unstandardized", "standardized") %in% est_type))) {
      # Both unstandardized and standardized
      est_all <- lavaan::parameterEstimates(fit_all,
                                            se = FALSE,
                                            pvalue = FALSE,
                                            ci = FALSE,
                                            standardized = TRUE,
                                            output = "text")
      if (!variances) {
        i <- est_all$lhs == est_all$rhs
        est_all <- est_all[!i, ]
      }
      tmp_est_hdr <- "Unstandardized and Standardized Estimates"
    }

    cat(header_str("Descriptive Statistics",
                  hw = .4,
                  prefix = "\n",
                  suffix = "\n\n"))

    print(psych::describe(all_data,
                          range = FALSE),
          digits = digits_descriptive)

    tmp <- paste("Parameter Estimates Based on All",
                nrep,
                "Samples Combined")
    cat(header_str(tmp,
                  prefix = "\n",
                  suffix = "\n\n"))

    cat("Total Sample Size:", n * nrep, "\n")

    cat(header_str(tmp_est_hdr,
                  hw = .4,
                  prefix = "\n",
                  suffix = "\n\n"))

    if (!variances) {
      cat("Variances and error variances omitted.\n")
    }

    print(est_all,
          nd = digits)
  } else {
    cat("\nCall print with 'data_long = TRUE' for further information.\n")
  }

  invisible(x)
}

#' @noRd
pool_sim_data <- function(sim_out) {
  all_data <- lapply(sim_out,
                     function(x) x$mm_lm_dat_out)
  all_data <- do.call(rbind,
                      all_data)
  all_data
}

#' @noRd
set_user_pop <- function(ptable,
                         fit) {
  if (!(":=" %in% ptable$op)) {
    return(ptable)
  }
  user_fun <- fit@Model@def.function
  i <- ptable$free
  i <- i[i > 0]
  j <- match(i, ptable$free)
  pop <- ptable$start[j]
  user_pop <- user_fun(pop)
  k <- match(names(user_pop), ptable$lhs)
  ptable[k, "start"] <- user_pop
  ptable
}


#' @noRd
sim_data_i <- function(repid = 1,
                       n = 100,
                       model = NULL,
                       pop_es = NULL,
                       ptable = NULL,
                       mm_out = NULL,
                       mm_lm_out = NULL,
                       number_of_indicators = NULL,
                       reliability = NULL,
                       x_fun = list(),
                       e_fun = list(),
                       seed = NULL,
                       drop_list_single_group = TRUE,
                       merge_groups = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  # TODO:
  # - Set the default values for parameter
  #   not specified (do this in ptable_pop).
  # - Can accept ptable, mm_out, and mm_lm_out, to save the
  #   time in repeating these steps unnecessarily.
  if (is.null(ptable)) {
    ptable <- ptable_pop(model = model,
                        pop_es = pop_es,
                        standardized = TRUE)
  } else {
    model <- attr(ptable, "model")
  }
  if (is.null(mm_out)) {
    mm_out <- model_matrices_pop(ptable,
                                drop_list_single_group = FALSE)
  }
  if (is.null(mm_lm_out)) {
  mm_lm_out <- mm_lm(mm_out,
                     drop_list_single_group = FALSE)
  }

  ngroups <- max(ptable$group)
  if (length(n) == 1) {
    n <- rep(n, ngroups)
  }

  vnames <- lavaan::lavNames(ptable,
                             type = "ov")
  p <- length(vnames)

  if ((length(number_of_indicators) == 1) &&
      is.numeric(number_of_indicators)) {
    number_of_indicators <- rep(number_of_indicators,
                                p)
    names(number_of_indicators) <- vnames
  }
  if (!is.list(number_of_indicators)) {
    number_of_indicators <- rep(list(number_of_indicators),
                                ngroups)
  }

  if ((length(reliability) == 1) &&
      is.numeric(reliability)) {
    reliability <- rep(reliability,
                                p)
    names(reliability) <- vnames
  }
  if (!is.list(reliability)) {
    reliability <- rep(list(reliability),
                            ngroups)
  } else {
    reliability <- split_par_es(reliability)
  }
  mm_lm_dat_out <- mapply(mm_lm_data,
                          object = mm_lm_out,
                          n = n,
                          number_of_indicators = number_of_indicators,
                          reliability = reliability,
                          MoreArgs = list(keep_f_scores = FALSE,
                                          x_fun = x_fun,
                                          e_fun = e_fun),
                          SIMPLIFY = FALSE)

  model_original <- model
  model <- add_indicator_syntax(model,
                                number_of_indicators = number_of_indicators[[1]],
                                reliability = reliability[[1]])
  tmp <- ptable
  tmp$est <- tmp$start
  # fixed.x set to FALSE such that covariances are also displayed
  fit0 <- lavaan::sem(tmp,
              do.fit = FALSE,
              fixed.x = FALSE)
  if (ngroups > 1) {
    group_labels <- names(mm_out)
    group_name <- "group"
  } else {
    group_labels <- NULL
    group_name <- NULL
  }
  if (drop_list_single_group && (ngroups == 1)) {
    mm_out <- mm_out[[1]]
    mm_lm_out <- mm_lm_out[[1]]
    mm_lm_dat_out <- mm_lm_dat_out[[1]]
  }
  if (ngroups > 1) {
    for (i in group_labels) {
      mm_lm_dat_out[[i]]$group <- i
    }
  }
  if (merge_groups && (ngroups > 1)) {
    mm_lm_dat_out <- do.call(rbind,
                             mm_lm_dat_out)
    rownames(mm_lm_dat_out) <- NULL
  }
  out <- list(ptable = ptable,
              mm_out = mm_out,
              mm_lm_out = mm_lm_out,
              mm_lm_dat_out = mm_lm_dat_out,
              model_original = model_original,
              model_final = model,
              fit0 = fit0,
              group_name = group_name,
              group_labels = group_labels,
              number_of_indicators = number_of_indicators,
              reliability = reliability)
  class(out) <- c("sim_data_i", class(out))
  out
}

#' @noRd
pop_indirect <- function(x) {
  x_i <- x[[1]]
  ptable0 <- lavaan::parameterTable(x_i$fit0)
  fit_to_all_args0 <- list(model = x_i$model_final,
                           data = x_i$mm_lm_dat_out,
                           se = "none",
                           test = "none",
                           group = x_i$group_name,
                           fixed.x = FALSE)
  fit_all <- do.call(lavaan::sem,
                      fit_to_all_args0)

  # TODO:
  # - Need a better way to find product terms
  p_terms <- lavaan::lavNames(ptable0, "ov.interaction")

  if (length(p_terms) == 0) {
    p_terms <- NULL
  }

  # Multigroup models automatically supported
  all_paths <- manymome::all_indirect_paths(x[[1]]$fit0,
                                            exclude = p_terms)
  all_ind <- manymome::many_indirect_effects(all_paths,
                                            fit = fit_all,
                                            est = ptable0)

  all_ind
}
