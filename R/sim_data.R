#' @title Simulate Datasets Based on a Model
#'
#' @description Get a model matrix and
#' effect size specification and
#' simulate a number of datasets,
#' along with other information.
#'
#' @details
#' It does two tasks:
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
#' - Generate *m* simulated datasets
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
#' ## Setting `model` and `pop_es`
#'
#' Please refer to help page of
#' [ptable_pop()] on how to specify
#' `model` and `pop_es`.
#'
#' ## Setting `number_of_indicators` and `reliability`
#'
#' If a variable in the model is to be
#' replaced by indicators, set
#' `number_of_indicators` to a named
#' numeric vector. The names are the
#' variables of variables with
#' indicators, as appeared in the
#' `model` syntax. The value of each
#' name is the number of indicators. The
#' argument `reliability` should then be
#' set a named numeric vector (or list,
#' see the section on multigroup models)
#' to specify the population reliability
#' coefficients ("omega") of each set of
#' indicators. The population factor
#' loadings are then computed to ensure
#' that the population reliability
#' coefficient is of the target value.
#'
#' These are examples for a single group
#' model:
#'
#' `number of indicator = c(m = 3, x = 4, y = 5)`
#'
#' The numbers of indicators for `m`,
#' `x`, and `y` are 3, 4, and 5,
#' respectively.
#'
#' `reliability = c(m = .90, x = .80, y = .70)`
#'
#' The population reliability
#' coefficients of `m`, `x`, and `y` are
#' .90, .80, and .70, respectively.
#'
#' ### Multigroup Models
#'
#' Multigroup models are supported.
#' The number of groups is inferred
#' from `pop_es` (see the help page
#' of [ptable_pop()]).
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
#' `reliability = list(x = .80, m = c(.70, .80))`
#'
#' The reliability coefficients of `x` are
#' .80 in all groups, while the
#' reliability coefficients of `m` are
#' .70 in one group and .80 in another.
#'
#' @param nrep The number of replications
#' to generate the simulated datasets.
#' Default is 10.
#'
#' @param model The `lavaan` model
#' syntax of the population model.
#' Required.
#'
#' @param pop_es The character to
#' specify population effect sizes.
#' See 'Details' on how to set the
#' effect sizes for this argument.
#' Required.
#'
#' @param ... Parameters to be passed
#' to [ptable_pop].
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
#' 'Details' on how to set this
#' argument. Default is `NULL` and all
#' variables in the model syntax are
#' observed variables.
#'
#' @param reliability A named vector
#' to set the reliability coefficient
#' of each set of indicators. Default
#' is `NULL`.
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
#' A list of the class `sim_data`,
#' with length `nrep`. Each element
#' is a `sim_data_i` object, with
#' the following major elements:
#'
#' - ptable: A `lavaan` parameter
#'  table of the model, with population
#'  values set in the column `start`.
#'  (It is the output of the
#'  function `ptable_pop()`.)
#'
#' - mm_out: The population model
#'  represented by model matrices
#'  as in `lavaan`. (It is the output
#'  of the function
#' `model_matrices_pop()`.)
#'
#' - mm_lm_out: A list of regression
#'  model formula, one for each
#'  endogenous variable. (It is the
#'  output of the internal function
#' `mm_lm()`.)
#'
#' - mm_lm_dat_out: A simulated dataset
#'  generated from the population model.
#'  (It is the output of the internal
#'  function `mm_lm_data()`).
#'
#' - model_original: The original model
#'  syntax (i.e., the argument `model`).
#'
#' - model_final: A modified model
#'  syntax if the model is a latent
#'  variable model. Indicators are added
#'  to the syntax.
#'
#' - fit0: The output of [lavaan::sem()]
#'  with `ptable` as the model and
#'  `do.fit` set to `FALSE`. Use for
#'  easy retrieval of information
#'  about the model.
#'
#' @examples
#' mod <-
#' "m ~ x
#'  y ~ m + x"
#' es <-
#' c("y ~ m" = "m",
#'   "m ~ x" = "m",
#'   "y ~ x" = "n")
#' data_all <- sim_data(nrep = 5,
#'                      model = mod,
#'                      pop_es = es,
#'                      n = 100,
#'                      iseed = 1234)
#'
#' @export
sim_data <- function(nrep = 10,
                     model,
                     pop_es,
                     ...,
                     n = 100,
                     iseed = NULL,
                     number_of_indicators = NULL,
                     reliability = NULL,
                     parallel = FALSE,
                     progress = FALSE,
                     ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {

  ptable <- ptable_pop(model = model,
                       pop_es = pop_es,
                       ...)
  mm_out <- model_matrices_pop(ptable,
                               drop_list_single_group = FALSE)
  mm_lm_out <- mm_lm(mm_out,
                     drop_list_single_group = FALSE)

  out <- do_FUN(X = rep(model, nrep),
                FUN = sim_data_i,
                ptable = ptable,
                mm_out = mm_out,
                mm_lm_out = mm_lm_out,
                n = n,
                number_of_indicators = number_of_indicators,
                reliability = reliability,
                iseed = iseed,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  class(out) <- c("sim_data", class(out))
  return(out)
}


#' @title Title In Title Case
#'
#' @description One paragraph description.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @examples
#' \donttest{
#' }
#'
#' @noRd
sim_data_i <- function(model = NULL,
                       pop_es = NULL,
                       ptable = NULL,
                       mm_out = NULL,
                       mm_lm_out = NULL,
                       n = 100,
                       number_of_indicators = NULL,
                       reliability = NULL,
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
  if (!is.list(number_of_indicators)) {
    number_of_indicators <- rep(list(number_of_indicators),
                                ngroups)
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
                          MoreArgs = list(keep_f_scores = FALSE),
                          SIMPLIFY = FALSE)

  model_original <- model
  model <- add_indicator_syntax(model,
                                number_of_indicators = number_of_indicators[[1]],
                                reliability = reliability[[1]])
  tmp <- ptable
  tmp$est <- tmp$start
  fit0 <- lavaan::sem(tmp,
              do.fit = FALSE)
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
              group_labels = group_labels)
  class(out) <- c("sim_data_i", class(out))
  out
}

