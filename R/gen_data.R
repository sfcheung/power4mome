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
#' using the population model, to each
#' of the dataset.
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
#'  (It is the output of the internal
#'  function `ptable_pop()`.)
#'
#' - mm_out: The population model
#'  represented by model matrices
#'  as in `lavaan`. (It is the output
#'  of the internal function
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
#'  easy retrieve of information
#'  about the model.
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
sim_data <- function(nrep = 10,
                     model,
                     pop_es,
                     n = 100,
                     iseed = NULL,
                     number_of_indicators = NULL,
                     reliability = NULL,
                     parallel = FALSE,
                     progress = FALSE,
                     ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = rep(model, nrep),
                FUN = sim_data_i,
                pop_es = pop_es,
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
sim_data_i <- function(model,
                       pop_es,
                       n = 100,
                       number_of_indicators = NULL,
                       reliability = NULL,
                       seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  # TODO:
  # - Set the default values for parameter
  #   not specified (do this in ptable_pop).
  ptable <- ptable_pop(model = model,
                       pop_es = pop_es,
                       standardized = TRUE)
  mm_out <- model_matrices_pop(ptable)
  mm_lm_out <- mm_lm(mm_out)
  mm_lm_dat_out <- mm_lm_data(mm_lm_out,
                              n = n,
                              number_of_indicators = number_of_indicators,
                              reliability = reliability,
                              keep_f_scores = FALSE)
  model_original <- model
  model <- add_indicator_syntax(model,
                                number_of_indicators = number_of_indicators,
                                reliability = reliability)
  tmp <- ptable
  tmp$est <- tmp$start
  fit0 <- lavaan::sem(tmp,
              do.fit = FALSE)
  out <- list(ptable = ptable,
              mm_out = mm_out,
              mm_lm_out = mm_lm_out,
              mm_lm_dat_out = mm_lm_dat_out,
              model_original = model_original,
              model_final = model,
              fit0 = fit0)
  class(out) <- c("sim_data_i", class(out))
  out
}

