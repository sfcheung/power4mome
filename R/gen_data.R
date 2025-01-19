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
#' @export
#'
gen_data_i <- function(model,
                       pop_es,
                       n = 100,
                       number_of_indicators = NULL,
                       reliability = NULL,
                       seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
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
  out
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
#' @export
#'
gen_data <- function(nrep = 10,
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
                FUN = gen_data_i,
                pop_es = pop_es,
                n = n,
                number_of_indicators = number_of_indicators,
                reliability = reliability,
                iseed = iseed,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  return(out)
}
