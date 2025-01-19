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
do_test_i <- function(out_i,
                      test_fun,
                      test_args = list(0),
                      fit_name = "fit",
                      mc_out_name = "mc_out",
                      results_fun = NULL,
                      results_args = list()) {
# FUN must be a function that:
# - Extracts the outcome of merge_all(), e.g.,
#   - fit
#   - mc_all (optional)
# FUN_get_results: Extract the following from
# the output of FUN
# - A list with these elements:
#   - est: Scalar
#   - cilo: Scalar
#   - cihi: Scalar
#   - sig: Logical
  args <- list(x = out_i$fit,
               y = out_i$mc_out)
  names(args) <- c(fit_name, mc_out_name)
  args1 <- utils::modifyList(test_args,
                             args)
  out0 <- do.call(test_fun,
                  args1)
  out1 <- do.call(results_fun,
                  c(list(out0), results_args))
  return(out1)
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
do_test <- function(out_all,
                    test_fun,
                    test_args = list(),
                    fit_name = "fit",
                    mc_out_name = "mc_out",
                    results_fun = NULL,
                    results_args = list(),
                    parallel = FALSE,
                    progress = FALSE,
                    ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  out <- do_FUN(X = out_all,
                FUN = do_test_i,
                test_fun = test_fun,
                test_args = test_args,
                fit_name = fit_name,
                mc_out_name = mc_out_name,
                results_fun = results_fun,
                results_args = results_args,
                parallel = parallel,
                progress = progress,
                ncores = ncores)
  return(out)
}
