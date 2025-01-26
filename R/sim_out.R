#' @title Create a 'sim_out' Object
#'
#' @description Combine the outputs
#' of [sim_data()], [fit_model()],
#' and optionally [gen_mc()] to one
#' single object.
#'
#' @details
#' It merges into one object the output
#' of [sim_data()], which is a list of
#' `M` simulated datasets,
#' [fit_model()], which is a list of the
#' [lavaan::sem()] output for the `M`
#' datasets, and optionally the output
#' of [gen_mc()], which is a list of the
#' `M` sets of Monte Carlo estimates
#' based on the results of
#' [lavaan::sem()]. The list has `M`
#' elements, with the data, model fit
#' results, and optionally the Monte
#' Carlo estimates matched.
#'
#' This object can then be used for testing
#' effects of interests, which are
#' further processed to estimate the
#' power of this test.
#'
#' @return
#' It returns an `sim_out` object, which
#' is a list of length equal to the
#' length of `data_all`. Each element
#' of the list is a `sim_data` object
#' with two elements added to it:
#' `fit`, the output of [fit_model()]
#' for this replication, and `mc_out`,
#' the output of [gen_mc()] for this
#' replication, or `NA` if `mc_all`
#' is `NULL.`
#'
#' @param data_all The output of
#' [sim_data()].
#'
#' @param fit_all The output pf
#' [fit_model()].
#'
#' @param mc_all Optional. The out of
#' [gen_mc()].
#'
#' @examples
#' mod <-
#' "m ~ x
#'  y ~ m + x"
#' es <-
#' c("y ~ m" = "m",
#'   "m ~ x" = "m",
#'   "y ~ x" = "n")
#' dats <- sim_data(nrep = 5,
#'                  model = mod,
#'                  pop_es = es,
#'                  n = 100,
#'                  iseed = 1234)
#'
#' fits <- fit_model(dats)
#' sim_out_all <- sim_out(data_all = dats,
#'                        fit_all = fits)
#'
#' @export
#'
sim_out <- function(data_all,
                    fit_all,
                    mc_all = NULL) {
  nrep <- length(data_all)
  if (nrep != length(fit_all)) {
    stop("The numbers of replications do not match.")
  }
  if ((nrep != length(mc_all)) && !is.null(mc_all)) {
    stop("The numbers of replications do not match.")
  }
  if (is.null(mc_all)) {
    mc_all <- rep(NA, nrep)
  }
  tmpfct <- function(x, y, z) {
    x$fit <- y
    x$mc_out <- z
    x
  }
  out0 <- mapply(tmpfct,
                 x = data_all,
                 y = fit_all,
                 z = mc_all,
                 SIMPLIFY = FALSE)
  class(out0) <- c("sim_out", class(out0))
  return(out0)
}
