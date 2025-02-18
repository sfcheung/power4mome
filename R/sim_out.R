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
#' with the element `extra` added to
#' it. Other named elements will be
#' added under this name. For example.
#' the output of [fit_model()]
#' for this replication can be added
#' to `fit`, under `extra`. See
#' the description of the argument
#' `...` for details.
#'
#' @param data_all The output of
#' [sim_data()].
#'
#' @param ... Named arguments of
#' objects to be added to each
#' replication under the element
#' `extra`. For example, if set to
#' `fit = fit_all`, where `fit_all`
#' is the output of [fit_model()],
#' then `data_all[[1]]$extra$fit`
#' will be set to the first output
#' in `fit_all`.
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
#'                        fit = fits)
#'
#' @export
#'
sim_out <- function(data_all,
                    ...) {
  nrep <- length(data_all)
  args <- list(...)
  if (length(args) == 0) {
    args_by_rep <- rep(NA, nrep)
  } else {
    args_by_rep <- split_by_rep(args)
  }
  tmpfct <- function(x, y, z,
                     extra = NULL) {
    x$extra <- extra
    x
  }
  out0 <- mapply(tmpfct,
                 x = data_all,
                 extra = args_by_rep,
                 SIMPLIFY = FALSE)
  class(out0) <- c("sim_out", class(out0))
  return(out0)
}

#' @noRd
# Process optional arguments
split_by_rep <- function(args) {
  arg_names <- names(args)
  if (is.null(arg_names)) {
    stop("The additional arguments must be named.")
  }
  nrep <- sapply(args,
                 length)
  if (!all(nrep == max(nrep))) {
    tmp <- paste(arg_names, collapse = ", ")
    stop("The numbers of replications in <",
         tmp,
         "> must be the same.")
  }
  tmpfct <- function(...,
                     arg_names) {
    list(...)
  }
  out <- do.call(mapply,
                 c(list(FUN = tmpfct),
                   args,
                   list(SIMPLIFY = FALSE)))
  out
}