#' @title Create a 'sim_out' Object
#'
#' @description Combine the outputs
#' of [sim_data()], [fit_model()],
#' and optionally [gen_mc()] and/or
#' [gen_boot()] to one
#' single object.
#'
#' @details
#' It merges into one object the output
#' of [sim_data()], which is a list of
#' `nrep` simulated datasets,
#' [fit_model()], which is a list of the
#' [lavaan::sem()] output for the `nrep`
#' datasets, and optionally the output
#' of [gen_mc()] or [gen_boot()], which is a list of the
#' `R` sets of Monte Carlo or bootstrap estimates
#' based on the results of
#' [fit_model()]. The list has `nrep`
#' elements, each element with the data,
#' the model fit
#' results, and optionally the Monte
#' Carlo estimates matched.
#'
#' This object can then be used for testing
#' effects of interests, which are
#' further processed to estimate the
#' power of this test.
#'
#' The function [sim_out()] is used by
#' the all-in-one function
#' [power4test()]. Users usually do not
#' call this function directly, though
#' developers can use this function to
#' develop other functions for power
#' analysis, or to build their own
#' workflows to do the power analysis.
#'
#' @seealso [power4test()]
#'
#' @return
#' The function [sim_out()] returns a `sim_out` object, which
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
#' dats <- sim_data(nrep = 5,
#'                  model = mod,
#'                  pop_es = es,
#'                  n = 100,
#'                  iseed = 1234)
#'
#' # Fit the population model to each dataset
#'
#' fits <- fit_model(dats)
#'
#' # Combine the results to one object
#'
#' sim_out_all <- sim_out(data_all = dats,
#'                        fit = fits)
#' sim_out_all
#'
#' # Verify that the elements of fits are set to extra$fit
#'
#' library(lavaan)
#' parameterEstimates(fits[[1]])
#' parameterEstimates(sim_out_all[[1]]$extra$fit)
#' parameterEstimates(fits[[2]])
#' parameterEstimates(sim_out_all[[2]]$extra$fit)
#'
#' @export
#'
sim_out <- function(data_all,
                    ...) {
  nrep <- length(data_all)
  args <- list(...)
  add_new_extra <- FALSE
  if (length(args) > 0) {
    add_new_extra <- TRUE
    args_by_rep <- split_by_rep(args)
  } else {
    # TODO:
    # - Need to decide whether NA is a
    #   good option. It will not be
    #   added anyway.
    args_by_rep <- rep(NA,
                       nrep)
  }
  tmpfct <- function(x,
                     new_extra = NULL,
                     add_new_extra = FALSE) {
    # Always work, args or not
    if (!add_new_extra) {
      return(x)
    }
    if (!is.null(x$extra)) {
      x$extra <- append(x$extra,
                        new_extra)
      x
    } else {
      x$extra <- new_extra
    }
    x
  }
  out0 <- mapply(tmpfct,
                  x = data_all,
                  new_extra = args_by_rep,
                  MoreArgs = list(add_new_extra = add_new_extra),
                  SIMPLIFY = FALSE)
  class(out0) <- c("sim_out", "sim_data",
                    setdiff(class(out0), c("sim_out", "sim_data")))
  return(out0)
}

#' @param digits The numbers of digits
#' displayed after the decimal.
#'
#' @param digits_descriptive The
#' number of digits displayed after
#' the decimal for the descriptive
#' statistics table.
#'
#' @param fit_to_all_args A named list
#' of arguments to be passed to
#' [lavaan::sem()] when the model is
#' fitted to a sample combined from
#' all samples stored.
#'
#' @param x The `sim_out` object
#' to be printed.
#'
#' @return
#' The `print` method of `sim_out`
#' returns `x` invisibly. Called for
#' its side effect.
#'
#' @rdname sim_out
#' @export
print.sim_out <- function(x,
                          digits = 3,
                          digits_descriptive = 2,
                          fit_to_all_args = list(),
                          ...) {
  NextMethod()
  print_extra(x)
  invisible(x)
}

#' @noRd

print_extra <- function(x) {
  extra <- x[[1]]$extra
  if (is.null(extra)) {
    return(NULL)
  }
  tmp <- sapply(extra,
                function(x) identical(x, NA))
  if (!any(tmp)) {
    return(NULL)
  }
  extra <- extra[!tmp]
  extra_names <- names(extra)
  cat(header_str("Extra Element(s) Found",
                 prefix = "\n",
                 suffix = "\n\n"))
  cat(paste0("- ",
             extra_names),
      sep = "\n")

  cat(header_str("Element(s) of the First Dataset",
                 hw = .5,
                 prefix = "\n",
                 suffix = "\n"))

  for (xx in extra_names) {
    tmp <- paste0("<", xx, ">")
    cat(header_str(tmp,
                   hw = .4,
                   prefix = "\n",
                   suffix = "\n\n"))
    print(extra[[xx]])
  }
  return(NULL)
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