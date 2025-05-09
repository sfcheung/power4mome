#' @title Power By Sample Sizes
#'
#' @description Estimate power for a
#' range of sample sizes.
#'
#' @details This function regenerates
#' datasets for a set of sample sizes
#' and does the stored tests in each of
#' them.
#'
#' Optionally, it can also be run
#' on a object with no stored tests.
#' In this case, additional arguments
#' must be set to instruct [power4test()]
#' on the tests to be conducted.
#'
#' It is usually used to examine the
#' power over a range of sample sizes.
#'
#' @return The function
#' [power4test_by_n()] returns a
#' `power4test_by_n` object, which is a
#' list of `power4test` objects, one for
#' each sample size.
#'
#' @param object A `power4test` object.
#'
#' @param n A numeric vector of sample
#' sizes for which the simulation will
#' be conducted.
#'
#' @param progress Logical. Whether
#' progress of the simulation will be
#' displayed.
#'
#' @param ... For [power4test_by_n()],
#' they are arguments to be passed
#' to [power4test()]. For [c.power4test_by_n()],
#' they are [power4test_by_n()] outputs
#' to be combined together.
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#'
#' model_simple_med <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' model_simple_med_es <- c("y ~ m" = "l",
#'                          "m ~ x" = "m",
#'                          "y ~ x" = "n")
#'
#' sim_only <- power4test(nrep = 2,
#'                        model = model_simple_med,
#'                        pop_es = model_simple_med_es,
#'                        n = 100,
#'                        R = 40,
#'                        ci_type = "boot",
#'                        fit_model_args = list(fit_function = "lm"),
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_indirect_effect,
#'                        test_args = list(x = "x",
#'                                         m = "m",
#'                                         y = "y",
#'                                         boot_ci = TRUE,
#'                                         mc_ci = FALSE))
#'
#' out <- power4test_by_n(test_out,
#'                        n = c(100, 110, 120))
#' out_reject <- get_rejection_rates_by_n(out)
#' out_reject
#'
#' @export
power4test_by_n <- function(object,
                            n = NULL,
                            progress = TRUE,
                            ...) {
  if (!inherits(object, "power4test")) {
    stop("Only support 'power4test' objects.")
  }
  if (!is.null(object$sim_all[[1]]$group_name)) {
    stop("Does not support multigroup models for now.")
  }
  out <- list()
  # TODO
  # - Think about to handle MG models,
  #   for which n is a vector.
  for (x in n) {
    if (progress) {
      cat("Updating the simulation for sample size:",
          paste(x, collapse = ", "),
          "\n")
    }
    out[[as.character(x)]] <- power4test(object = object,
                                         n = x,
                                         progress = progress,
                                         ...)
  }
  class(out) <- c("power4test_by_n", class(out))
  out
}

#' @rdname power4test_by_n
#'
#' @param sort WHen combining `power4test_by_n`
#' objects, whether they will be sorted
#' by sample sizes. Default is `TRUE`.
#'
#' @return
#' The method [c.power4test_by_n()] returns
#' a `power4test_by_n` object with
#' all the elements (tests for different
#' sample sizes) combined.
#'
#' @description
#' The method [c.power4test_by_n()]
#' is used to combine tests from different
#' runs of [power4test_by_n()].
#' @export
c.power4test_by_n <- function(...,
                              sort = TRUE) {

  # Check whether they have the same ptable
  tmp <- list(...)
  if (length(tmp) > 1) {
    ptables <- lapply(tmp,
                      \(x) {x[[1]]$sim_all[[1]]$ptable})
    for (i in seq_along(ptables)[-1]) {
      chk <- identical(ptables[[i]],
                       ptables[[i - 1]])
      if (!chk) {
        stop("Not all objects are based on the same model.")
      }
    }
  }
  out <- NextMethod()
  out["sort"] <- NULL
  class(out) <- c("power4test_by_n", class(out))
  if (!sort) {
    return(out)
  }
  ns <- as.numeric(names(out))
  i <- order(ns)
  out <- out[i]
  class(out) <- c("power4test_by_n", class(out))
  return(out)
}

#' @rdname power4test_by_n
#' @param object_by_n A `power4test_by_n`
#' object, which is an output of
#' [power4test_by_n()].
#'
#' @param all_columns If `TRUE`, all
#' columns stored by a test will be
#' printed. Default is `FALSE` and
#' only essential columns related to
#' power will be printed.
#'
#' @return
#' The function [get_rejection_rates_by_n()]
#' returns a data frame which is
#' similar to the output of
#' [get_rejection_rates()], with a
#' column `n` added for the sample size
#' for each test.
#'
#' @description
#' The function [get_rejection_rates_by_n()]
#' is used to extract the rejection
#' rates form a `get_rejection_rates_by_n`
#' object, with sample sizes added to
#' the output.
#'
#' @export
get_rejection_rates_by_n <- function(object_by_n,
                                     all_columns = FALSE) {
  tmpfct <- function(x, n) {
    out_i <- get_rejection_rates(x,
                                 all_columns = all_columns)
    out_i <- data.frame(n = n,
                        out_i)
    out_i
  }
  out <- mapply(tmpfct,
                x = object_by_n,
                n = as.numeric(names(object_by_n)),
                SIMPLIFY = FALSE)
  out <- do.call(rbind,
                 out)
  rownames(out) <- NULL
  out
}
