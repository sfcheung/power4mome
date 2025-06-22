#' @title Power By Sample Sizes
#'
#' @description Estimate power for a
#' set of sample sizes.
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
#' power over a set of sample sizes.
#'
#' @return The function
#' [power4test_by_n()] returns a
#' `power4test_by_n` object, which is a
#' list of `power4test` objects, one for
#' each sample size.
#'
#' @param object A `power4test` object,
#' or a `power4test_by_n` object.
#' If it is a `power4test_by_n` object,
#' the first element, which is a
#' `power4test` object, will be used
#' as the value of this argument.
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
#' For the `print` method of the output
#' of [power4test_by_n()], they are
#' arguments to be passed to the
#' `print` method of the output of
#' [power4test()] ([print.power4test()]).
#'
#' @param by_seed If set to a number,
#' it will be used to generate the
#' seeds for each call to [power4test()].
#' If `NULL`, the default, then seeds
#' will still be randomly generated but
#' the results cannot be easily reproduced.
#'
#' @param by_nrep If set to a number,
#' it will be used to generate the
#' number of replications (`nrep`) for
#' each call to [power4test()].
#' If set to a numeric vector of the
#' same length as `n`, then these are
#' the `nrep` values for each of the
#' calls, allowing for different numbers
#' of replications for the sample sizes.
#' If `NULL`, the default, then the
#' original `nrep` will be used.
#' This argument is used by
#' [x_from_power()] for efficiency, and
#' is rarely used when calling
#' this function directly.
#'
#' @param save_sim_all If `FALSE`,
#' the dataset in the
#' `power4test` objects are not saved,
#' to reduce
#' the size of the output. Default
#' is `TRUE`.
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#' # Specify the model
#'
#' model_simple_med <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' # Specify the population values
#'
#' model_simple_med_es <-
#' "
#' m ~ x: m
#' y ~ m: l
#' y ~ x: n
#' "
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
#' out_reject <- rejection_rates(out)
#' out_reject
#'
#' @export
power4test_by_n <- function(object,
                            n = NULL,
                            progress = TRUE,
                            ...,
                            by_seed = NULL,
                            by_nrep = NULL,
                            save_sim_all = TRUE) {
  if (!inherits(object, "power4test") &&
      !inherits(object, "power4test_by_n")) {
    stop("Only support 'power4test' or 'power4test_by_n' objects.")
  }
  # Use the first object if the object is a power4test_by_n object:
  if (inherits(object, "power4test_by_n")) {
    object <- object[[1]]
  }
  if (!is.null(object$sim_all[[1]]$group_name)) {
    stop("Does not support multigroup models for now.")
  }
  out <- list()

  # # Disable this for now
  # # Store the original object
  # n_org <- attr(object, "args")$n
  # out[[1]] <- object

  if (length(by_seed) == length(n)) {
    seeds <- by_seed
  } else {
    if (!is.null(by_seed)) {
      set.seed(by_seed)
    }
    seeds <- sample.int(99999999,
                        size = length(n))
  }

  if (length(by_nrep) == length(n)) {
    new_nrep <- by_nrep
  } else {
    if (!is.null(by_nrep)) {
      new_nrep <- rep(by_nrep, length(n))
    } else {
      new_nrep <- NULL
    }
  }
  # TODO
  # - Think about to handle MG models,
  #   for which n is a vector.
  for (i in seq_along(n)) {
    x <- n[i]
    if (progress) {
      cat("\nUpdating the simulation for sample size:",
          paste(x, collapse = ", "),
          "\n")
    }
    if (is.null(new_nrep)) {
      tmp_out <- tryCatch(power4test(
                              object = object,
                              n = x,
                              progress = progress,
                              iseed = seeds[i],
                              ...
                            ),
                            error = function(e) e)
    } else {
      tmp_out <- tryCatch(power4test(
                              object = object,
                              n = x,
                              nrep = new_nrep[i],
                              progress = progress,
                              iseed = seeds[i],
                              ...
                            ),
                            error = function(e) e)
    }

    if (inherits(tmp_out, "error")) {

      if (progress) {
        msg <- "Error in estimating power for this sample size:"
        msg <- c(msg, paste0("- ", x))
        msg <- c(msg, "The error message:")
        msg <- c(msg, paste0("- ", tmp_out$message))
        msg <- c(msg, "This sample sizes is skipped.")
        cat(c("", msg, ""), sep = "\n")
      }

      next
    }

    if (!save_sim_all) {
      # Keep the first element
      tmp_out$sim_all <- tmp_out$sim_all[1]
    }
    out[[as.character(x)]] <- tmp_out
  }
  class(out) <- c("power4test_by_n", class(out))
  out
}

#' @rdname power4test_by_n
#'
#' @param sort When combining the
#' objects, whether they will be sorted
#' by sample sizes. Default is `TRUE`.
#'
#' @param skip_checking_models Whether
#' the check of the data generation model
#' will be checked. Default is `TRUE`.
#' Should be set to `FALSE` only when
#' users are certain the they are based
#' on the same model, or when the model
#' is not saved (e.g., `save_sim_all`
#' set to `FALSE` when the objects
#' were generated).
#' This argument is used by
#' [x_from_power()] for efficiency, and
#' is rarely used when calling
#' the `c` method directly.
#'
#' @return
#' The method [c.power4test_by_n()] returns
#' a `power4test_by_n` object with
#' all the elements (tests for different
#' sample sizes) combined.
#'
#' @details
#' The method [c.power4test_by_n()]
#' is used to combine tests from different
#' runs of [power4test_by_n()].
#' @export
c.power4test_by_n <- function(...,
                              sort = TRUE,
                              skip_checking_models = FALSE) {

  # Check whether they have the same ptable
  tmp <- list(...)
  if ((length(tmp) > 1) && !skip_checking_models) {
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
  out["skip_checking_models"] <- NULL

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
#' The function
#' returns a data frame which is
#' similar to the output of
#' [rejection_rates()], with a
#' column `n` added for the sample size
#' for each test.
#'
#' @description
#' The function
#' is used to extract the rejection
#' rates form a `power4test_by_n`
#' object, with sample sizes added to
#' the output.
#'
#' @noRd
rejection_rates_by_n <- function(object_by_n,
                                 all_columns = FALSE,
                                 ...) {
  tmpfct <- function(x, n) {
    out_i <- rejection_rates(x,
                             all_columns = all_columns,
                             ...)
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

#' @rdname power4test_by_n
#'
#' @param original_object The object
#' to be converted to a `power4test_by_n`
#' object.
#'
#' @return
#' The function [as.power4test_by_n()] returns
#' a `power4test_by_n` object converted
#' from the input object.
#'
#' @details
#' The function [as.power4test_by_n()]
#' is used to convert a `power4test`
#' object to a `power4test_by_n`
#' object, if it is not already one.
#' Useful when concatenating
#' `power4test` objects with
#' `power4test_by_n` objects.
#' @export
as.power4test_by_n <- function(original_object) {
  if (inherits(original_object, "power4test")) {
    x0 <- attr(original_object, "args")$n
    original_object <- list(original_object)
    class(original_object) <- c("power4test_by_n", class(original_object))
    names(original_object) <- as.character(x0)
    return(original_object)
  }
  if (inherits(original_object, "power4test_by_n")) {
    return(original_object)
  }
  stop("original_object not of a supported class.")
}

#' @rdname power4test_by_n
#'
#' @param x The object
#' to be printed.
#'
#' @param print_all If `TRUE`, all
#' elements in `x`, that is, the results
#' of all sample sizes examined, will
#' be printed. If `FALSE`, then only
#' those of the first sample size
#' will be printed.
#'
#' @return
#' The `print`-method of `power4test_by_n`
#' objects returns the object invisibly.
#' It is called for its side-effect.
#'
#' @export
print.power4test_by_n <- function(
    x,
    print_all = FALSE,
    ...
  ) {
  n_tried <- names(x)
  catwrap(
      c(paste("The sample(s) examined:",
              paste0(sort(n_tried),
                     collapse = ", ")))
      )

  if (print_all) {
    cat("\nThe output for each `power4test` analysis:\n")
    ii <- seq_along(x)
  } else {
    cat("\nThe output of the first element:\n")
    ii <- 1
  }

  for (i in ii) {
    n_i <- attr(x[[i]], "args")$n
    xx <- x[[i]]
    class(xx) <- "power4test"
    cat(header_str("",
                  hw = .84,
                  sym = "-",
                  prefix = "\n",
                  suffix = "\n",
                  sep_i = ""))
    cat(header_str(paste0("Sample Size: ", n_i),
                  hw = .8,
                  sym = "-",
                  suffix = "\n"))
    cat(header_str("",
                  hw = .84,
                  sym = "-",
                  suffix = "\n",
                  sep_i = ""))
    print(xx,
          ...)
  }
  if (!print_all) {
    cat("\n")
    catwrap(
        "Print with 'print_all = TRUE' to print all elements."
        )
  }
}