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
      tmp_out <- power4test(object = object,
                             n = x,
                             progress = progress,
                             iseed = seeds[i],
                             ...)
    } else {
      tmp_out <- power4test(object = object,
                             n = x,
                             nrep = new_nrep[i],
                             progress = progress,
                             iseed = seeds[i],
                             ...)
    }
    if (!save_sim_all) {
      # Keep the first element
      tmp_out$sim_all <- tmp_out$sim_all[1]
    }
    out[[i]] <- tmp_out
  }
  # names(out) <- c(n_org, n)
  names(out) <- c(n)
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
