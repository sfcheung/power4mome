
#' @title Rejection Rates
#'
#' @description Get all rejection rates
#' of all tests stored in a `power4test`
#' object or other supported objects.
#'
#' @details
#' For a `power4test` object,
#' it loops over the tests stored
#' in a `power4test` object and retrieve
#' the rejection rate of each test.
#'
#' @return
#' The `rejection_rates` method for
#' `power4test` objects returns
#' a data frame with the number of
#' rows equal to the number of tests.
#' Note that some tests, such as
#' the test by [test_parameters()],
#' conduct one test for each parameters.
#' Each such test is counted as one
#' test.
#' The data frame has these columns:
#'
#' - `test`: The name of the test.
#'
#' - `label`: The label for each
#'  test, or `"Test"` if a test only
#'  does on test (e.g., [test_indirect_effect()]).
#'
#' - `pvalid`: The proportion of valid
#'  tests across all replications.
#'
#' - `reject`: The rejection rate for
#'  each test. If the null hypothesis
#'  is false, then this is the power.
#'
#' @param object The object
#' from which the rejection rates
#' are to be extracted, such as
#' a `power4test` object,
#' a `power4test_by_n` object,
#' or a `power4test_by_es` object.
#'
#' @param ... Optional arguments. Not
#' used.
#'
#' @seealso [power4test()],
#' [power4test_by_n()], and
#' [power4test_by_es()].
#'
#' @examples
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
#' sim_only <- power4test(nrep = 4,
#'                        model = model_simple_med,
#'                        pop_es = model_simple_med_es,
#'                        n = 100,
#'                        R = 50,
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
#' test_out <- power4test(object = test_out,
#'                        test_fun = test_parameters)
#' rejection_rates(test_out)
#'
#' # See the help pages of power4test_by_n() and power4test_by_es()
#' # for other examples.
#'
#' @export
rejection_rates <- function(object,
                            ...) {
  UseMethod("rejection_rates")
}

#' @export
#' @rdname rejection_rates
rejection_rates.default <- function(object,
                                    ...) {
  stop("The object is not of a supported class.")
}

#' @param all_columns If `TRUE`, all
#' columns stored by a test will be
#' printed. Default is `FALSE` and
#' only essential columns related to
#' power will be printed.
#'
#' @param ci If `TRUE`, confidence
#' intervals for the rejection rates
#' (column `reject` or `sig`) will
#' be computed. Normal approximation
#' is used.
#'
#' @param level The level of confidence
#' for the confidence intervals, if
#' `ci` is `TRUE`.
#'
#' @param se If `TRUE`, standard errors
#' for the rejection rates
#' (column `reject` or `sig`) will
#' be computed. Normal approximation
#' is used.
#'
#' @export
#' @rdname rejection_rates
rejection_rates.power4test <- function(object,
                                       all_columns = FALSE,
                                       ci = TRUE,
                                       level = .95,
                                       se = FALSE,
                                       ...) {
  out0 <- summarize_tests(object)
  out1 <- lapply(out0,
                 rejection_rates_i,
                 all_columns = all_columns,
                 ci = ci,
                 level = level,
                 se = se)
  if (all_columns) {
    out2 <- do.call(rbind_adv,
                    out1)
  } else {
    out2 <- do.call(rbind,
                    out1)
  }
  rownames(out2) <- NULL
  out2
}

#' @noRd
rejection_rates_i <- function(object_i,
                                  all_columns = FALSE,
                                  ci = ci,
                                  level = level,
                                  se = se) {
  if (is.vector(object_i$mean)) {
    out <- rejection_rates_i_vector(object_i,
                                        all_columns = all_columns,
                                        ci = ci,
                                        level = level,
                                        se = se)
    return(out)
  }
  if (length(dim(object_i$mean)) == 2) {
    # Likely a data frame
    out <- rejection_rates_i_data_frame(object_i,
                                            all_columns = all_columns,
                                            ci = ci,
                                            level = level,
                                            se = se)
    return(out)
  }
  stop("Something is wrong. The tests results are not of the supported format.")
}

#' @noRd
rejection_rates_i_vector <- function(object_i,
                                         all_columns = FALSE,
                                         ci = TRUE,
                                         level = .95,
                                         se = FALSE) {
  test_args <- object_i$test_attributes
  test_name <- test_args$test_name
  if (all_columns) {
    out_i <- data.frame(test = test_name,
                        test_label = "Test",
                        pvalid = object_i$nvalid["sig"] / object_i$nrep,
                        nvalid = object_i$nvalid["sig"],
                        nrep = object_i$nrep,
                        rbind(object_i$mean),
                        row.names = NULL)
    tmp <- gsub("sig",
                "reject",
                colnames(out_i),
                fixed = TRUE)
    colnames(out_i) <- tmp
  } else {
    out_i <- data.frame(test = test_name,
                        test_label = "Test",
                        pvalid = object_i$nvalid["sig"] / object_i$nrep,
                        nvalid = object_i$nvalid["sig"],
                        reject = object_i$mean["sig"],
                        row.names = NULL)
  }
  if (se || ci) {
    out_i <- rejection_rates_add_ci(out_i,
                                    level = level,
                                    add_reject = FALSE,
                                    add_se = se)
    if (!ci) {
      out_i$reject_ci_lo <- NULL
      out_i$reject_ci_hi <- NULL
    }
  }
  if (!all_columns) {
    # nvalid used only for adding CI and SE
    out_i$nvalid <- NULL
  }
  out_i
}

#' @noRd
rejection_rates_i_data_frame <- function(object_i,
                                         all_columns = FALSE,
                                         ci = TRUE,
                                         level = .95,
                                         se = FALSE) {
  test_args <- object_i$test_attributes
  test_name <- test_args$test_name
  out_i0 <- object_i$mean
  p <- nrow(out_i0)
  pvalid <- object_i$nvalid[, "sig", drop = TRUE] / object_i$nrep
  if (all_columns) {
    out_i <- data.frame(test = test_name,
                        pvalid = pvalid,
                        nvalid = object_i$nvalid[, "sig", drop = TRUE],
                        nrep = object_i$nrep,
                        object_i$mean,
                        row.names = NULL)
    tmp <- gsub("sig",
                "reject",
                colnames(out_i),
                fixed = TRUE)
    colnames(out_i) <- tmp
    class(out_i) <- class(object_i$mean)
  } else {
    out_i <- data.frame(test = test_name,
                        test_label = out_i0$test_label,
                        pvalid = pvalid,
                        nvalid = object_i$nvalid[, "sig", drop = TRUE],
                        reject = object_i$mean[, "sig", drop = TRUE],
                        row.names = NULL)
  }
  if (se || ci) {
    out_i <- rejection_rates_add_ci(out_i,
                                    level = level,
                                    add_reject = FALSE,
                                    add_se = se)
    if (!ci) {
      out_i$reject_ci_lo <- NULL
      out_i$reject_ci_hi <- NULL
    }
  }
  if (!all_columns) {
    # nvalid used only for adding CI and SE
    out_i$nvalid <- NULL
  }
  out_i
}

#' @noRd

rbind_adv <- function(...) {
  # Need this function only if all_columns
  dfs <- list(...)
  cnames <- lapply(dfs,
                   colnames)
  cnames_all <- Reduce(union, cnames)
  # If all_columns
  # - test
  # - test_label
  # - pvalid
  # - nvalid
  # - nrep
  # - ...
  # - se, reject_ci_lo, reject_ci_hi (if se || ci)
  c1 <- lapply(cnames,
               \(x) setdiff(x, c("test",
                                 "test_label",
                                 "pvalid",
                                 "nvalid",
                                 "nrep",
                                 "sig",
                                 "reject_se",
                                 "reject_ci_lo",
                                 "reject_ci_hi")))
  c2 <- Reduce(union, c1)
  c3 <- c("test",
          "test_label",
          "pvalid",
          "nvalid",
          "nrep",
          c2)
  cnames_final <- c(c3, setdiff(cnames_all, c3))
  tmpfct <- function(x) {
    # Add NA
    d1 <- setdiff(cnames_final, colnames(x))
    if (length(d1) == 0) return(x)
    for (y in d1) {
      x[y] <- NA
    }
    x <- x[cnames_final]
    return(x)
  }
  dfs_out <- lapply(dfs,
                    tmpfct)
  dfs_out1 <- do.call(rbind,
                      dfs_out)
  dfs_out1
}

#' @return
#' The `rejection_rates` method for
#' for `power4test_by_es` objects
#' returns a data frame which is
#' similar to the output of
#' [rejection_rates()], with a
#' column added for the effect size (`pop_es_name` and
#' `pop_es_values`)
#' for each test.
#'
#' @details
#' The `rejection_rates` method for
#' `power4test_by_es` objects
#' is used to extract the rejection
#' rates from a `power4test_by_es`
#' object, with effect sizes added to
#' the output.
#'
#' @rdname rejection_rates
#' @export
rejection_rates.power4test_by_es <- function(object,
                                             all_columns = FALSE,
                                             ...) {
  rejection_rates_by_es(object_by_es = object,
                        all_columns = all_columns)
}

#' @return
#' The `rejection_rates` method for
#' for `power4test_by_n` objects
#' returns a data frame which is
#' similar to the output of
#' for a `power4test` object, with a
#' column `n` added for the sample size
#' for each test.
#'
#' @details
#' The `rejection_rates` method for
#' `power4test_by_n` objects
#' is used to extract the rejection
#' rates, with sample sizes added to
#' the output.
#'
#' @rdname rejection_rates
#' @export
rejection_rates.power4test_by_n <- function(object,
                                            all_columns = FALSE,
                                            ...) {
  rejection_rates_by_n(object_by_n = object,
                       all_columns = all_columns)
}