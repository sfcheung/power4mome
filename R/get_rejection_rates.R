
#' @title Rejection Rates
#'
#' @description Get all rejection rates
#' of all tests stored in a `power4test`
#' object or other supported objects.
#'
#' @details
#' For a `power4test` object,
#' it loops over the tests stored
#' in a `power4test` object and retrieves
#' the rejection rate of each test.
#'
#' @return
#' The `rejection_rates` method returns
#' a `rejection_rates_df` object,
#' with a `print` method.
#'
#' If the input (`object`) is a
#' `power4test` object, the output is
#' a data-frame like object with the
#' number of
#' rows equal to the number of tests.
#' Note that some tests, such as
#' the test by [test_parameters()],
#' conduct one test for each parameters.
#' Each such test is counted as one
#' test.
#'
#' The data frame has at least these columns:
#'
#' - `test`: The name of the test.
#'
#' - `label`: The label for each
#'  test, or `"Test"` if a test only
#'  does one test (e.g., [test_indirect_effect()]).
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
#' are to be computed, such as
#' a `power4test` object,
#' a `power4test_by_n` object,
#' or a `power4test_by_es` object.
#'
#' @param ... Optional arguments. For
#' the `print` method, these arguments
#' will be passed to the `print` method
#' of `data.frame` objects [print.data.frame()].
#' Not used by other methods.
#'
#' @seealso [power4test()],
#' [power4test_by_n()], and
#' [power4test_by_es()], which are
#' supported by this method.
#'
#' @examples
#'
#' # Specify the population model
#'
#' model_simple_med <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' # Specify the effect sizes (population parameter values)
#'
#' model_simple_med_es <-
#' "
#' y ~ m: l
#' m ~ x: m
#' y ~ x: n
#' "
#'
#' # Generate some datasets to check the model
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
#' # Do the test 'test_indirect_effect' on each datasets
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_indirect_effect,
#'                        test_args = list(x = "x",
#'                                         m = "m",
#'                                         y = "y",
#'                                         boot_ci = TRUE,
#'                                         mc_ci = FALSE))
#'
#' # Do the test 'test_parameters' on each datasets
#' # and add the results to 'test_out'
#'
#' test_out <- power4test(object = test_out,
#'                        test_fun = test_parameters)
#'
#' # Compute and print the rejection rates for stored tests
#'
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
#' extracted. Default is `FALSE` and
#' only essential columns related to
#' power will be printed.
#'
#' @param ci If `TRUE`, confidence
#' intervals for the rejection rates
#' (column `reject` or `sig`) will
#' be computed. Normal approximation
#' is used in forming the confidence
#' intervals.
#'
#' @param level The level of confidence
#' for the confidence intervals, if
#' `ci` is `TRUE`. Default is .95,
#' denoting 95%.
#'
#' @param se If `TRUE`, standard errors
#' for the rejection rates
#' (column `reject` or `sig`) will
#' be computed. Normal approximation
#' is used to compute the standard
#' errors.
#'
#' @param collapse Whether a single
#' decision (significant vs. not significant)
#' is made across all tests for a test
#' that consists of several tests
#' (e.g., the tests of several parameters).
#' If `"none"`, tests will be summarized
#' individually. If `"all_sig"`, then
#' the set of tests is considered significant
#' if all individual tests are significant.
#' If `"at_least_one_sig"`, then the set of
#' tests is considered significant if
#' at least one of the tests is significant.
#' If `"at_least_k_sig"`, then the set of
#' tests is considered significant if
#' at least `k` tests are significant,
#' `k` set by the argument `at_least_k`.
#'
#' @param at_least_k Used by `collapse`,
#' the number of tests required to be
#' significant for the set of tests to
#' be considered significant.
#'
#'
#' @export
#' @rdname rejection_rates
rejection_rates.power4test <- function(object,
                                       all_columns = FALSE,
                                       ci = TRUE,
                                       level = .95,
                                       se = FALSE,
                                       collapse = c("none",
                                                    "all_sig",
                                                    "at_least_one_sig",
                                                    "at_least_k_sig"),
                                       at_least_k = 1,
                                       ...) {
  out0 <- summarize_tests(object,
                          collapse = collapse,
                          at_least_k = at_least_k)
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
  class(out2) <- c("rejection_rates_df",
                   class(out2))
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
                        est = object_i$mean["est"],
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
                        est = object_i$mean[, "est", drop = TRUE],
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
#' returns an object of the
#' class `rejection_rates_df_by_es`,
#' which is a subclass of
#' `rejection_rates_df`.
#' It is a data frame which is
#' similar to the output of
#' [rejection_rates()], with two
#' columns added for the effect size (`pop_es_name` and
#' `pop_es_values`)
#' for each test.
#'
#' @details
#' The `rejection_rates` method for
#' `power4test_by_es` objects
#' is used to compute the rejection
#' rates from a `power4test_by_es`
#' object, with effect sizes added to
#' the output.
#'
#' @rdname rejection_rates
#' @export
rejection_rates.power4test_by_es <- function(object,
                                             all_columns = FALSE,
                                             ci = TRUE,
                                             level = .95,
                                             se = FALSE,
                                             ...) {
  out <- rejection_rates_by_es(object_by_es = object,
                               all_columns = all_columns,
                               ci = ci,
                               level = level,
                               se = se,
                               ...)
  class(out) <- c("rejection_rates_df_by_es",
                  "rejection_rates_df",
                  class(out))
  out
}

#' @return
#' The `rejection_rates` method for
#' for `power4test_by_n` objects
#' returns an object of the
#' class `rejection_rates_df_by_n`,
#' which is a subclass of
#' `rejection_rates_df`.
#' It is a data frame which is
#' similar to the output of
#' for a `power4test` object, with a
#' column `n` added for the sample size
#' for each test.
#'
#' @details
#' The `rejection_rates` method for
#' `power4test_by_n` objects
#' is used to compute the rejection
#' rates, with sample sizes added to
#' the output.
#'
#' @rdname rejection_rates
#' @export
rejection_rates.power4test_by_n <- function(object,
                                            all_columns = FALSE,
                                            ci = TRUE,
                                            level = .95,
                                            se = FALSE,
                                            ...) {
  out <- rejection_rates_by_n(object_by_n = object,
                              all_columns = all_columns,
                              ci = ci,
                              level = level,
                              se = se,
                              ...)
  class(out) <- c("rejection_rates_df_by_n",
                  "rejection_rates_df",
                  class(out))
  out
}

#' @param x The `rejection_rates_df`
#' object to be printed.
#'
#' @param digits The number of digits
#' to be printed
#' after the decimal.
#'
#' @param annotation Logical. Whether
#' additional notes will be printed.
#'
#' @param abbreviate_col_names Logical.
#' Whether some column names will be
#' abbreviated.
#'
#' @return
#' The `print` method of a
#' `rejection_rates_df` object return
#' the object invisibly. It is called
#' for its side-effect.
#'
#' @rdname rejection_rates
#' @export
print.rejection_rates_df <- function(x,
                                     digits = 3,
                                     annotation = TRUE,
                                     abbreviate_col_names = TRUE,
                                     ...) {
  x0 <- x
  class(x0) <- "data.frame"

  # Handle special columns
  if ("nvalid" %in% colnames(x0)) {
    x0$nvalid <- as.character(x0$nvalid)
  }
  if ("nrep" %in% colnames(x0)) {
    x0$nrep <- as.character(x0$nrep)
  }

  # In by_n
  if ("n" %in% colnames(x0)) {
    x0$n <- as.character(x0$n)
  }

  test_i <- unique(x0$test)
  if (length(test_i) == 1) {
    x0$test <- NULL
  }
  test_label_i <- unique(x0$test_label)
  if (length(test_label_i) == 1) {
    x0$test_label <- NULL
  }

  abbr_names <- c(pvalid = "p.v",
                  nvalid = "n.v",
                  reject_se = "r.se",
                  reject_ci_lo = "r.cilo",
                  reject_ci_hi = "r.cihi")

  # Abbreviate column names
  if (abbreviate_col_names) {
    cnames <- colnames(x0)
    for (i in seq_along(abbr_names)) {
      cnames <- gsub(names(abbr_names)[i],
                     abbr_names[i],
                     cnames,
                     fixed = TRUE)
    }
    colnames(x0) <- cnames
  }

  x1 <- format_num_cols(x0,
                        digits = digits)

  if (length(test_i) == 1) {
    cat("[test]:", test_i, "\n")
  }
  if (length(test_label_i) == 1) {
    cat("[test_label]:", test_label_i, "\n")
  }

  print(x1,
        ...)

  if (annotation) {
    cat("Notes:\n")

    if ("n" %in% colnames(x1)) {
      catwrap(paste0("- ",
                     "n",
                     ": The sample size in a trial."),
              exdent = 2)
    }
    if ("par" %in% colnames(x1)) {
      catwrap(paste0("- ",
                     "par",
                     ": The parameter being varied."),
              exdent = 2)
    }
    if ("es" %in% colnames(x1)) {
      catwrap(paste0("- ",
                     "es",
                     ": The population value of 'par' in a trial."),
              exdent = 2)
    }
    tmp <- ifelse(abbreviate_col_names,
                  abbr_names["pvalid"],
                  "pvalid")
    if (tmp %in% colnames(x1)) {
      catwrap(paste0("- ",
                     tmp,
                     ": The proportion of valid replications."),
              exdent = 2)
    }
    tmp <- ifelse(abbreviate_col_names,
                  abbr_names["nvalid"],
                  "nvalid")
    if (tmp %in% colnames(x1)) {
      catwrap(paste0("- ",
                     tmp,
                     ": The number of valid replications."),
              exdent = 2)
    }
    if ("nrep" %in% colnames(x1)) {
      tmp <- "nrep"
      catwrap(paste0("- ",
                     tmp,
                     ": The number of replications."),
              exdent = 2)
    }
    if ("est" %in% colnames(x1)) {
      catwrap(paste0("- est: The mean of the estimates in a test across replications."),
              exdent = 2)
    }
    if ("cilo" %in% colnames(x1)) {
      catwrap(paste0("- cilo, cihi: The mean of the lower/upper limits of ",
                     "the confidence intervals in a test ",
                     "across replications."),
              exdent = 2)
    }
    if ("reject" %in% colnames(x1)) {
      catwrap(paste0("- reject: The proportion of 'significant' replications, ",
                     "that is, the rejection rate. ",
                     "If the null hypothesis is true, this is the Type I error rate. ",
                     "If the null hypothesis is false, this is the power."),
              exdent = 2)
    }
    tmp <- ifelse(abbreviate_col_names,
                  abbr_names["reject_se"],
                  "reject_se")
    if (tmp %in% colnames(x1)) {
      catwrap(paste0("- ",
                     tmp,
                     ": The standard error of the rejection rate, ",
                     "based on normal approximation."),
              exdent = 2)
    }
    tmp1 <- ifelse(abbreviate_col_names,
                   abbr_names["reject_ci_lo"],
                   "reject_ci_lo")
    tmp2 <- ifelse(abbreviate_col_names,
                   abbr_names["reject_ci_hi"],
                   "reject_ci_hi")
    if (tmp1 %in% colnames(x1)) {
      tmp <- paste0(tmp1, ",", tmp2)
      catwrap(paste0("- ",
                     tmp,
                     ": The confidence interval ",
                     "of the rejection rate, based on normal approximation."),
              exdent = 2)
    }
    catwrap(paste0("- Refer to the tests for the meanings of other columns."),
            exdent = 2)
  }
  invisible(x)
}