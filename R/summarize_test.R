#' @title Summarize Test Results
#'
#' @description Extract and summarize
#' test results.
#'
#' @details
#' The function [summarize_tests()]
#' is used to extract
#' information from each test stored
#' in a `power4test` object.
#'
#' The method `print.test_out_list()` is
#' used to print the content of a list
#' of test stored in a `power4test`
#' object, with the option to print
#' just the names of tests.
#'
#' # The role of `summarize_tests()` and related functions
#'
#' The function [summarize_tests()] and
#' related print methods are used by
#' the all-in-one function
#' [power4test()] and its `summary`
#' method. Users usually do not
#' call them directly, though
#' developers can use this function to
#' develop other functions for power
#' analysis, or to build their own
#' workflows to do the power analysis.
#'
#' @return
#' The function [summarize_tests()] returns
#' a list of the class `test_summary_list`.
#' Each element contains a summary of a
#' test stored. The elements are of
#' the class `test_summary`, with
#' these elements:
#'
#' - `test_attributes`: The stored
#'  information of a test, for printing.
#'
#' - `nrep`: The number of datasets
#'  (replications).
#'
#' - `mean`: The means of numeric
#'  information. For significance
#'  tests, these are the rejection
#'  rates.
#'
#' - `nvalid`: The number of non-`NA`
#'  replications used to compute each
#'  mean.
#'
#' @param object A `power4test` object
#' or the element `test_all` in
#' a `power4test` object.
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
#' @seealso [power4test()]
#'
#' @examples
#'
#' # Specify the model
#'
#' mod <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' # Specify the population values
#'
#' es <-
#' "
#' y ~ m: l
#' m ~ x: m
#' y ~ x: n
#' "
#'
#' # Simulated datasets
#'
#' sim_only <- power4test(nrep = 2,
#'                        model = mod,
#'                        pop_es = es,
#'                        n = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' # Test the parameters in each dataset
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_parameters)
#'
#' # Print the summary
#'
#' summarize_tests(test_out)
#'
#' @export
summarize_tests <- function(object,
                            collapse = c("none",
                                         "all_sig",
                                         "at_least_one_sig",
                                         "at_least_k_sig"),
                            at_least_k = 1) {
  collapse <- match.arg(collapse)
  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  out <- sapply(object,
                summarize_test_i,
                collapse = collapse,
                at_least_k = at_least_k,
                simplify = FALSE)
  class(out) <- c("test_summary_list", class(out))
  out
}

#' @rdname summarize_tests
#'
#' @param x The object to be printed.
#'
#' @param digits The numbers of digits
#' after the decimal when printing
#' numeric results.
#'
#' @param ... Optional arguments.
#' Not used.
#'
#' @return
#' The `print` methods returns `x` invisibly.
#' They are called for their side
#' effects.
#'
#' @export
print.test_summary_list <- function(x,
                                    digits = 3,
                                    ...) {
  for (xx in x) {
    print(xx,
          digits = digits,
          ...)
  }
  invisible(x)
}

#' @rdname summarize_tests
#'
#' @export
print.test_summary <- function(x,
                               digits = 2,
                               ...) {
  x0 <- unclass(x)
  nvalid <- x0$nvalid
  test_attr <- x0$test_attributes
  test_name <- test_attr$test_name
  cat(header_str(paste0("<", test_name, ">"),
                 hw = .7,
                 prefix = "\n",
                 suffix = "\n\n"))
  if (is.vector(x0$mean)) {
    results_type <- "vector"
  } else {
    results_type <- "data.frame"
  }
  if (results_type == "vector") {
    nvalid_sig <- unname(nvalid["sig"])
    propvalid_sig <- unname(nvalid_sig / x0$nrep)
    cat("Mean(s) across replication:\n")
    out0 <- formatC(x0$mean,
                    digits = digits,
                    format = "f")
    print(as.data.frame(rbind(out0),
                        check.names = FALSE),
          row.names = FALSE)
    cat("\n")
    cat("- The value 'sig' is the rejection rate.\n")
    cat("- If the null hypothesis is false, this is the power.\n")
    cat("- Number of valid replications for rejection rate:",
        nvalid_sig, "\n")
    cat("- Proportion of valid replications for rejection rate:",
        formatC(propvalid_sig, digits = digits, format = "f"),
        "\n")
  }
  if (results_type == "data.frame") {
    nvalid_sig <- unname(nvalid[, "sig", drop = TRUE])
    propvalid_sig <- unname(nvalid_sig / x0$nrep)
    one_nvalid <- all(nvalid_sig == max(nvalid_sig))
    cat("Mean(s) across replication:\n")
    df_means <- x0$mean
    if (!one_nvalid) {
      df_means$p_sig <- propvalid_sig
    }
    df_means <- format_num_cols(df_means,
                                digits = digits)
    print(df_means)
    cat("\n")
    cat("- The column 'sig' shows the rejection rates.\n")
    cat("- If the null hypothesis is false, the rate is the power.\n")
    if (one_nvalid) {
      cat("- Number of valid replications for rejection rate(s):",
          max(nvalid_sig), "\n")
      cat("- Proportion of valid replications for rejection rate(s):",
          formatC(max(propvalid_sig), digits = digits, format = "f"),
          "\n")
    } else {
      cat(strwrap(paste0("- The column 'p_sig' shows proportions of ",
                        "valid replications for rejection rates."),
                  exdent = 2), sep = "\n")
    }
  }
  invisible(x)
}

#' @rdname summarize_tests
#'
#' @param test_long If `TRUE`, a detailed
#' report will be printed.
#'
#' @export
print.test_out_list <- function(x,
                                digits = 3,
                                test_long = FALSE,
                                ...) {
  # TODO:
  # - Retrieve and print test notes.
  if (!test_long) {
    test_names <- names(x)
    cat(header_str("Test(s) Conducted",
                  hw = .8,
                  prefix = "\n",
                  suffix = "\n\n"))
    cat(paste0("- ", test_names),
        sep = "\n")
    cat("\n")
    cat("Call print() and set 'test_long = TRUE' for a detailed report.\n")
  } else {
    out <- summarize_tests(x)
    print(out,
          digits = digits,
          ...)
  }
  invisible(x)
}

#' @noRd
format_num_cols <- function(object,
                            digits = digits) {
  tmp <- as.data.frame(object,
                       check.names = FALSE)
  tmp2 <- lapply(tmp,
                  function(x,
                            digits) {
                    if (is.numeric(x)) {
                      x <- formatC(x,
                                    digits = digits,
                                    format = "f")
                      return(x)
                    } else {
                      return(x)
                    }
                  },
                  digits = digits)
  tmp2 <- as.data.frame(tmp2,
                        check.names = FALSE)
  class(tmp2) <- class(object)
  tmp2
}

#' @noRd
summarize_test_i <- function(x,
                             collapse = collapse,
                             at_least_k = at_least_k) {
  test_i <- x[[1]]$test_results
  if (is.vector(test_i)) {
    # A vector
    out <- summarize_one_test_vector(x)
    return(out)
  }
  if (isTRUE(length(dim(test_i)) == 2)) {
    # A table, likely a data frame
    out <- summarize_one_test_data_frame(x,
                                         collapse = collapse,
                                         at_least_k = at_least_k)
    return(out)
  }
  stop("test_results not supported. Something's wrong.")
}

#' @noRd
summarize_one_test_vector <- function(x) {
  test_results_all <- lapply(x,
                             function(xx) xx$test_results)
  has_R <- !is.null(test_results_all[[1]]["R"])
  if (has_R) {
    R <- unname(test_results_all[[1]]["R"])
    Rext <- R_extrapolate()
    R_case <- bz_case(R)
    do_bz <- (R_case != "") &&
             getOption("power4mome.bz", default = TRUE)
  } else {
    R <- NULL
    do_bz <- FALSE
    R_case <- ""
  }
  if (do_bz) {
    if (R_case == "one") {
      Rk <- Rext[seq(1, which(Rext == R) - 1)]
      test_results_all <- lapply(
              test_results_all,
              add_rr_ext,
              R = R,
              Rk = Rk
            )
    }
  }
  nrep <- length(test_results_all)
  test_results_all <- do.call(rbind,
                              test_results_all)
  test_results_all <- as.data.frame(test_results_all,
                                    check.names = FALSE)
  test_means <- colMeans(test_results_all, na.rm = TRUE)
  if (do_bz) {
    tmp <- bz_rr(test_means)
    test_means["sig"] <- as.numeric(tmp)
    bz_model <- attr(
                  tmp,
                  "bz_model"
                )
  } else {
    bz_model <- NULL
  }
  test_not_na <- apply(test_results_all,
                       2,
                       function(x) {sum(!is.na(x))})
  out1 <- list(test_attributes = attributes(x),
               nrep = nrep,
               mean = test_means,
               nvalid = test_not_na)
  # May add other attributes in the future
  attr(out1,
      "extra") <- list(bz_extrapolated = do_bz,
                       R_case = R_case,
                       bz_model = bz_model)
  class(out1) <- c("test_summary", class(out1))
  out1
}

#' @noRd
summarize_one_test_data_frame <- function(x,
                                          collapse = c("none",
                                                       "all_sig",
                                                       "at_least_one_sig",
                                                       "at_least_k_sig"),
                                          at_least_k = 1) {
  collapse <- match.arg(collapse)
  nrep <- length(x)
  test_i <- x[[1]]$test_results
  test_label <- attr(test_i, "test_label")
  i <- sapply(test_i,
              is.numeric)
  i_names <- colnames(test_i)[i]
  out0 <- sapply(test_i[, "test_label", drop = TRUE],
                 function(xx) {
                   t(sapply(x,
                            function(yy) {
                              tmp <- yy$test_results
                              unlist(tmp[tmp[, test_label] == xx, i])
                            },
                            simplify = TRUE))
                 },
                 simplify = FALSE)
  do_bz <- FALSE
  has_R <- FALSE
  R_case <- ""
  bz_model <- NULL
  if ((length(out0) == 1) ||
      (collapse == "none")) {
    has_R <- "R" %in% colnames(out0[[1]])
    if (has_R) {
      R <- sapply(out0,
                  \(x) x[, "R"],
                  simplify = FALSE)
      R <- unname(unlist(R))
      if (all(R != R[1])) {
        # Not all R equal. Do not do Boos-Zhang
        do_bz <- FALSE
      } else {
        R <- R[1]
        Rext <- R_extrapolate()
        R_case <- bz_case(R)
        do_bz <- (R_case != "") &&
                 getOption("power4mome.bz", default = TRUE)
      }
    } else {
      R <- NULL
      do_bz <- FALSE
      R_case <- ""
    }
    if (do_bz) {
      if (R_case == "one") {
        Rk <- Rext[seq(1, which(Rext == R) - 1)]
       for (j1 in seq_along(out0)) {
          tmp0 <- out0[[j1]]
          if (!(paste0("bz_", R) %in% colnames(tmp0))) {
            tmp2 <- add_bz_i(tmp0)
            out0[[j1]] <- tmp2
          }
        }
      }
    }
    out1 <- t(sapply(out0,
                    colMeans,
                    na.rm = TRUE))
    if (do_bz) {
      bz_model <- as.list(seq_len(nrow(out1)))
      for (j1 in seq_len(nrow(out1))) {
        tmp <- bz_rr(out1[j1, , drop = TRUE])
        bz_model[[j1]] <- attr(tmp,
                               "bz_model")
        out1[j1, "sig"] <- tmp
      }
    } else {
      bz_model <- NULL
    }
    test_not_na <- t(sapply(out0,
                      function(x) {
                        apply(x,
                              2,
                              function(xx) {sum(!is.na(xx))})
                      }))
    test_means <- test_i
    test_means[, i_names] <- out1[, i_names, drop = FALSE]
  } else {
    # Boos-Zhang method not supported if collapse != "none"
    out1a <- out0[[1]]
    out1a[] <- as.numeric(NA)
    sig0 <- sapply(out0,
                    function(xx) xx[, "sig", drop = TRUE],
                    simplify = TRUE)
    if (collapse == "all_sig") {
      sig1 <- apply(sig0,
                    MARGIN = 1,
                    function(xx) as.numeric(all(xx > 0)),
                    simplify = TRUE)
      out1a[, "sig"] <- sig1
    }
    if (collapse == "at_least_one_sig") {
      sig1 <- apply(sig0,
                    MARGIN = 1,
                    function(xx) as.numeric(any(xx > 0)),
                    simplify = TRUE)
      out1a[, "sig"] <- sig1
    }
    if (collapse == "at_least_k_sig") {
      sig1 <- apply(sig0,
                    MARGIN = 1,
                    function(xx) as.numeric(isTRUE(sum(xx > 0, na.rm = TRUE) >= at_least_k)),
                    simplify = TRUE)
      out1a[, "sig"] <- sig1
    }
    out1b <- colMeans(out1a, na.rm = TRUE)
    out1 <- out1a[1, , drop = FALSE]
    out1[] <- out1b
    test_not_na <- out1a[1, , drop = FALSE]
    test_not_na[] <- sum(apply(sig0,
                       MARGIN = 1,
                       function(xx) as.numeric(all(!is.na(xx)))))
    test_means <- test_i[1 , ]
    test_means[] <- NA
    if (collapse == "at_least_k_sig") {
      tmp <- paste0("at_least_", at_least_k, "_sig")
    } else {
      tmp <- collapse
    }
    test_means[1, "test_label"] <- tmp
    test_means[, i_names] <- out1[, i_names, drop = FALSE]
  }
  class(test_means) <- class(test_i)
  out1 <- list(test_attributes = attributes(x),
               nrep = nrep,
               mean = test_means,
               nvalid = test_not_na)
  # May add other attributes in the future
  attr(out1,
      "extra") <- list(bz_extrapolated = do_bz,
                       R_case = R_case,
                       bz_model = bz_model)
  class(out1) <- c("test_summary", class(out1))
  out1
}

