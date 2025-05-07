#' @title Summarize Test Results
#'
#' @description Extract and summarize
#' test results.
#'
#' @details
#' This function is used to extract
#' information from each test stored
#' in a `power4test` object.
#'
#' @return
#' The function [summarize_tests()] returns
#' a list of the class `test_summary_list`.
#' Each element contains a summary of a
#' test stored. The elements are of
#' the class `test_summary`, with
#' these elments:
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
#' @seealso [power4test()]
#'
#' @examples
#'
#' mod <-
#' "
#' m ~ a*x
#' y ~ b*m + x
#' ab := a * b
#' "
#'
#' mod_es <- c("y ~ m" = "l",
#'             "m ~ x" = "m",
#'             "y ~ x" = "n")
#'
#' sim_only <- power4test(nrep = 2,
#'                        model = mod,
#'                        pop_es = mod_es,
#'                        n = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_parameters)
#'
#' test_out
#'
#' @export
summarize_tests <- function(object) {
  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  out <- sapply(object,
                summarize_test_i,
                simplify = FALSE)
  class(out) <- c("test_summary_list", class(out))
  out
}

#' @rdname summarize_tests
#'
#' @param x The object to be printed.
#'
#' @param digits The numbers of digits
#' after the decimal when print
#' numeric results.
#'
#' @param ... Optional arguments.
#' Not used.
#'
#' @return
#' The `print` methods return `x` invisibly.
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
#' @details
#' The method `print.test_out_list()` is
#' used to print the contest of a list
#' of test stored in a `power4test`
#' object, with the option to print
#' just the names of tests.
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
summarize_test_i <- function(x) {
  test_i <- x[[1]]$test_results
  if (is.vector(test_i)) {
    # A vector
    out <- summarize_one_test_vector(x)
    return(out)
  }
  if (isTRUE(length(dim(test_i)) == 2)) {
    # A table, likely a data frame
    out <- summarize_one_test_data_frame(x)
    return(out)
  }
  stop("test_results not supported. Something's wrong.")
}

#' @noRd
summarize_one_test_vector <- function(x) {
  test_results_all <- lapply(x,
                             function(xx) xx$test_results)
  nrep <- length(test_results_all)
  test_results_all <- do.call(rbind,
                              test_results_all)
  test_results_all <- as.data.frame(test_results_all,
                                    check.names = FALSE)
  test_means <- colMeans(test_results_all, na.rm = TRUE)
  test_not_na <- apply(test_results_all,
                       2,
                       function(x) {sum(!is.na(x))})
  out1 <- list(test_attributes = attributes(x),
               nrep = nrep,
               mean = test_means,
               nvalid = test_not_na)
  class(out1) <- c("test_summary", class(out1))
  out1
}

#' @noRd
summarize_one_test_data_frame <- function(x) {
  nrep <- length(x)
  test_i <- x[[1]]$test_results
  test_label <- attr(test_i, "test_label")
  i <- sapply(test_i,
              is.numeric)
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
  out1 <- t(sapply(out0,
                   colMeans,
                   na.rm = TRUE))
  test_not_na <- t(sapply(out0,
                     function(x) {
                       apply(x,
                             2,
                             function(xx) {sum(!is.na(xx))})
                     }))
  test_means <- test_i
  test_means[, i] <- out1
  class(test_means) <- class(test_i)
  out1 <- list(test_attributes = attributes(x),
               nrep = nrep,
               mean = test_means,
               nvalid = test_not_na)
  class(out1) <- c("test_summary", class(out1))
  out1
}

