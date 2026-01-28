#' @noRd
as_test_data_frame_i <- function(
  x,
  test_name
) {
  # Convert a test results vector to a one-row test results data frame
  if ("test_results" %in% names(x)) {
    # x should be the test results
    x <- x$test_results
  }
  if (!is.null(dim(x))) {
    # It is not a vector. Assume it is a data frame and return it as-is.
    # Skip checking x, for efficiency.
    return(x)
  }
  out <- data.frame(
            test_label = test_name,
            rbind(x)
          )
  attr(out, "test_label") <- "test_label"
  out
}

#' @noRd
as_test_data_frame <- function(
  test_output
) {
  # Convert a list of test vectors to a list of one-row test data frame
  test_name <- attr(
                  test_output,
                  "test_name"
                )
  for (i in seq_along(test_output)) {
    test_output[[i]]$test_results <-
      as_test_data_frame_i(test_output[[i]]$test_results,
                           test_name = test_name)
  }
  test_output
}

#' @noRd
as_test_data_frame_all_tests <- function(
  object
) {
  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  # object: The test_all element of a `power4test` object
  for (i in seq_along(object)) {
    object[[i]] <- as_test_data_frame(object[[i]])
  }
  object
}

#' @noRd

rbind_diff_cols <- function(x) {
  # Bind rows with different colnames
  test_names <- names(x)
  all_cnames <- lapply(x,
                       \(x) colnames(x))
  all_cnames <- unique(unlist(all_cnames,
                       use.names = FALSE))
  out <- lapply(x,
            \(xx) {
              tmp <- setdiff(all_cnames, colnames(xx))
              if (length(tmp) == 0) {
                return(xx)
              }
              xx[, tmp] <- NA
              xx
            })
  out <- mapply(function(xx, yy) {
                  out <- cbind(test = xx,
                               yy)
                  rownames(out) <- NULL
                  out
                },
                xx = test_names,
                yy = out,
                SIMPLIFY = FALSE)
  do.call(rbind, unname(out))
}

#' @noRd
collapse_all_tests <- function(
  object,
  keep = c("est", "cilo", "cihi", "sig", "pvalue"),
  alpha = .05,
  p_adjust_method = "none"
) {
  # p_adjust_method: To be passed to p.adjust()
  # alpha: Used if p_adjust_method != "none",
  #        Update `sig`.
  # Get test_all
  # Collapse all tests into one test
  # NOTE:
  # test_labels no longer relevant
  # because the column names are
  # not guaranteed to be the same
  # across tests.
  # This functions should be used only
  # with collapse across tests.

  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  test_all <- as_test_data_frame_all_tests(object)
  nrep <- length(test_all[[1]])

  out <- lapply(seq_len(nrep),
            function(i) {
              test_results_all_i <- lapply(test_all,
                                  \(x) x[[i]]$test_results)
              out1 <- rbind_diff_cols(test_results_all_i)
              tmp <- rep("--", nrow(out1))
              i <- out1$test_label == out1$test
              out1$test_label[i] <- tmp[i]
              # Some old tests may not have pvalues
              keep <- intersect(keep, colnames(out1))
              out1 <- out1[, c("test", "test_label", keep), drop = FALSE]
              if ((p_adjust_method != "none") &&
                  ("pvalue" %in% colnames(out1))) {
                if (!all(is.na(out1$pvalue))) {
                  out1$pvalue_org <- out1$pvalue
                  out1$pvalue <- stats::p.adjust(
                                        out1$pvalue_org,
                                        method = p_adjust_method
                                      )
                  out1$sig <- as.numeric(out1$pvalue < alpha)
                }
              }
              out1 <- list(test_results = out1)
              attr(out1$test_results, "test_label") <- "test_label"
              out1
            })
  attr(out, "test_name") <- "All tests merged into one"
  attr(out, "tests") <- names(object)
  out
}

