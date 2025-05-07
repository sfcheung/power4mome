# Functions used in test

#' @noRd
test_summary <- function(object) {
  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  out <- sapply(object,
                test_summary_i,
                simplify = FALSE)
  out
}

#' @noRd
test_summary_i <- function(object) {
  test_i <- object[[1]]$test_results
  if (is.vector(test_i)) {
    # A vector
    out <- test_summary_i_vector_one_test(object)
    return(out)
  }
  if (isTRUE(length(dim(test_i)) == 2)) {
    # A table, likely a data frame
    out <- test_summary_i_data_frame(object)
    return(out)
  }
  stop("test_results not supported. Something's wrong.")
}

#' @noRd
test_summary_i_vector_one_test <- function(object) {
  test_results_all <- sapply(object,
                             function(xx) xx$test_results)
  test_results_all <- as.data.frame(t(test_results_all),
                                    check.names = FALSE)
  colMeans(test_results_all, na.rm = TRUE)
}

#' @noRd
test_summary_i_data_frame <- function(object) {
  test_i <- object[[1]]$test_results
  test_label <- attr(test_i, "test_label")
  i <- sapply(test_i,
              is.numeric)
  out0 <- sapply(test_i[, "test_label", drop = TRUE],
                 function(xx) {
                   t(sapply(object,
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
  out2 <- test_i
  out2[, i] <- out1
  class(out2) <- class(test_i)
  out2
}