# WIP: Testing a print/summary function

#' @noRd
summarize_test <- function(object) {
  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  out <- sapply(object,
                summarize_test_i,
                simplify = FALSE)
  class(out) <- c("test_summary", class(out))
  out
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
  test_results_all <- as.data.frame(test_results_all)
  test_means <- colMeans(test_results_all, na.rm = TRUE)
  test_not_na <- apply(test_results_all,
                       2,
                       function(x) {sum(!is.na(x))})
  out1 <- list(test_attributes = attributes(x),
               nrep = nrep,
               mean = test_means,
               nvalid = test_not_na)
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
  out1
}

