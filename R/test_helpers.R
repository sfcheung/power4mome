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
  test_results_all <- sapply(object,
                             function(xx) xx$test_results)
  test_results_all <- as.data.frame(t(test_results_all))
  colMeans(test_results_all, na.rm = TRUE)
}