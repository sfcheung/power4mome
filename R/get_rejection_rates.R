
#' @title Rejection Rates of All Tests
#'
#' @description Get all rejection rates
#' of all tests stored in a `power4test`
#' object.
#'
#' @details
#' It loops over the tests stored
#' in a `power4test` object and retrieve
#' the rejection rate of each test.
#'
#' @return
#' A data frame with the number of
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
#' @param object A `power4test` object.
#'
#' @seealso [power4test()]
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
#' get_rejection_rates(test_out)
#'
#' @export
get_rejection_rates <- function(object) {
  out0 <- summarize_tests(object)
  out1 <- lapply(out0,
                 get_rejection_rates_i)
  out2 <- do.call(rbind,
                  out1)
  rownames(out2) <- NULL
  out2
}

#' @noRd
get_rejection_rates_i <- function(object_i) {
  if (is.vector(object_i$mean)) {
    out <- get_rejection_rates_i_vector(object_i)
    return(out)
  }
  if (length(dim(object_i$mean)) == 2) {
    # Likely a data frame
    out <- get_rejection_rates_i_data_frame(object_i)
    return(out)
  }
}

#' @noRd
get_rejection_rates_i_vector <- function(object_i) {
  test_args <- object_i$test_attributes
  test_name <- test_args$test_name
  out_i <- data.frame(test = test_name,
                      label = "Test",
                      pvalid = object_i$nvalid["sig"] / object_i$nrep,
                      reject = object_i$mean["sig"],
                      row.names = NULL)
  out_i
}

#' @noRd
get_rejection_rates_i_data_frame <- function(object_i) {
  test_args <- object_i$test_attributes
  test_name <- test_args$test_name
  out_i0 <- object_i$mean
  p <- nrow(out_i0)
  pvalid <- object_i$nvalid[, "sig", drop = TRUE] / object_i$nrep
  out_i <- data.frame(test = test_name,
                      label = out_i0$test_label,
                      pvalid = pvalid,
                      reject = object_i$mean[, "sig", drop = TRUE],
                      row.names = NULL)
  out_i
}
