#' @title Process Data by Computing Scale Scores
#'
#' @description For the `process_data`
#' argument. To compute scale scores
#' from indicators and replace the
#' indicators scores by computed
#' scale scores.
#'
#' @details
#' This function is to be used in
#' the `process_data` argument of
#' [power4test()].
#'
#' It retrieves the attribute
#' `"number_of_indicators"`,
#' stored by [power4test()], to identify
#' factors with indicators, computes
#' the scale scores based on `method`,
#' and replace the indicators by the
#' scale scores.
#'
#' All subsequent steps, such as the
#' test functions, will see only the
#' scale scores, or original scores if
#' a variable has no indicator. The
#' model will also be fitted on the
#' scale scores, not on the indicators.
#'
#' It can be used to estimate power
#' for analyzing the scale scores,
#' taking into account the measurement
#' error due to imperfect reliability.
#'
#' @return
#' It returns a data frame with the
#' scale scores computed.
#'
#' @param data A data frame with the
#' indicator scores. It must has an
#' attribute `number_of_indicators`.
#' The same argument used by
#' [power4test()]. This attribute is
#' used to identify the factor names
#' and their indicators.
#'
#' @param method The method to be used
#' to compute the scale scores. Can
#' be `"mean"` or `"sum"`. The default
#' `na.rm = FALSE` will be used.
#' Therefore, `data` must not have
#' missing data.
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
#' mod_es <-
#' "
#' y ~ m: l
#' m ~ x: m
#' y ~ x: n
#' "
#'
#' # Specify the numbers of indicators and reliability coefficients
#'
#' k <- c(y = 3,
#'        m = 4,
#'        x = 5)
#' rel <- c(y = .70,
#'          m = .70,
#'          x = .70)
#'
#' # Simulate the data
#'
#' out <- power4test(
#'          nrep = 2,
#'          model = mod,
#'          pop_es = mod_es,
#'          n = 200,
#'          number_of_indicators = k,
#'          reliability = rel,
#'          process_data = list(fun = "scale_scores"),
#'          test_fun = test_parameters,
#'          test_args = list(op = "~"),
#'          parallel = FALSE,
#'          iseed = 1234)
#'
#' dat <- pool_sim_data(out)
#' head(dat)
#'
#' @export
scale_scores <- function(data,
                         method = c("mean", "sum")) {
  method <- match.arg(method)
  score_function <- switch(
                      method,
                      mean = mean,
                      sum = sum)
  k_ind <- attr(data, "number_of_indicators")
  # ==== Any factors with indicators? ====
  if (isTRUE(is.null(k_ind)) ||
      (all(k_ind == 1))) {
    return(data)
  }
  f_names <- names(k_ind)
  f <- function(x, xk) {
    paste0(x, seq_len(xk))
  }
  ind_names <- mapply(
                FUN = f,
                x = f_names,
                xk = k_ind,
                SIMPLIFY = FALSE
              )
  for (xi in f_names) {
    xi_ind <- ind_names[[xi]]
    xi_scores <- apply(
                  data[, xi_ind, drop = FALSE],
                  MARGIN = 1,
                  FUN = score_function
                )
    data[, xi_ind] <- NULL
    data[, xi] <- xi_scores
  }
  return(data)
}