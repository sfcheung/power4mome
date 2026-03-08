#' @title Process Data by Creating Ordinal Variables
#'
#' @description For the `process_data`
#' argument. It converts continuous
#' indicator variable to ordinal variables.
#'
#' @details
#' This function is to be used in
#' the `process_data` argument of
#' [power4test()].
#'
#' It is used for converting the
#' continuous indicator scores generated
#' to ordinal scores (with values 1, 2, 3, etc.).
#'
#' For example, if the cut points (thresholds) are
#' -2 and 2, then sores will be converted
#' to three categories: (-Inf to -2],
#' (-2 to 2], and (2 to Inf]. The intervals
#' are closed on the right. That is,
#' a score of -2 is in the interval
#' (-Inf to -2], not in the interval
#' (-2 to 2).
#'
#' The conversion is implemented by [cut()].
#'
#' There are two ways to specify the
#' conversion. The first one is to use
#' some built-in thresholds, based on
#' Savalei and Rhemtulla (2013), Table 1.
#' Call [cut_patterns()] with no argument
#' to list all the built-in patterns
#' and their names.
#'
#' Alternatively, users can specify the
#' thresholds directly through the argument
#' `cuts`.
#'
#' Currently, all indicators of a latent
#' variable must be converted in the same
#' way.
#'
#' @return
#' The function [ordinal_variables()]
#' returns a data frame with the
#' the converted scores.
#'
#' @param data A data frame.
#'
#' @param cut_patterns A named vector.
#' The names are the names of the latent
#' variables for which indicator scores
#' will be converted. Each value must be
#' the name of one of the built-in
#' patterns (call `cut_patterns()` to
#' list the patterns and their names).
#' Can be used with `cuts` but a latent
#' variable should appear only either
#' in `cut_patterns` or `cuts`, not both.
#'
#' @param cuts A named list.
#' The names are the names of the latent
#' variables for which indicator scores
#' will be converted.
#' Each element is a vector of the
#' thresholds for the conversion. `-Inf`
#' and `Inf` will be automatically
#' included during the conversion.
#' Can be used with `cut_patterns` but a latent
#' variable should appear only either
#' in `cut_patterns` or `cuts`, not both.
#'
#' @seealso [power4test()]. See also
#' [cut()] for the implementation.
#'
#' @references
#' Savalei, V., & Rhemtulla, M. (2013).
#' The performance of robust test
#' statistics with categorical data.
#' *British Journal of Mathematical and Statistical Psychology*,
#' *66*(2), 201--223.
#' \doi{10.1111/j.2044-8317.2012.02049.x}
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
#'          process_data = list(fun = "ordinal_variables",
#'                              args = list(cut_patterns = c(x = "s3"),
#'                                          cuts = list(m = c(-2, 0, 2)))),
#'          test_fun = test_parameters,
#'          test_args = list(op = "~"),
#'          parallel = FALSE,
#'          iseed = 1234)
#'
#' dat <- pool_sim_data(out)
#' head(dat, 50)
#'
#' # The built-in patterns
#'
#' cut_patterns()
#'
#' @export
ordinal_variables <- function(
  data,
  cut_patterns = NULL,
  cuts = NULL
) {
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
    if (xi %in% names(cut_patterns)) {
      xi_ind <- ind_names[[xi]]
      cut_patterns_i <- cut_patterns[xi]
      xi_scores <- apply(
                    data[, xi_ind, drop = FALSE],
                    MARGIN = 2,
                    FUN = cut_pattern_scores,
                    cut_pattern = cut_patterns_i
                  )
      data[, xi_ind] <- xi_scores
    } else if (xi %in% names(cuts)) {
      xi_ind <- ind_names[[xi]]
      cuts_i <- cuts[[xi]]
      xi_scores <- apply(
                    data[, xi_ind, drop = FALSE],
                    MARGIN = 2,
                    FUN = \(x, breaks) {
                      as.numeric(cut(x, breaks = breaks))
                    },
                    breaks = c(-Inf, cuts_i, Inf)
                  )
      data[, xi_ind] <- xi_scores
    }
  }
  return(data)
}

#' @param which A name of a built-in pattern.
#'
#' @return
#' The function [cut_patterns()]
#' returns a named list of the built-in
#' patterns if `which = NULL`, and
#' a numeric vector of the thresholds
#' of a built-in pattern if `which` is
#' set to one of the names of the built-in
#' patterns.
#'
#' @rdname ordinal_variables
#' @export
cut_patterns <- function(
  which = NULL
) {
  # Savalei & Rhemtulla (2013) Table 1
  pattern_list <- list(
    s2 = c(0),
    s3 = c(-.83, .83),
    s4 = c(-1.25, 0, 1.25),
    s5 = c(-1.50, -.50, .50, 1.50),
    s6 = c(-1.60, -.83, 0, .83, 1.60),
    s7 = c(-1.79, -1.07, -.36, .36, 1.07, 1.79),
    ma2 = c(.36),
    ma3 = c(-.50, .76),
    ma4 = c(-.31, .79, 1.66),
    ma5 = c(-.70, .39, 1.16, 2.05),
    ma6 = c(-1.05, .08, .81, 1.44, 2.33),
    ma7 = c(-1.43, -.43, .38, .94, 1.44, 2.54),
    ea2 = c(1.04),
    ea3 = c(.58, 1.13),
    ea4 = c(.28, .71, 1.23),
    ea5 = c(.05, .44, .84, 1.34),
    ea6 = c(-.13, .25, .61, .99, 1.48),
    ea7 = c(-.25, .13, .47, .81, 1.18, 1.64),
    `-ma2` = -rev(c(.36)),
    `-ma3` = -rev(c(-.50, .76)),
    `-ma4` = -rev(c(-.31, .79, 1.66)),
    `-ma5` = -rev(c(-.70, .39, 1.16, 2.05)),
    `-ma6` = -rev(c(-1.05, .08, .81, 1.44, 2.33)),
    `-ma7` = -rev(c(-1.43, -.43, .38, .94, 1.44, 2.54)),
    `-ea2` = -rev(c(1.04)),
    `-ea3` = -rev(c(.58, 1.13)),
    `-ea4` = -rev(c(.28, .71, 1.23)),
    `-ea5` = -rev(c(.05, .44, .84, 1.34)),
    `-ea6` = -rev(c(-.13, .25, .61, .99, 1.48)),
    `-ea7` = -rev(c(-.25, .13, .47, .81, 1.18, 1.64))
  )
  if (is.null(which)) {
    return(pattern_list)
  } else {
    if (which %in% names(pattern_list)) {
      return(pattern_list[[which]])
    } else {
      stop("The pattern ",
           sQuote(which),
           " is not supported. Call cut_patterns() for supported patterns.")
    }
  }
}

#' @noRd
cut_pattern_scores <- function(
  x,
  cut_pattern
) {
  out <- cut(
            x,
            breaks = c(-Inf,
                       cut_patterns(cut_pattern),
                       Inf))
  as.numeric(out)
}
