#' @title Common Data Processing
#'
#' @description For the `process_data`
#' argument. It do three types of processing
#' in one step: Creating ordinal variables,
#' generating missing values, and computing
#' scales scores.
#'
#' @details
#' This function is to be used in
#' the `process_data` argument of
#' [power4test()].
#'
#' This function is simply a wrapper
#' of the following three functions:
#'
#' - [ordinal_variables()]
#'
#' - [missing_values()]
#'
#' - [scale_scores()]
#'
#' Please refer to these functions on
#' how the raw data is processed.
#'
#' @return
#' The function returns a data frame with
#' the processed data.
#'
#' @param data A data frame.
#'
#' @inheritParams ordinal_variables
#' @inheritParams missing_values
#' @inheritParams scale_scores
#'
#' @param missing_values_args A named
#' list of optional arguments to be
#' passed to [mice::ampute()]. Note that
#' `prop` and `mech` will override
#' the values set for them in
#' `missing_values_args`, if any.
#'
#' @seealso [power4test()]. See also
#' [ordinal_variables()], [missing_values],
#' and [scale_scores] for the processes
#' employed.
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
#'          process_data = list(
#'            fun = common_processes,
#'            args = list(
#'                      prop = .75,
#'                      cut_patterns = c(x = "-ma3", y = "-ma3", m = "-ma3")
#'                    )
#'            ),
#'          test_fun = test_parameters,
#'          test_args = list(op = "~"),
#'          parallel = FALSE,
#'          iseed = 1234)
#'
#' dat <- pool_sim_data(out)
#' head(dat, 50)
#'
#' @export
common_processes <- function(
  data,
  cut_patterns = NULL,
  cuts = NULL,
  missing_values_args = list(),
  prop = 0.5,
  mech = "MCAR",
  method = c("mean", "sum"),
  na.rm = FALSE
) {

  method <- match.arg(method)

  # ==== ordinal_variables ====

  data_new <- ordinal_variables(
    data = data,
    cut_patterns = cut_patterns,
    cuts = cuts
  )

  # ==== missing_values ====

  mv_args <- missing_values_args
  mv_args$prop <- prop
  mv_args$mech <- mech
  mv_args$data <- data_new
  data_new <- do.call(
    missing_values,
    mv_args
  )

  # ==== scale_scores ====

  data_new <- scale_scores(
    data = data_new,
    method = method,
    na.rm = na.rm
  )
  return(data_new)
}
