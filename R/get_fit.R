#' @title Get a Fit Object From a 'power4test' Object
#'
#' @description A helper to get the
#' fit object (e.g., a `lavaan` output)
#' for a replication the output of
#' [power4test()] and friends.
#'
#' @details
#' There are cases in which users would like
#' to examine the fit results in a replication.
#' If the sample size of a replication is
#' large enough, the fit results can also
#' be used to check the specification of
#' the model. The helper [get_sim_fit()] is
#' for extracting the stored fit results
#' from the output of [power4test()]
#' and friends.
#'
#' @return
#' If a specific object is requested,
#' it returns the fit object, such as
#' the output of [lavaan::sem()] or
#' [lmhelprs::many_lm()].
#'
#' If `which` is set to `NULL`, then
#' it returns a character vector of
#' the names of the supported fit objects.
#'
#' @param object A `power4test` object,
#' such as the output of [power4test()].
#'
#' @param which The name of the fit object
#' to be retrieved. If set to `NULL`,
#' it returns the names of supported
#' fit objects.
#'
#' @param fit_class A character vector
#' of the classes
#' of fit objects to be retrieved.
#'
#' @param i The replication from which
#' the fit object is to be retrieved.
#'
#' @seealso See [power4test()] for
#' the all-in-one function, on which
#' this function is to be used.
#'
#' @examples
#'
#' # Specify the model
#'
#' model_simple_med <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' # Specify the population values
#'
#' model_simple_med_es <-
#' "
#' m ~ x: m
#' y ~ m: l
#' y ~ x: n
#' "
#'
#' # Just a test with only two replications
#' out <- power4test(nrep = 2,
#'                   model = model_simple_med,
#'                   pop_es = model_simple_med_es,
#'                   n = 100,
#'                   test_fun = test_parameters,
#'                   test_args = list(pars = "m~x"),
#'                   iseed = 1234,
#'                   parallel = FALSE,
#'                   progress = TRUE)
#'
#' get_sim_fit(out)
#'
#' @export
get_sim_fit <- function(
  object,
  which = "fit",
  fit_class = c("lavaan", "lm_list_lmhelprs"),
  i = 1
) {
  if (!inherits(object, "power4test")) {
    stop('object not a power4test object.')
  }
  x0 <- object$sim_all[[1]]
  extra <- x0$extra
  is_fit <- sapply(
    extra,
    \(x) inherits(x, fit_class)
  )
  if (!any(is_fit)) {
    stop("No supported fit objects found.")
  }
  fit_names <- names(extra)[is_fit]
  if (is.null(which)) {
    return(fit_names)
  }
  if (!(which %in% fit_names)) {
    stop("No supported fit object named ", sQuote(which))
  }
  out <- extra[[which]]
  out
}
