#' @title Test Several Conditional Indirect Effects
#'
#' @description Test several conditional
#' indirect effects
#' for a `power4test` object.
#'
#' @details
#' This function is to be used in
#' [power4test()] for testing several
#' conditional
#' indirect effects, by setting it
#' to the `test_fun` argument.
#'
#' It uses [manymome::cond_indirect_effects()]
#' to do the test. It can be used on
#' models fitted by [lavaan::sem()]
#' or fitted by a sequence of calls
#' to [stats::lm()], although only
#' nonparametric bootstrap confidence
#' interval is supported for models
#' fitted by regression using
#' [stats::lm()].
#'
#' It can also be used to test
#' conditional effects on a direct path
#' with no mediator. Just omit `m` when
#' calling the function.
#'
#' @return
#' In its normal usage, it returns
#' the output returned by
#' [manymome::cond_indirect_effects()],
#' with the following modifications:
#'
#' - `est`: The estimated
#'  conditional indirect effects.
#'
#' - `cilo` and `cihi`: The
#'  lower and upper limits of the
#'  confidence interval (95% by
#'  default), respectively.
#'
#' - `sig`: Whether a test by confidence
#'  interval is significant (`1`) or
#'  not significant (`0`).
#'
#' - `test_label`: A column of labels
#'  generated to label the conditional
#'  effects.
#'
#' @inheritParams test_indirect_effect
#'
#' @param fit The fit object, to be
#' passed to [manymome::cond_indirect_effects()].
#'
# @param x <- Inherited
#'
# @param m <- Inherited
#'
# @param y <- Inherited
#'
#' @param wlevels The output of
#' [manymome::merge_mod_levels()], or
#' the moderator(s) to be passed to
#' [manymome::mod_levels_list()].
#' If all the moderators can be
#' represented by one variable, that is,
#' each moderator is (a) a numeric
#' variable, (b) a dichotomous
#' categorical variable, or (c) a
#' factor or string variable used in
#' [stats::lm()] in fit, then it is a
#' vector of the names of the moderators
#' as appeared in the data frame. If at
#' least one of the moderators is a
#' categorical variable represented by
#' more than one variable, such as
#' user-created dummy variables used in
#' [lavaan::sem()], then it must be a
#' list of the names of the moderators,
#' with such moderators represented by
#' a vector of names. For example:
#' `list("w1", c("gpgp2", "gpgp3")`,
#' the first moderator `w1` and the
#' second moderator a three-category
#' variable represented by `gpgp2` and
#' `gpgp3`. See the help page of
#' [manymome::cond_indirect_effects()]
#' for further details.
#'
# @param mc_ci <- Inherited
#'
# @param mc_out <- Inherited
#'
# @param boot_ci <- Inherited
#'
# @param boot_out <- Inherited
#'
#' @param ... Additional arguments to
#' be passed to [manymome::cond_indirect_effects()].
#'
# @param fit_name <- Inherited
#'
# @param get_map_names <- Inherited
#'
# @param get_test_name <- Inherited
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#' # Specify the model
#'
#' model_simple_mod <-
#' "
#' m ~ x + w + x:w
#' y ~ m + x
#' "
#'
#' # Specify the population values
#'
#' model_simple_mod_es <-
#' "
#' y ~ m: l
#' y ~ x: n
#' m ~ x: m
#' m ~ w: n
#' m ~ x:w: l
#' "
#'
#' # Simulate the data
#'
#' sim_only <- power4test(nrep = 5,
#'                        model = model_simple_mod,
#'                        pop_es = model_simple_mod_es,
#'                        n = 100,
#'                        R = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' # Do the tests in each replication
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_cond_indirect_effects,
#'                        test_args = list(x = "x",
#'                                         m = "m",
#'                                         y = "y",
#'                                         wlevels = c("w"),
#'                                         mc_ci = TRUE))
#' print(test_out,
#'       test_long = TRUE)
#'
#' @export

test_cond_indirect_effects <- function(fit = fit,
                                       x = NULL,
                                       m = NULL,
                                       y = NULL,
                                       wlevels = NULL,
                                       mc_ci = TRUE,
                                       mc_out = NULL,
                                       boot_ci = FALSE,
                                       boot_out = NULL,
                                       ...,
                                       fit_name = "fit",
                                       get_map_names = FALSE,
                                       get_test_name = FALSE) {
  if (fit_name != "fit") {
    mc_name <- paste0(fit_name, "_mc_out")
    boot_name <- paste0(fit_name, "_boot_out")
  } else {
    mc_name <- "mc_out"
    boot_name <- "boot_out"
  }
  map_names <- c(fit = fit_name,
                 mc_out = mc_name,
                 boot_out = boot_name)
  if (get_map_names) {
    return(map_names)
  }
  if (get_test_name) {
    tmp <- paste0(c(x, m, y),
                  collapse = "->")
    args <- as.list(match.call())
    tmp2 <- character(0)
    if (isTRUE(args$standardized_x) && !isTRUE(args$standardized_y)) {
      tmp <- paste0(tmp, " ('x' standardized)")
    }
    if (!isTRUE(args$standardized_x) && isTRUE(args$standardized_y)) {
      tmp <- paste0(tmp, " ('y' standardized)")
    }
    if (isTRUE(args$standardized_x) && isTRUE(args$standardized_y)) {
      tmp <- paste0(tmp, " ('x' and 'y' standardized)")
    }
    return(paste0("test_cond_indirect_effects: ", tmp, collapse = ""))
  }
  if (boot_ci) mc_ci <- FALSE
  out <- manymome::cond_indirect_effects(x = x,
                                         y = y,
                                         m = m,
                                         wlevels = wlevels,
                                         fit = fit,
                                         mc_ci = mc_ci,
                                         mc_out = mc_out,
                                         boot_ci = boot_ci,
                                         boot_out = boot_out,
                                         progress = FALSE,
                                         ...)
  tmp <- rownames(attr(out, "wlevels"))
  tmp2 <- paste0(c(x, m, y),
                 collapse = "->")
  test_label <- paste(tmp2, "|", tmp)
  out2 <- as.data.frame(out,
                        check.names = FALSE)
  out2 <- cbind(test_label = test_label,
                out2)
  tmp <- colnames(out2)
  if ("std" %in% tmp) {
    tmp <- gsub("ind", "est_raw", tmp, fixed = TRUE)
    tmp <- gsub("std", "est", tmp, fixed = TRUE)
  } else {
    tmp <- gsub("ind", "est", tmp, fixed = TRUE)
  }
  tmp <- gsub("CI.lo", "cilo", tmp, fixed = TRUE)
  tmp <- gsub("CI.hi", "cihi", tmp, fixed = TRUE)
  tmp <- gsub("Sig", "sig", tmp, fixed = TRUE)
  colnames(out2) <- tmp
  out1 <- ifelse((out2$cilo > 0) | (out2$cihi < 0),
                 yes = 1,
                 no = 0)
  out2$sig <- out1
  rownames(out2) <- NULL
  attr(out2, "test_label") <- "test_label"
  return(out2)
}

#' @noRd

cond_name <- function(wvalues) {
  w1 <- names(wvalues)
  out <- paste0(w1, " = ", wvalues)
  out <- paste(out,
               collapse = "; ")
  return(out)
}