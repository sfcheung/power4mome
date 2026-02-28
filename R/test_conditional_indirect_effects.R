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
#' @param compare_groups If the model
#' is a multigroup model, compute and
#' test group differences for all pairwise
#' combinations of the groups. Ignored
#' if the model is a single-group model.
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
#' # Set nrep to a larger value in real analysis, such as 400
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
                                       check_post_check = TRUE,
                                       test_method = NULL,
                                       compare_groups = FALSE,
                                       ...,
                                       fit_name = "fit",
                                       get_map_names = FALSE,
                                       get_test_name = FALSE) {

  # ==== Enable pvalue? ====

  args <- list(...)
  if (!is.null(mc_out)) {
    R <- length(mc_out)
  } else if (!is.null(boot_out)) {
    R <- length(boot_out)
  } else {
    R <- NULL
  }
  R <- args$R %||% formals(manymome::cond_indirect_effects)$R
  ci_level <- args$level %||% formals(manymome::cond_indirect)$level
  R_bz_ok <- isTRUE(R %in% R_extrapolate(alpha = 1 - ci_level))
  bz_not_FALSE <- !isFALSE(options("power4mome.bz"))
  test_method_NULL <- is.null(test_method)
  if (R_bz_ok &&
      bz_not_FALSE &&
      test_method_NULL) {
    test_method <- "pvalue"
  } else {
    test_method <- match.arg(test_method,
                             c("ci", "pvalue"))
  }

  internal_options <- list()
  if (test_method == "pvalue") {
    internal_options <- utils::modifyList(internal_options,
                                          list(skip_ci = TRUE,
                                               pvalue_min_size = -Inf))
  }
  if (test_method == "ci") {
    internal_options <- utils::modifyList(internal_options,
                                          list(skip_ci = FALSE))
  }
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
  if (inherits(fit, "lavaan")) {
    ngroups <- lavaan::lavInspect(fit, "ngroups")
    fit_ok <- lavaan::lavInspect(fit, "converged") &&
              (suppressWarnings(lavaan::lavInspect(fit, "post.check") ||
               !check_post_check))
  } else {
    ngroups <- 1
    fit_ok <- TRUE
  }

  # ==== Compute conditional indirect effects ====

  if (fit_ok) {
    if (is.null(wlevels)) {

      # ==== wlevels is NULL: Multigroup? ====

      if (ngroups == 1) {
        stop("wlevels not supplied but the model is not a multigroup model.")
      }
      out <- tryCatch(manymome::cond_indirect_effects(
                                          x = x,
                                          y = y,
                                          m = m,
                                          fit = fit,
                                          mc_ci = mc_ci,
                                          mc_out = mc_out,
                                          boot_ci = boot_ci,
                                          boot_out = boot_out,
                                          progress = FALSE,
                                          ...,
                                          internal_options = internal_options),
                    error = function(e) e)
    } else {

      # ==== wlevels is not NULL ====

      out <- tryCatch(manymome::cond_indirect_effects(
                                          x = x,
                                          y = y,
                                          m = m,
                                          wlevels = wlevels,
                                          fit = fit,
                                          mc_ci = mc_ci,
                                          mc_out = mc_out,
                                          boot_ci = boot_ci,
                                          boot_out = boot_out,
                                          progress = FALSE,
                                          ...,
                                          internal_options = internal_options),
                    error = function(e) e)
    }
  } else {
    out <- NA
  }
  if (inherits(out, "error") ||
      identical(out, NA)) {
    out2 <- data.frame(
              est = as.numeric(NA),
              cilo = as.numeric(NA),
              cihi = as.numeric(NA),
              sig = as.numeric(NA),
              pvalue = as.numeric(NA)
            )
    return(out2)
  }
  # TODO:
  # - Update to support multigroup model with moderation in the future

  # ==== Prepare the output ====

  if (ngroups > 1) {

    # ==== Multigroup model ====

    if (compare_groups) {

      # ==== Compare groups ====

      level <- attr(out, "full_output")[[1]]$level
      out_org <- out
      out <- all_cond_diff(
                  out_org,
                  level = level
                )
    }
    tmp <- out$Group
    tmp2 <- paste0(c(x, m, y),
                   collapse = "->")
    test_label <- paste(tmp2, "|", tmp)
    out2 <- as.data.frame(out,
                          check.names = FALSE)
    out2 <- cbind(test_label = test_label,
                  out2)
  } else {

    # ==== Single-group model ====

    tmp <- rownames(attr(out, "wlevels"))
    tmp2 <- paste0(c(x, m, y),
                   collapse = "->")
    test_label <- paste(tmp2, "|", tmp)
    out2 <- as.data.frame(out,
                          check.names = FALSE)
    out2 <- cbind(test_label = test_label,
                  out2)
  }
  # Add pvalues
  tmpfct <- function(x) {
    x$mc_p %||% (x$boot_p %||% as.numeric(NA))
  }
  out_p <- sapply(attr(out, "full_output"),
                  tmpfct,
                  USE.NAMES = FALSE)
  i <- match("Sig", colnames(out2))
  out2 <- cbind(out2[, 1:i],
                pvalue = out_p,
                out2[, -c(1:i)])

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
  if (test_method == "ci") {
    out1 <- ifelse((out2$cilo > 0) | (out2$cihi < 0),
                  yes = 1,
                  no = 0)
    out2$sig <- out1
  }
  if (test_method == "pvalue") {
    out_all <- attr(out,
                    "full_output")
    ci_type <- NA
    if (!is.null(out_all[[1]]$mc_indirect)) {
      ci_type <- "mc"
    }
    if (!is.null(out_all[[1]]$boot_indirect)) {
      ci_type <- "boot"
    }
    p_name <- switch(ci_type,
                     mc = "mc_p",
                     boot = "boot_p")
    p_all <- sapply(out_all,
                    \(x) unname(x[[p_name]]),
                    USE.NAMES = FALSE)
    out2$pvalue <- p_all
    out2$sig <- ifelse(
                out2$pvalue < (1 -  out_all[[1]]$level),
                yes = 1,
                no = 0
              )
    est_name <- switch(ci_type,
                       mc = "mc_indirect",
                       boot = "boot_indirect")
    est_all <- lapply(out_all,
                      \(x) x[[est_name]])
    bz_alpha_ok <- isTRUE(all.equal(1 - out_all[[1]]$level,
                                    getOption("power4mome.bz.alpha",
                                                        default = .05)))
    R <- sapply(est_all,
                length)
    nlt0 <- sapply(est_all,
                   \(x) sum(as.numeric(x < 0)))
    out2$R <- R
    out2$nlt0 <- nlt0
    out2$alpha <- 1 - out_all[[1]]$level
    if (bz_alpha_ok) {
      tmp <- lapply(out_all,
              function(x) {
                boot_est <- x$boot_indirect %||% x$mc_indirect
                boot_sig <- bz_sig_partition(
                              boot_est,
                              alpha = 1 - x$level
                            )
                boot_sig
              })
      tmp <- do.call(rbind,
                    unname(tmp))
      out2 <- cbind(out2,
                    tmp)
    }
  }
  rownames(out2) <- NULL
  attr(out2, "test_label") <- "test_label"
  return(out2)
}

#' @noRd
# Input:
# - A cond_indirect_effects object
# Output:
# - A data frame of pairwise comparison
all_cond_diff <- function(
  out,
  level
) {
  k <- nrow(out)
  all_ij <- utils::combn(
              seq_len(k),
              m = 2,
              simplify = FALSE
            )
  outa <- lapply(
              all_ij,
              \(xx) {
                ij_cond_diff(
                  out,
                  from = xx[1],
                  to = xx[2],
                  level = level
                )
              }
            )
  full_output <- lapply(
                    outa,
                    \(x) attr(x, "full_output")
                  )
  outz <- do.call(
            rbind,
            outa
          )
  attr(outz, "full_output") <- full_output
  outz
}

#' @noRd
# Input:
# - A cond_indirect_effects object
# Output:
# - A vector with results from a cond_indirect_diff object
ij_cond_diff <- function(
  out,
  from,
  to,
  level
) {
  outa <- manymome::cond_indirect_diff(
              out,
              from = from,
              to = to,
              level = level
            )
  # Need these
  # - est: index
  # - ci: cilo, cihi
  # - pvalue
  # - sig
  # - test_label
  ind <- outa$index
  CI.lo <- outa$ci[1]
  CI.hi <- outa$ci[2]
  if (!identical(outa$mc_diff, NA)) {
    boot_diff <- outa$mc_diff
  } else if (!identical(outa$boot_diff, NA)) {
    boot_diff <- outa$boot_diff
  } else {
    boot_diff <- NULL
  }
  pvalue <- est2p(
              boot_diff,
              min_size = -Inf
            )
  Sig <- ifelse((CI.lo > 0) | (CI.hi < 0),
                yes = "Sig",
                "")
  label <- paste0(
              out$Group[to],
              " - ",
              out$Group[from]
            )
  fake_out <- list()
  if (outa$ci_type == "mc") {
    fake_out$mc_indirect <- outa$mc_diff
    fake_out$mc_p <- pvalue
  } else if (outa$ci_type == "boot") {
    fake_out$boot_indirect <- outa$boot_diff
    fake_out$boot_p <- pvalue
  }
  fake_out$level <- level
  outb <- data.frame(
              Group = label,
              ind = ind,
              CI.lo = CI.lo,
              CI.hi = CI.hi,
              Sig = Sig,
              check.names = FALSE
            )
  row.names(outb) <- NULL
  attr(outb, "full_output") <- fake_out
  outb
}
