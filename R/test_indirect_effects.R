#' @title Test Several Indirect Effects
#'
#' @description Test several indirect effects
#' for a `power4test` object.
#'
#' @details
#' This function is to be used in
#' [power4test()] for testing an
#' indirect effect, by setting it
#' to the `test_fun` argument.
#'
#' It uses [manymome::many_indirect_effects()]
#' to do the test. It can be used on
#' models fitted by [lavaan::sem()]
#' or fitted by a sequence of calls
#' to [stats::lm()], although only
#' nonparametric bootstrap confidence
#' interval is supported for models
#' fitted by regression using
#' [stats::lm()].
#'
#' @return
#' In its normal usage, it returns
#' a data frame with the
#' following columns:
#'
#' - `est`: The estimated
#'  indirect effect for each path.
#'
#' - `cilo` and `cihi`: The
#'  lower and upper limits of the
#'  confidence interval (95% by
#'  default), respectively,
#'  for each indirect effect
#'
#' - `sig`: Whether a test by confidence
#'  interval is significant (`1`) or
#'  not significant (`0`).
#'
#' - `test_label`: A column of labels
#'  generated to label the indirect
#'  effects.
#'
#' If `omnibus` is `"all_sig"` or
#' `"at_least_one"sig"`, then
#' the data frame has only one row,
#' and the columns `"est"`, `"cilo"`,
#' and `"cihi"` are `NA`. The column
#' `sig` is determined by whether
#' all paths are significant (`"all_sig"`)
#' or whether at least one path is
#' significant (`"at_least_one_sig"`).
#'
#'
#' @inheritParams test_indirect_effect
#'
#' @param m Must be a list of character
#' vectors. Each character vector stores the
#' name(s) of mediator(s) along a path.
#' The path
#' moves from the first mediator in the
#' vector to the last mediator in the
#' vector. If `NULL`, the stored paths
#' will be used, which are all the
#' indirect paths in the model between
#' `x` and `y`, by default.
#'
#'
#' @param ... Additional arguments to
#' be passed to [manymome::many_indirect_effects()].
#'
#' @param omnibus If `"no"`, the default,
#' then the test results for all paths
#' are stored. If `"all_sig"`, then
#' only one row of test is stored, and
#' the test is declared significant if
#' *all* paths are significant. If
#' `"at_least_one_sig"`, then only
#' one row of test is stored, and the
#' test is declared significant if
#' at least one of the paths is
#' significant. If `"at_least_k_sig"`,
#' then only one row of test is stored,
#' and the test is declared significant
#' if at least `k` of the paths is
#' significant, `k` determined by the
#' argument `at_least_k`.
#'
#' @param at_least_k The minimum number
#' of paths required to be significant
#' for the omnibus test to be considered
#' significant. Used when
#' `omnibus` is `"at_least_k_sig"`.
#'
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#' # Specify the model
#'
#' model_simple_med <-
#' "
#' m1 ~ x
#' m2 ~ x
#' y ~ m1 + m2 + x
#' "
#'
#' # Specify the population values
#'
#' model_simple_med_es <-
#' "
#' y ~ m1: s
#' m1 ~ x: m
#' y ~ m2: s
#' m2 ~ x: l
#' y ~ x: n
#' "
#'
#' # Simulate the data
#'
#' sim_only <- power4test(nrep = 5,
#'                        model = model_simple_med,
#'                        pop_es = model_simple_med_es,
#'                        n = 100,
#'                        R = 100,
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' # Do the test in each replication
#'
#' test_ind <- power4test(object = sim_only,
#'                        test_fun = test_k_indirect_effects,
#'                        test_args = list(x = "x",
#'                                         y = "y",
#'                                         mc_ci = TRUE))
#' print(test_ind,
#'       test_long = TRUE)
#'
#' # Set omnibus = "all_sig" to declare
#' # significant only if all paths are
#' # significant
#'
#' test_ind_all_sig <- power4test(
#'                        object = sim_only,
#'                        test_fun = test_k_indirect_effects,
#'                        test_args = list(x = "x",
#'                                         y = "y",
#'                                         mc_ci = TRUE,
#'                                         omnibus = "all_sig"))
#' print(test_ind_all_sig,
#'       test_long = TRUE)
#'
#' @export

test_k_indirect_effects <- function(
                            fit = fit,
                            x = NULL,
                            m = NULL,
                            y = NULL,
                            mc_ci = TRUE,
                            mc_out = NULL,
                            boot_ci = FALSE,
                            boot_out = NULL,
                            check_post_check = TRUE,
                            test_method = c("ci", "pvalue"),
                            ...,
                            omnibus = c("no", "all_sig", "at_least_one_sig", "at_least_k_sig"),
                            at_least_k = 1,
                            fit_name = "fit",
                            get_map_names = FALSE,
                            get_test_name = FALSE
                          ) {
  test_method <- match.arg(test_method)
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
  omnibus <- match.arg(omnibus)
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
    tmp <- paste0(x, "-...->", y)
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
    return(paste0("test_indirects: ", tmp, collapse = ""))
  }
  if (boot_ci) mc_ci <- FALSE
  if (inherits(fit, "lavaan")) {
    fit_ok <- lavaan::lavInspect(fit, "converged") &&
              (suppressWarnings(lavaan::lavInspect(fit, "post.check") ||
               !check_post_check))
  } else {
    fit_ok <- TRUE
  }
  if (fit_ok) {
    all_paths <- try(fit@external$fit_external$all_paths, silent = TRUE)
    if (inherits(all_paths, "try-error") ||
        isTRUE(is.null(all_paths))) {
      all_paths <- try(attr(fit, "fit_external")$all_paths, silent = TRUE)
      if (inherits(all_paths, "try-error") ||
          isTRUE(is.null(all_paths))) {
        stop("Paths not found Please test each path separately.")
      }
    }
    if (is.null(m)) {
      i_xy <- sapply(
                all_paths,
                function(xx) {
                  (xx$x == x) && (xx$y == y)
                },
                simplify = TRUE
              )
      if (!any(i_xy)) {
        stop("No indirect path between ", x, " and ", y, ".")
      }
      all_paths0 <- all_paths[i_xy]
    } else {
      i_xy <- sapply(
                all_paths,
                function(xx) {
                  i0 <- (xx$x == x) && (xx$y == y)
                  i1 <- FALSE
                  for (mm in m) {
                    mm <- unname(mm)
                    mm_i <- unname(xx$m)
                    if (identical(mm, mm_i)) {
                      i1 <- TRUE
                      break
                    }
                  }
                  i0 && i1
                },
                simplify = TRUE
              )
      if (!any(i_xy)) {
        stop("No requested paths found in the model.")
      }
      all_paths0 <- all_paths[i_xy]
    }
    class(all_paths0) <- class(all_paths)
    out <- tryCatch(manymome::many_indirect_effects(
                                   paths = all_paths0,
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
    out <- NA
  }
  if (inherits(out, "error") ||
      identical(out, NA)) {
    out2 <- data.frame(
              est = as.numeric(NA),
              cilo = as.numeric(NA),
              cihi = as.numeric(NA),
              sig = as.numeric(NA)
            )
    return(out2)
  }
  out1 <- manymome::indirect_effects_from_list(
                              out,
                              add_sig = FALSE,
                              pvalue = TRUE,
                              se = FALSE
                            )
  out1 <- cbind(test_label = rownames(out1),
                out1)
  rownames(out1) <- NULL
  out1_names <- colnames(out1)
  out1_names <- gsub("CI.lo", "cilo", out1_names)
  out1_names <- gsub("CI.hi", "cihi", out1_names)
  out1_names <- gsub("ind", "est", out1_names)
  out1_names <- gsub("std", "est", out1_names)
  colnames(out1) <- out1_names
  if (test_method == "ci") {
    sig <- ifelse((out1$cilo > 0) |
                  (out1$cihi < 0),
                  yes = 1,
                  no = 0)
    out1$sig <- sig
  }
  R_case <- ""
  bz_alpha_ok <- isTRUE(all.equal(1 - out[[1]]$level,
                                  getOption("power4mome.bz.alpha",
                                                      default = .05)))
  if (test_method == "pvalue") {
    out1$sig <- ifelse(
                out1$pvalue < (1 -  out[[1]]$level),
                yes = 1,
                no = 0
              )
    R <- sapply(out,
                \(x) max(length(x$boot_indirect),
                         length(x$mc_indirect)))
    R_case <- bz_case(R[1])
    nlt0 <- sapply(out,
                \(x) max(sum(as.numeric(x$boot_indirect < 0)),
                         sum(as.numeric(x$mc_indirect < 0))))
    out1$R <- unname(R)
    out1$nlt0 <- unname(nlt0)
    out1$alpha <- unname(1 -  out[[1]]$level)
    if (bz_alpha_ok) {
      tmp <- lapply(out,
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
      out1 <- cbind(out1,
                    tmp)
    }
  }
  if (omnibus == "no") {
    attr(out1, "test_label") <- "test_label"
    return(out1)
  } else {
    out2 <- out1[1, ]
    tmp <- paste0(c(x, y), collapse = "-...->")
    tmp <- paste0(tmp, switch(omnibus,
                              all_sig = " (All sig)",
                              at_least_one_sig = " (1+ sig)",
                              at_least_k_sig = paste0(" (",
                                                      at_least_k,
                                                      "+ sig)")))
    out2[1, "test_label"] <- tmp
    out2[, c("est", "cilo", "cihi")] <- as.numeric(NA)
    tmp <- switch(omnibus,
                  all_sig = as.numeric(isTRUE(all(out1$sig == 1))),
                  at_least_one_sig = as.numeric(isTRUE(any(out1$sig == 1))),
                  at_least_k_sig = as.numeric(isTRUE(sum(out1$sig == 1) >= at_least_k)))
    out2$sig <- tmp
    if ((R_case == "one") &&
        (omnibus == "all_sig") &&
        bz_alpha_ok) {
      out1_bz <- add_bz_i(out1)
      out1_bz <- out1_bz[, !(colnames(out1_bz) %in% colnames(out2))]
      out1_bz2 <- apply(
                      out1_bz,
                      MARGIN = 2,
                      min
                    )
      out1_bz2 <- rbind(out1_bz2)
      rownames(out1_bz2) <- NULL
      out2 <- cbind(out2, out1_bz2)
    }
    if ((R_case == "cum") &&
        (omnibus == "all_sig") &&
        bz_alpha_ok) {
      out1_bz <- out1
      out1_bz <- out1_bz[, grepl("bz_", colnames(out1_bz))]
      out1_bz2 <- apply(
                      out1_bz,
                      MARGIN = 2,
                      \(x) as.numeric(all(x == 1))
                    )
      out1_bz2 <- rbind(out1_bz2)
      rownames(out1_bz2) <- NULL
      out2[, colnames(out1_bz2)] <- out1_bz2
    }
    if (any(is.na(out2$sig))) {
      out2$sig <- as.numeric(NA)
    }
    attr(out2, "test_label") <- "test_label"
    return(out2)
  }
}
