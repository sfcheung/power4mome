library(testthat)

skip("WIP")

skip_if_not_installed("lmhelprs")

test_that("indirect effect by lm", {

test_summary <- function(object) {
  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  out <- sapply(object,
                test_summary_i,
                simplify = FALSE)
  out
}

test_summary_i <- function(object) {
  test_results_all <- sapply(object,
                             function(xx) xx$test_results)
  test_results_all <- as.data.frame(t(test_results_all))
  colMeans(test_results_all, na.rm = TRUE)
}

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

sim_only <- power4test(nrep = 5,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       fit_model_args = list(fit_function = lmhelprs::many_lm),
                       do_the_test = FALSE,
                       iseed = 1234)

#' @noRd

# The following function will be replaced by the official one

test_indirect <- function(fit = fit,
                          x = NULL,
                          m = NULL,
                          y = NULL,
                          mc_ci = TRUE,
                          mc_out = NULL,
                          ...,
                          get_map_names = FALSE,
                          get_test_name = FALSE) {
  map_names <- c(fit = "fit",
                mc_out = "mc_out")
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
    return(paste0("test_indirect: ", tmp, collapse = ""))
  }
  out <- manymome::indirect_effect(x = x,
                                   y = y,
                                   m = m,
                                   fit = fit,
                                   mc_ci = mc_ci,
                                   mc_out = mc_out,
                                   progress = FALSE,
                                   ...)
  ci0 <- stats::confint(out)
  out1 <- ifelse((ci0[1, 1] > 0) || (ci0[1, 2] < 0),
                  yes = 1,
                  no = 0)
  out2 <- c(est = unname(coef(out)),
            cilo = ci0[1, 1],
            cihi = ci0[1, 2],
            sig = out1)
  return(out2)
}

test_ind <- power4test(object = sim_only,
                       test_fun = test_indirect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE,
                                        mc_ci = FALSE))

(chk <- test_summary(test_ind))
expect_true(length(chk) == 1)

})
