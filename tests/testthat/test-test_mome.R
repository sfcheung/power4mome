library(testthat)

test_that("index of mome", {

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

mod <-
"
m ~ x + w + x:w
y ~ m
"

mod_es <- c("m ~ x" = "n",
            "y ~ x" = "m",
            "m ~ w" = "n",
            "m ~ x:w" = "l")

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       R = 100,
                       do_the_test = FALSE,
                       iseed = 1234)

#' @noRd

test_index_of_mome <- function(fit = fit,
                      x = NULL,
                      m = NULL,
                      y = NULL,
                      w = NULL,
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
    tmp <- paste0(tmp,
                  ", moderated by ",
                  w)
    return(paste0("test_index_of_mome: ", tmp, collapse = ""))
  }
  out <- manymome::index_of_mome(x = x,
                                 y = y,
                                 m = m,
                                 w = w,
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

test_out1 <- power4test(object = sim_only,
                         test_fun = test_index_of_mome,
                         test_args = list(x = "x",
                                          m = "m",
                                          y = "y",
                                          w = "w",
                                          mc_ci = TRUE))

(chk <- test_summary(test_out1))

fits <- lapply(sim_only$sim_all,
               function(x) x$extra$fit)
mc_outs <- lapply(sim_only$sim_all,
                  function(x) x$extra$mc_out)
chk_outs <- mapply(manymome::index_of_mome,
                   fit = fits,
                   mc_out = mc_outs,
                   MoreArgs = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   w = "w",
                                   mc_ci = TRUE),
                  SIMPLIFY = FALSE)
chk_cis <- lapply(chk_outs,
                  confint)
chk_cis <- do.call(rbind, chk_cis)
chk_sigs <- (chk_cis[, 1] > 0) | (chk_cis[, 2] < 0)

expect_equal(chk[[1]]["sig"],
             mean(chk_sigs),
             ignore_attr = TRUE)
expect_equal(chk[[1]][c("cilo", "cihi")],
             colMeans(chk_cis),
             ignore_attr = TRUE)

})
