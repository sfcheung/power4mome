skip("WIP")

library(testthat)

scale_scores <- function(data,
                         model,
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

test_that("indirect effect", {

model_simple_med <-
"
m ~ a*x
y ~ b*m + x
ab := a * b
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")
k <- c(y = 3,
       m = 3,
       x = 3)
rel <- c(y = .70,
         m = .70,
         x = .70)

sim_only1 <- power4test(nrep = 500,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 200,
                       number_of_indicators = k,
                       reliability = rel,
                       process_data = list(fun = "scale_scores"),
                       fit_model_args = list(estimator = "ML"),
                       R = 1000,
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       parallel = FALSE,
                       iseed = 1234)

test_ind1 <- power4test(object = sim_only1,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        mc_ci = TRUE),
                       progress = !is_testing())
test_ind1

sim_only2 <- power4test(nrep = 500,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 200,
                       number_of_indicators = k,
                       reliability = rel,
                       fit_model_args = list(estimator = "ML"),
                       R = 1000,
                       do_the_test = FALSE,
                       progress = !is_testing(),
                       parallel = FALSE,
                       iseed = 1234)

test_ind2 <- power4test(object = sim_only2,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        mc_ci = TRUE),
                       progress = !is_testing())

rejection_rates(test_ind1)
rejection_rates(test_ind2)

})
