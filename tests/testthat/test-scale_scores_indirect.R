library(testthat)

test_that("scale-scores: indirect effect", {

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

sim_only1 <- power4test(
                  nrep = 2,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 100,
                  number_of_indicators = k,
                  reliability = rel,
                  process_data = list(fun = "scale_scores"),
                  fit_model_args = list(estimator = "ML"),
                  R = 100,
                  do_the_test = FALSE,
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)

test_ind1 <- power4test(
                  object = sim_only1,
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  progress = !is_testing())
test_ind1

tmp <- pool_sim_data(sim_only1)
expect_setequal(colnames(tmp),
                c("x", "y", "m"))

})
