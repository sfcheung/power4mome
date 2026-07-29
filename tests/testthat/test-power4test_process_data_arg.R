library(testthat)

test_that("Chain process", {

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

my_process_data <- \(x) x |>
                   missing_values(prop = .75) |>
                   ordinal_variables(cut_patterns = c(x = "-ma3", y = "-ma3", m = "-ma3")) |>
                   scale_scores()

sim_only1 <- power4test(
                  nrep = 1,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  number_of_indicators = k,
                  reliability = rel,
                  n = 12000,
                  process_data = list(fun = my_process_data),
                  fit_model_args = list(estimator = "ML"),
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)
tmp <- pool_sim_data(sim_only1) * 3

# Missing?
expect_true(any(!complete.cases(tmp)))
# Scale scores?
expect_true(ncol(tmp) == 3)

})
