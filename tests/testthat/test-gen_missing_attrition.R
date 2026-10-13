library(testthat)

test_that("missing_values", {

model_simple_med <-
"
m ~ a*x
y ~ b*m + x
ab := a * b
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

# ---- With indicators ----

k <- c(y = 3,
       m = 4,
       x = 5)
rel <- c(y = .70,
         m = .70,
         x = .70)

tmp1 <- from_attrition(
  vars_in_waves = list(
    "x",
    "m",
    "y"
  ),
  attrition_rate = .30
)

expect_equal(
  sum(.70^2, sapply(tmp1, \(x) x$prop)),
  1
)

tmp2 <- from_attrition(
  vars_in_waves = list(
    "x",
    c("m1", "m2"),
    c("m3"),
    "y"
  ),
  attrition_rate = .30
)

expect_equal(
  sum(.70^3, sapply(tmp2, \(x) x$prop)),
  1
)

my_patterns <- list(
  list(vars = c("y", "m"),
       prop = .20),
  list(vars = c("y"),
       prop = .10)
)

sim_only1 <- power4test(
                  nrep = 1,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  number_of_indicators = k,
                  reliability = rel,
                  n = 12000,
                  process_data = list(
                    fun = "missing_values",
                    args = list(
                      vars_in_waves = list(
                        "x",
                        "m",
                        "y"
                      ),
                      attrition_rate = .30
                    )
                  ),
                  fit_model_args = list(estimator = "ML"),
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)
tmp <- pool_sim_data(sim_only1)
chk <- mice::md.pattern(tmp, plot = FALSE)
chk2 <- as.numeric(rownames(chk))[1:4] / nrow(tmp)

expect_equal(chk2[1],
             .50,
             tolerance = 1e-1)
expect_equal(
  nrow(chk),
  4
)

expect_equal(
  unname(chk[, ncol(chk)][seq_len(nrow(chk)) - 1]),
  c(0, 3, 7)
)

# ---- Some with indicators ----

k <- c(y = 3,
       x = 5)
rel <- c(y = .70,
         x = .70)

sim_only1 <- power4test(
                  nrep = 1,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  number_of_indicators = k,
                  reliability = rel,
                  n = 12000,
                  process_data = list(
                    fun = "missing_values",
                    args = list(
                      vars_in_waves = list(
                        "x",
                        "m",
                        "y"
                      ),
                      attrition_rate = .30
                    )
                  ),
                  fit_model_args = list(estimator = "ML"),
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)
tmp <- pool_sim_data(sim_only1)
chk <- mice::md.pattern(tmp, plot = FALSE)
chk2 <- as.numeric(rownames(chk))[1:4] / nrow(tmp)

expect_equal(chk2[1],
             .49,
             tolerance = 1e-1)
expect_equal(
  nrow(chk),
  4
)

expect_equal(
  unname(chk[, ncol(chk)][seq_len(nrow(chk)) - 1]),
  c(0, 3, 4)
)

# ---- Some with indicators, common processes ----

k <- c(y = 3,
       x = 5)
rel <- c(y = .70,
         x = .70)

sim_only1 <- power4test(
                  nrep = 1,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  number_of_indicators = k,
                  reliability = rel,
                  n = 12000,
                  process_data = list(
                    fun = "common_processes",
                    args = list(
                      vars_in_waves = list(
                        "x",
                        "m",
                        "y"
                      ),
                      attrition_rate = .30
                    )
                  ),
                  fit_model_args = list(estimator = "ML"),
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)
tmp <- pool_sim_data(sim_only1)
chk <- mice::md.pattern(tmp, plot = FALSE)
chk2 <- as.numeric(rownames(chk))[1:4] / nrow(tmp)

expect_equal(chk2[1],
             .49,
             tolerance = 1e-1)
expect_equal(
  nrow(chk),
  4
)

expect_equal(
  unname(chk[, ncol(chk)][seq_len(nrow(chk)) - 1]),
  c(0, 3, 4)
)


})
