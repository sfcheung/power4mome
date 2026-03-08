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

# Default

sim_only1 <- power4test(
                  nrep = 1,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 12000,
                  process_data = list(fun = "missing_values"),
                  fit_model_args = list(estimator = "ML"),
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)
tmp <- pool_sim_data(sim_only1)
expect_setequal(colnames(tmp),
                c("x", "y", "m"))
chk <- mice::md.pattern(tmp, plot = FALSE)
chk2 <- as.numeric(rownames(chk))[1:4] / nrow(tmp)

expect_equal(chk2[1],
             .50,
             tolerance = 1e-1)

# Additional arguments 1

my_patterns <- matrix(1, nrow = 2, ncol = 3)
colnames(my_patterns) <- c("x", "y", "m")
my_patterns[1, "x"] <- 0
my_patterns[2, "y"] <- 0
my_patterns

sim_only2 <- power4test(
                  nrep = 1,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 12000,
                  process_data = list(
                    fun = "missing_values",
                    args = list(patterns = my_patterns)),
                  fit_model_args = list(estimator = "ML"),
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)
tmp <- pool_sim_data(sim_only2)
expect_setequal(colnames(tmp),
                c("x", "y", "m"))
chk <- mice::md.pattern(tmp, plot = FALSE)
chk2 <- as.numeric(rownames(chk))[1:4] / nrow(tmp)

expect_equal(chk2[1],
             .50,
             tolerance = 1e-1)
expect_all_equal(as.numeric(chk[1:3, "m"]),
                 1)


# Additional arguments 1

my_patterns <- matrix(1, nrow = 2, ncol = 3)
colnames(my_patterns) <- c("m", "x", "y")
my_patterns[1, "x"] <- 0
my_patterns[2, "y"] <- 0
my_patterns

sim_only3 <- power4test(
                  nrep = 1,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 10000,
                  process_data = list(
                    fun = "missing_values",
                    args = list(patterns = my_patterns,
                                mech = "MAR",
                                prop = .25)),
                  fit_model_args = list(estimator = "ML"),
                  progress = !is_testing(),
                  parallel = FALSE,
                  iseed = 1234)
tmp <- pool_sim_data(sim_only3)
expect_setequal(colnames(tmp),
                c("x", "y", "m"))
chk <- mice::md.pattern(tmp, plot = FALSE)
chk2 <- as.numeric(rownames(chk))[1:4] / nrow(tmp)

expect_equal(chk2[1],
             .75,
             tolerance = 1e-1)
expect_all_equal(as.numeric(chk[1:3, "m"]),
                 1)

# With indicators

k <- c(y = 3,
       m = 4,
       x = 5)
rel <- c(y = .70,
         m = .70,
         x = .70)

sim_only1 <- power4test(
                  nrep = 1,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  number_of_indicators = k,
                  reliability = rel,
                  n = 12000,
                  process_data = list(fun = "missing_values"),
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

})
