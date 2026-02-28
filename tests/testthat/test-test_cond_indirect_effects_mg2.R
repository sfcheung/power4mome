library(testthat)

test_that("cond indirect effects: mg: compare groups", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <-
"
y ~ m: l
m ~ x:
  - nil
  - s
  - m
y ~ x: nil
"

# Generate the data

dat1 <- power4test(
          nrep = 2,
          model = model_simple_med,
          pop_es = model_simple_med_es,
          n = c(100, 50, 75),
          R = 100,
          ci_type = "mc",
          progress = !is_testing(),
          iseed = 1234
        )

# Multigroup tests

out1 <- power4test(
          dat1,
          test_fun = test_cond_indirect_effects,
          test_args = list(x = "x",
                           m = "m",
                           y = "y",
                           mc_ci = TRUE,
                           compare_groups = TRUE),
          progress = !is_testing(),
          iseed = 1234
        )

tmp1 <- rejection_rates(out1)
expect_true(all(grepl("Group", tmp1$test_label)))

out1b <- power4test(
          dat1,
          test_fun = test_cond_indirect_effects,
          test_args = list(x = "x",
                           m = "m",
                           y = "y",
                           mc_ci = TRUE),
          progress = !is_testing(),
          iseed = 1234
        )

tmp1b <- rejection_rates(out1b)

expect_equal(tmp1$est[c(1, 3)],
             diff(tmp1b$est))

})

test_that("cond indirect effects: mg: compare groups: two groups", {

model_simple_med <-
"
m ~ x
y ~ m + x
"

model_simple_med_es <-
"
y ~ m:
  - m
  - sm
m ~ x:
  - s
  - m
y ~ x: nil
"

# Generate the data

dat1 <- power4test(
          nrep = 2,
          model = model_simple_med,
          pop_es = model_simple_med_es,
          n = c(100, 75),
          R = 100,
          ci_type = "mc",
          progress = !is_testing(),
          iseed = 1234
        )

# Multigroup tests

out1 <- power4test(
          dat1,
          test_fun = test_cond_indirect_effects,
          test_args = list(x = "x",
                           m = "m",
                           y = "y",
                           mc_ci = TRUE,
                           compare_groups = TRUE),
          progress = !is_testing(),
          iseed = 1234
        )

tmp1 <- rejection_rates(out1)
expect_true(all(grepl("Group", tmp1$test_label)))

out1b <- power4test(
          dat1,
          test_fun = test_cond_indirect_effects,
          test_args = list(x = "x",
                           m = "m",
                           y = "y",
                           mc_ci = TRUE),
          progress = !is_testing(),
          iseed = 1234
        )

tmp1b <- rejection_rates(out1b)

expect_equal(diff(tmp1b$est),
             tmp1$est)

})
