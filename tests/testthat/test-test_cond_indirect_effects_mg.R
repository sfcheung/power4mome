library(testthat)

test_that("cond indirect effects: mg", {

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
y ~ x: nil
"

# Generate the data

dat1 <- power4test(
          nrep = 2,
          model = model_simple_med,
          pop_es = model_simple_med_es,
          n = c(100, 50),
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
                           mc_ci = TRUE),
          progress = !is_testing(),
          iseed = 1234
        )

tmp1 <- rejection_rates(out1)
expect_true(all(grepl("Group", tmp1$test_label)))

# One single conditional indirect effects

out2a <- power4test(
          dat1,
          test_fun = test_cond_indirect,
          test_args = list(x = "x",
                           m = "m",
                           y = "y",
                           mc_ci = TRUE,
                           group = "Group1"),
          progress = !is_testing(),
          iseed = 1234
        )

tmp2a <- rejection_rates(out2a)

out2b <- power4test(
          dat1,
          test_fun = test_cond_indirect,
          test_args = list(x = "x",
                           m = "m",
                           y = "y",
                           mc_ci = TRUE,
                           group = "Group2"),
          progress = !is_testing(),
          iseed = 1234
        )

tmp2b <- rejection_rates(out2b)

expect_true(all.equal(tmp1$est,
                      c(tmp2a$est, tmp2b$est)))

})
