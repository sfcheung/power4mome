
test_that("power4test: update respect parallel", {
skip_on_cran()

model_simple_med <-
"
m ~ a*x
y ~ b*m + x
ab := a * b
"

model_simple_med_es <- c("y ~ m" = "l",
                         "m ~ x" = "m",
                         "y ~ x" = "n")

# Generate the data

tmp1 <- system.time(
out <- power4test(
        nrep = 10,
        model = model_simple_med,
        pop_es = model_simple_med_es,
        n = 50,
        test_fun = test_parameters,
        test_args = list(op = ":="),
        parallel = TRUE,
        ncores = 2,
        progress = TRUE,
        iseed = 1234)
)
tmp1
expect_true(tmp1["elapsed"] > 2 * tmp1["user.self"])

tmp2 <- system.time(
out2 <- power4test(
          out,
          test_fun = test_indirect_effect,
          test_args = list(x = "x",
                       m = "m",
                       y = "y"),
        )
)
expect_true(tmp2["elapsed"] > 2 * tmp2["user.self"])

})