library(testthat)

test_that("Merging test in by_* functions", {

skip("WIP")

model <-
"
m ~ x + z + x:z
y ~ m + x
"

model_es <-
"
m ~ x:z: s
m ~ x: m
y ~ m: m
y ~ x: s
"

out <- power4test(
            nrep = 2,
            model = model,
            pop_es = model_es,
            n = 100,
            test_fun = test_parameters,
            test_args = list(pars = "m~x"),
            ci_type = "mc",
            R = 100,
            iseed = 1234,
            parallel = FALSE,
            progress = !is_testing())

rejection_rates(out)

out2 <- power4test(
            out,
            test_fun = test_parameters,
            test_args = list(pars = c("m~z", "y~m")),
            iseed = 1234,
            parallel = FALSE,
            progress = !is_testing())

rejection_rates(out2)

out3 <- power4test(
            out2,
            test_fun = test_index_of_mome,
            test_args = list(x = "x",
                             y = "y",
                             m = "m",
                             w = "z"),
            iseed = 1234,
            parallel = FALSE,
            progress = !is_testing())

rejection_rates(out3,
                merge_all_tests = TRUE,
                collapse = "all_sig")

out <- power4test_by_n(out3,
                       n = c(100, 110),
                       by_seed = 1234,
                       progress = !is_testing())

rejection_rates(out,
                merge_all_tests = TRUE,
                collapse = "all_sig")

}
