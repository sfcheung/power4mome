library(testthat)
suppressMessages(library(lavaan))

test_that("summarize tests: merge tests", {

model <-
"
m ~ x + z + x:z
y ~ m + x
"

model_es <-
"
m ~ x:z: s
m ~ x: s
y ~ m: s
y ~ x: s
"

out <- power4test(
            nrep = 10,
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

tmp2 <- collapse_all_tests(out3)

expect_true("pvalue" %in% colnames(tmp2[[1]]$test_results))

tmp3 <- collapse_all_tests(out3,
                           p_adjust_method = "BH")

expect_true("pvalue" %in% colnames(tmp3[[1]]$test_results))
expect_true("pvalue_org" %in% colnames(tmp3[[1]]$test_results))

p_org <- tmp3[[1]]$test_results$pvalue_org
p_adj <- tmp3[[1]]$test_results$pvalue

expect_equal(p.adjust(p_org, method = "BH"),
             p_adj)

tmp <- summarize_tests(out3,
                       merge_all_tests = TRUE,
                       p_adjust_method = "BH")

expect_output(print(tmp),
              "merged")
expect_true(length(tmp) == 1)

tmp2 <- summarize_tests(out3,
                       merge_all_tests = TRUE)

expect_all_true(tmp[[1]]$mean$pvalue >= tmp2[[1]]$mean$pvalue)

tmp <- summarize_tests(out3,
                       collapse = "at_least_k",
                       at_least_k = 1,
                       merge_all_tests = TRUE,
                       p_adjust_method = "BH")
expect_true(nrow(tmp[[1]]$mean) == 1)

expect_output(print(tmp),
              "merged")

tmp2 <- summarize_tests(out3,
                       collapse = "at_least_k",
                       at_least_k = 1,
                       merge_all_tests = TRUE)
tmp <- rejection_rates(out3,
                merge_all_tests = TRUE,
                p_adjust_method = "BH")

expect_output(print(tmp),
              "merged")

tmp2 <- rejection_rates(out3,
                merge_all_tests = TRUE)

expect_output(print(tmp2),
              "merged")

})
