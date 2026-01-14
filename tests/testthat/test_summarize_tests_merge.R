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
            progress = FALSE)

rejection_rates(out)

out2 <- power4test(
            out,
            test_fun = test_parameters,
            test_args = list(pars = c("m~z", "y~m")),
            iseed = 1234,
            parallel = FALSE,
            progress = FALSE)

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
            progress = FALSE)

rejection_rates(out3)

out3$test_all[[3]][[1]]
out3$test_all[[2]][[1]]

attributes(out3$test_all[[3]])

attributes(out3$test_all[[2]][[1]]$test_results)

as_test_data_frame_i(out3$test_all[[3]][[1]],
                     test_name = attr(out3$test_all[[3]], "test_name"))
as_test_data_frame_i(out3$test_all[[3]][[1]]$test_results,
                     test_name = attr(out3$test_all[[3]], "test_name"))

expect_identical(as_test_data_frame(out3$test_all[[1]]),
                 out3$test_all[[1]])

chk <- as_test_data_frame(out3$test_all[[3]])
expect_false(identical(
                  chk,
                  out3$test_all[[3]])
              )
expect_s3_class(chk[[1]]$test_results, "data.frame")

tmp <- as_test_data_frame_all_tests(out3)
print(out2$test_all, test_long = TRUE)
print(out3$test_all, test_long = TRUE)
print(tmp, test_long = TRUE)

tmp2 <- collapse_all_tests(out3)

names(attributes(out3$test_all[[2]]))

summarize_one_test_data_frame(tmp2,
                              collapse = "all_sig")
summarize_one_test_data_frame(out3$test_all[[2]])
summarize_one_test_data_frame(out3$test_all[[2]],
                              collapse = "all_sig")

tmp <- summarize_tests(out3,
                       merge_all_tests = TRUE)
expect_output(print(tmp),
              "merged")
expect_true(length(tmp) == 1)

tmp <- summarize_tests(out3,
                       collapse = "all_sig",
                       merge_all_tests = TRUE)
expect_true(nrow(tmp[[1]]$mean) == 1)

expect_output(print(tmp),
              "merged")

tmp <- rejection_rates(out3,
                merge_all_tests = TRUE)

expect_output(print(tmp),
              "merged")

})
