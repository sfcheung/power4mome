skip("WIP")

library(testthat)
suppressMessages(library(lavaan))

test_that("summarize tests", {

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

as_test_data_frame_i <- function(
  x,
  test_name
) {
  # Convert a test results vector to a one-row test results data frame
  if ("test_results" %in% names(x)) {
    # x should be the test results
    x <- x$test_results
  }
  if (!is.null(dim(x))) {
    # It is not a vector. Assume it is a data frame and return it as-is.
    # Skip checking x, for efficiency.
    return(x)
  }
  out <- data.frame(
            test_label = test_name,
            rbind(x)
          )
  attr(out, "test_label") <- "test_label"
  out
}

as_test_data_frame_i(out3$test_all[[3]][[1]],
                     test_name = attr(out3$test_all[[3]], "test_name"))
as_test_data_frame_i(out3$test_all[[3]][[1]]$test_results,
                     test_name = attr(out3$test_all[[3]], "test_name"))

as_test_data_frame <- function(
  test_output
) {
  # Convert a list of test vectors to a list of one-row test data frame
  test_name <- attr(
                  test_output,
                  "test_name"
                )
  for (i in seq_along(test_output)) {
    test_output[[i]]$test_results <-
      as_test_data_frame_i(test_output[[i]]$test_results,
                           test_name = test_name)
  }
  test_output
}

expect_identical(as_test_data_frame(out3$test_all[[1]]),
                 out3$test_all[[1]])

chk <- as_test_data_frame(out3$test_all[[3]])
expect_false(identical(
                  chk,
                  out3$test_all[[3]])
              )
expect_s3_class(chk[[1]]$test_results, "data.frame")

as_test_data_frame_all_tests <- function(
  object
) {
  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  # object: The test_all element of a `power4test` object
  for (i in seq_along(object)) {
    object[[i]] <- as_test_data_frame(object[[i]])
  }
  object
}

tmp <- as_test_data_frame_all_tests(out3)
print(out2$test_all, test_long = TRUE)
print(out3$test_all, test_long = TRUE)
print(tmp, test_long = TRUE)

rbind_diff_cols <- function(x) {
  # Bind rows with different colnames
  test_names <- names(x)
  all_cnames <- lapply(x,
                       \(x) colnames(x))
  all_cnames <- unique(unlist(all_cnames,
                       use.names = FALSE))
  out <- lapply(x,
            \(xx) {
              tmp <- setdiff(all_cnames, colnames(xx))
              if (length(tmp) == 0) {
                return(xx)
              }
              xx[, tmp] <- NA
              xx
            })
  out <- mapply(function(xx, yy) {
                  out <- cbind(test = xx,
                               yy)
                  rownames(out) <- NULL
                  out
                },
                xx = test_names,
                yy = out,
                SIMPLIFY = FALSE)
  do.call(rbind, unname(out))
}

collapse_all_tests <- function(
  object,
  keep = c("est", "cilo", "cihi", "sig")
) {
  # Get test_all
  # Collapse all tests into one test
  # NOTE:
  # test_labels no longer relevant
  # because the column names are
  # not guaranteed to be the same
  # across tests.
  # This functions should be used only
  # with collapse across tests.

  if (inherits(object, "power4test")) {
    object <- object$test_all
  }
  test_all <- as_test_data_frame_all_tests(object)
  nrep <- length(test_all[[1]])

  out <- lapply(seq_len(nrep),
            function(i) {
              test_results_all_i <- lapply(test_all,
                                  \(x) x[[i]]$test_results)
              out1 <- rbind_diff_cols(test_results_all_i)
              out1$test_label <- paste0("test", seq_len(nrow(out1)))
              out1 <- out1[, c("test", "test_label", keep), drop = FALSE]
              out1 <- list(test_results = out1)
              attr(out1$test_results, "test_label") <- "test_label"
              out1
            })
  attr(out, "test_name") <- "All tests merged into one"
  attr(out, "tests") <- names(object)
  out
}

tmp2 <- collapse_all_tests(out3)

names(attributes(out3$test_all[[2]]))

summarize_one_test_data_frame(tmp2,
                              collapse = "all_sig")
summarize_one_test_data_frame(out3$test_all[[2]])
summarize_one_test_data_frame(out3$test_all[[2]],
                              collapse = "all_sig")

})
