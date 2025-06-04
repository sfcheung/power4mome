library(testthat)

test_that("fix_fit_model_args", {
  # Conditions
  # Scenarios:
  # - [DONE] A list with no element.
  #   - list()
  #     - One fit model, and no additional arguments
  # - [DONE] A list with one element, named "fit"
  #   - list(fit = list())
  #   - list(fit = list(missing = "fiml", estimator = "MLR"))
  #     - One fit model, and these are the arguments
  #     - The first name must be "fit"
  # - [DONE] A list with more than one arguments. The first one is not named "fit"
  #   - list(args = list())
  #     - One fit model, and the value of args is a list.
  #   - list(missing = "fiml")
  #     - One fit model, and this is the arguments
  #   - list(missing = "fiml", estimator = "MLR")
  #     - One fit model, and these are the arguments
  # - [DONE] A list with more than one argument. The first one is named "fit"
  #   - list(fit = list(),
  #          fitx = list(missing = "fiml.x", estimator = "ML"))
  #     - Two or more fit models.
  #     - The first name must be "fit"
  #   - list(fit = list(missing = "fiml", estimator = "MLR"),
  #          fitx = list(missing = "fiml.x", estimator = "ML"))
  #     - Two or more fit models.
  #     - The first name must be "fit"
expect_identical(fix_fit_model_args(list()),
                 list(fit = list()))
expect_identical(fix_fit_model_args(list(fit = list())),
                 list(fit = list()))
expect_identical(fix_fit_model_args(list(fit = list(missing = "fiml"))),
                 list(fit = list(missing = "fiml")))
expect_identical(fix_fit_model_args(list(fit = list(missing = "fiml",
                                                    estimator = "MLR"))),
                 list(fit = list(missing = "fiml",
                                 estimator = "MLR")))
expect_identical(fix_fit_model_args(list(fit = list(missing = "fiml",
                                                    tmp = list(1, 2, 3)))),
                 list(fit = list(missing = "fiml",
                                 tmp = list(1, 2, 3))))
expect_identical(fix_fit_model_args(list(fit = list(tmp = list(1, 2, 3)))),
                 list(fit = list(tmp = list(1, 2, 3))))
expect_identical(fix_fit_model_args(list(args = list())),
                 list(fit = list(args = list())))
expect_identical(fix_fit_model_args(list(missing = "fiml")),
                 list(fit = list(missing = "fiml")))
expect_identical(fix_fit_model_args(list(missing = "fiml",
                                         estimator = "MLR")),
                 list(fit = list(missing = "fiml",
                                 estimator = "MLR")))
expect_identical(fix_fit_model_args(list(fit = list(),
                                         fitx = list(missing = "fiml.x",
                                                     estimator = "ML"))),
                 list(fit = list(),
                      fitx = list(missing = "fiml.x",
                                  estimator = "ML")))
expect_identical(fix_fit_model_args(list(fit = list(missing = "fiml"),
                                         fitx = list(missing = "fiml.x",
                                                     estimator = "ML"))),
                 list(fit = list(missing = "fiml"),
                      fitx = list(missing = "fiml.x",
                                  estimator = "ML")))
# This won't throw an error:
# list(fit1 = list(missing = "fiml"),
#      fitx = list(missing = "fiml.x",
#                  estimator = "ML")))
# But leave it this way for now. Users should not do this,
# but there is no robust way to differentiat this from:
# list(arg1 = list(),
#      arg2 = list())
# expect_error(fix_fit_model_args(list(fit1 = list(missing = "fiml"),
#                                      fitx = list(missing = "fiml.x",
#                                                  estimator = "ML"))))
})
