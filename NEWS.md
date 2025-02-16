# power4mome 0.0.0.9014

- Initialized the package. (0.0.0.9000)

- Major functions drafted and the main
  workflows are finalized. Ready for
  documenting the changes. (0.0.0.9001)

- Documented how to set `pop_es`.
  (0.0.0.9002)

- Added preliminary support for
  covariance-only models (no regression
  path) and confirmatory factor analytic
  models (CFA). (0.0.0.9003)

- Added support for multigroup models.
  (0.0.0.9004)

- For `sim_data()`, `number_of_indicators`
  and `reliability` can be set to one
  single values. (0.0.0.9005)

- Added a check for negative variances
  or variances greater than one in the
  model. (0.0.0.9006)

- A warning will be raised by `ptable_pop()`
  if `pop_es` has one or more variables
  not in the model. (0.0.0.9007)

- Added error and warning checking
  code to `fit_model()`.
  (0.0.0.9008)

- Added error checking code to `gen_mc()`.
  (0.0.0.9009)

- Added tests for `sim_out()` and
  `do_test()`. (0.0.0.9010)

- Updated the help of `do_test()` on
  the requirement for the results
  and test functions. (0.0.0.9011)

- Updated `fit_model()` to support
  user-supplied models. (0.0.0.9012)

- Updated `do_test()` and `sim_out()`
  for arbitrary number of models
  and arbitrary elements to be used
  in `test_fun()`. (0.0.0.9013)

- Updated `do_test()` and `sim_out()`
  to support tests that compare two
  models. (0.0.0.9014)