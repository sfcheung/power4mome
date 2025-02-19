# power4mome 0.0.0.9024

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

- Added tests of `power4test()`.
  (0.0.0.9015)

- Removed redundant arguments in
  tests of `power4test()`.
  (0.0.0.9016)

- Updated `power4test()` to store more
  than one test. (0.0.0.9017)

- Updated `fit_model()` to support
  fitting a model by another function
  (0.0.0.9018)

- Added the argument `x_fun()` to
  `sim_data()` for generating nonnormal
  exogenous variables.
  (0.0.0.9019)

- Updated `power4test()` and related
  functions to support updating
  a `power4test` object with different
  population effect sizes or sample size(s).
  (0.0.0.9020)

- Added the option to generate
  nonparametric bootstrap estimates.
  Slow for `lavaan` but can be used
  with models fittedc by `lm()`.
  (0.0.0.9021)

- Added `test_indirect_effect()` for
  testing an indirect effect when
  calling `power4test()`.
  (0.0.0.9022)

- Added `test_index_mome()` for testing
  a moderated mediation effect (by
  testing the index of moderated
  mediation) when calling `power4test()`.
  (0.0.0.9023)

- Added `test_parameters()`
  for testing all free parameters
  when calling `power4test()`.
  (0.0.0.9024)

- Added `test_moderation()` for testing
  all product terms when calling
  `power4test()`.
  (0.0.0.9024)

- Some test functions can suppport
  models fitted by `lmhelprs::many_lm()`.
  (0.0.0.9024)

- Simplified the argument `fit_function`
 of `fit_model()`. They can now be
 strings, such as `lavaan` and `lm`.
 (0.0.0.9025)