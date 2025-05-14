# power4mome 0.0.1.18

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
  with models fitted by `lm()`.
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

- Some test functions can support
  models fitted by `lmhelprs::many_lm()`.
  (0.0.0.9024)

- Simplified the argument `fit_function`
 of `fit_model()`. They can now be
 strings, such as `lavaan` and `lm`.
 (0.0.0.9025)

- Added `summarize_tests()`, along
  with `print` method, to summarize
  te test results in a `power4test`
  object. (0.0.0.9026)

- Added a print method for `power4test`
  objects. (0.0.0.9027)

- `sim_out` can now be used to update
  a `sim_out` object by adding elements
  to `extra`. (0.0.0.9028)

- `power4test()` can updata a
  `power4test` object using only a
  new `nrep`. (0.0.0.9029)

- Added `get_rejection_rates()` to
  extract rejection rates from all
  stored tests. (0.0.0.9030)

- Added `power4test_by_n()` and
  `power4test_by_pop_es()` for
  finding the power for a range of
  sample sizes or effect sizes.
  (0.0.0.9031)

- Update the help pages and the examples.
  (0.0.0.9032)

- Minor fixes on the `boot_ci` and
  `mc_ci` arguments of `test_indirect_effect()`
  and `test_index_of_mome()`.
  (0.0.0.9033)

- Revised `fit_model()` such that it
  can be used to refit a model on a new
  set of data. (0.0.0.9034)

- Revised `power4test()` to support
  fitting more than one model in each
  datasets. The `test_` functions have
  also been updated to allow users
  specifying which model will be uesd
  in doing the tests.
  (0.0.0.9035)

- Drafted the `pkgdown` site.
  (0.0.0.9036)

- Added `pop_es_yaml()` and revised
  `ptable_pop()` to support this
  method to specify population values.
  (0.0.0.9037)

- Added the option to disable the
  computation of implied statistics.
  Default to `FALSE` becasue it is not
  required for testing the unstandardized
  effects. Require `manymome` 0.2.7.1
  or above. (0.0.0.9038)

- Finalized for internal testing.
  (0.0.1)

- Fixed a bug in using `x_fun`.
  (0.0.1.1)

- Added a temporary check for package
  versions. (0.0.1.2)

- Fixed the issue with printing
  `boot_out` and `mc_out` objects
  by `print.sim_data()`. (0.0.1.3)

- Revised `get_rejection_rates_*`
  functions to add the option to return all columns
  stored by a test.
  (0.0.1.4)

- Added version requirements for
  `lmhelprs` and `manymome`. (0.0.1.5)

- Added `test_cond_indirect()` and
  `test_cond_indirect_effects()`.
  (0.0.1.7)

- Fixed some issues with the column
  names. (0.0.1.8)

- Added `es1` and `es2` to `power4test()`,
  to allow users to change the effect
  sizes for the labels. (0.0.1.9)

- Added a `c()` method to the output
  of `power4test_by_n()`.
  (0.0.1.10)

- Added a `c()` method to the output
  of `power4test_by_pop_es()`.
  (0.0.1.11)

- Added `by_seed` to `power4test_by_n()`
  and `power4test_by_pop_es()` for
  reproducible results. (0.0.1.12)

- Results are now reproducible is
  `parallel` is `TRUE`. (0.0.1.13)

- Updated `power4test_by_pop_es()` and
  `power4test_by_n()` to accept the
  output of these two functions.
  (0.0.1.14)

- Added `by_nrep` to `power4test_by_n()`.
  (0.0.1.15)

- Added more columns to the results
  of functions returning rejection
  rates. (0.0.1.16)

- Added `n_from_power()` to search
  the sample size with the target
  power. (0.0.1.17)

- Updated `psi_std()` to skip checking
  the variances of product terms.
  (0.0.1.18)