# power4mome 0.0.1.96

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

- Improved `rejection_rates_add_ci()` to
  be usable by other functions.
  (0.0.1.19)

- Updated `get_rejection_rates()` to
  report confidence intervals and
  standard errors of rejection rates.
  (0.0.1.20)

- Revised `get_rejection_rates()` and
  related functions. The column of
  rejection rates is now always named
  `"reject"`. (0.0.1.21)

- Added more `ptable_pop()` arguments
  to `power4test()`. (0.0.1.22)

- Added `power_curve()` to estimate the
  relation between rejection rates and
  a predictor. Updated `n_from_power()`
  to use `power_curve()` internally.
  (0.0.1.23)

- The `print` method of `sim_out` objects
  can print the standardized and/or
  unstandardized estimates when fitting
  a model to the merged data.
  (0.0.1.24)

- Renamed `power4test_by_pop_es()` to
  `power4test_by_es()`, because `"n"`
  and `"es"` (not `"pop_es"`) are used
  in other functions to distinguish the
  predictor used. (0.0.1.25)

- Renamed `get_rejection_rates_by_pop_es()`
  to `get_rejection_rates_by_es()`, to
  be consistent with the name
  `power4test_by_es()`. (0.0.1.26)

- Replaced `n_from_power()` by the
  more general version `x_from_power()`,
  and updated related functions.
  (0.0.1.27)

- Updated `x_from_power()` to fully
  support `x = "es"`.
  (0.0.1.28)

- Minor changes to the printout of
  progress for `power4test_by_n()`
  and `power4test_by_es()`.
  (0.0.1.29)

- Fixed a minor bug in the internal
  function `fix_nls_args()`.
  (0.0.1.30)

- Renamed `get_rejection_rates*` to
  `rejection_rates*` because this
  looks more consistent with
  the common naming convention.
  (0.0.1.31)

- Convert `rejection_rates()` to an
  S3 generic method.
  (0.0.1.32)

- Add `rejection_rates` methods for
  `power4test_by_n` and `power4test_by_es`
  objects. The original functions
  `rejection_rates_by_n()` and
  `rejection_rates_by_es()` are no longer
  exported. All relevant functions are
  revised to use the `rejection_rates` method
  instead of these two functions.
  (0.0.1.33)

- Created a class for the output of
  `rejection_rates` and added a print
  method. (0.0.1.34)

- Updated the arguments of the print
  method for `rejection_rate()` output
  of `power4test_by_es` and
  `power4test_by_es` objects.
  (0.0.1.35)

- Removed `power_curve_by_n()` and
  `power_curve_by_es()`. No longer
  needed. (0.0.1.36)

- Updated `power_by_n()` and
  `power_by_es()` to have the option not
  to save the data and model in the
  `sim_all`, and updated the `c()`
  method to skip testing the model.
  This option is used by `x_from_power()`.
  (0.0.1.37)

- Updated `x_from_power()` not to save
  the data and model by default.
  Dramatically reduce the size of the
  output. (0.0.1.37)

- Updated the doc of `do_test()`.
  (0.0.1.38)

- Updated the doc of `test_parameters()`
  on valid parameter names. Also added
  a helper `find_par_names()` for finding
  valid names in a `power4test` object.
  (0.0.1.39)

- Updated all help pages. (0.0.1.40)

- Added `initial_R` and `initial_nrep`
  to `x_from_power()`. (0.0.1.41)

- Changed the default value of `final_nrep`
  in `x_from_power()` to 400. (0.0.1.41)

- Updated `x_from_power()` to use
  the outputs of `power4test_by_n()`
  and `power4test_by_es()`.
  (0.0.1.42)

- Updated `x_from_power()` to reuse
  the outputs of `x_from_power()`.
  (0.0.1.43)

- Fixed some issues related to R CMD Check.
  (0.0.1.44)

- Fixed a typo in the help page
  of `power4test()`. (0.0.1.45)

- Fixed a bug in handling `.cov.` in
  `pop_es`. (0.0.1.46)

- Added the tag `.ind.` for setting the
  values of all component paths along
  an indirect path. This tag is also
  supported by `power4test_by_es()` and
  `x_from_power()`.
  (0.0.1.47)

- The `print` method of `sim_data` will
  print population indirect effect(s),
  if any. (0.0.1.48)

- Corrected the value for `mi`. (0.0.1.49)

- Improved the search algorithm. (0.0.1.49)

- Clarified that the `x_fun` argument can
  also specify the function to generate
  the error terms. (0.0.1.50)

- Added `e_fun` for generating nonnormal
  error terms for indicators. (0.0.1.51)

- Added `gen_missing` for generating
  missing data. (0.0.1.52)

- Adopted a more robust way to process
  `fit_model_args` in `power4test()`.
  (0.0.1.53)

- Fixed the bug that `pop_es` with all
  numeric cannot be processed.
  (0.0.1.54)

- Cleared obsolete TODOs in comments.
  (0.0.1.55)

- The argument `map_names` now work
  properly. (0.0.1.56)

- Rename `gen_missing` to `process_data`.
  (0.0.1.57)

- Added support for printing missing
  data pattern. (0.0.1.58)

- Improved the search algorithm of
  `x_from_power()`. (0.0.1.59)

- Update `print.sim_data()` to compute
  indirect paths only for "pure x-variables
  and "pure y-variables", by default.
  (0.0.1.60)

- Added the bisection method to
  `x_from_power()` and set it as the
  default algorithm. (0.0.1.61)

- Added `check_valid_es_values()` to
  identify the range of valid values
  for a parameter. (0.0.1.62)

- The default algorithm of `x_from_power()`
  now depends on `x`. (0.0.1.63)

- Updated `x_from_power()` to determine
  the interval for `es` automatically.
  (0.0.1.64)

- Optimized the power curve algorithm.
  (0.0.1.65)

- Added `as.power4test_by_n()` and
  `as.power4test_by_es()`.
  (0.0.1.66)

- Updated `x_from_power()` to quit
  early if solution is already found
  in the input.
  (0.0.1.67)

- Updated `x_from_power()` and related
  functions to store more information.
  (0.0.1.68)

- Updated methods of `x_from_power()`
  for `goal` and `what`.
  (0.0.1.69)

- Disabled `sig_area` if `x = "es"`.
  (0.0.1.70)

- Updated `power4test_by_n()` and
  `power4test_by_es()` to catch error
  in `power4test()` and skip a value.
  (0.0.1.71)

- Added error catching to some test
  functions. (0.0.1.72)

- Some test functions will now check
  whether a model fit passed the
  `post.check` test by `lavaan`. (0.0.1.73)

- Updated to allow using expressions
  (e.g., `"s * 2"`, `"sqrt(.30)"`)
  to specify the population value of
  a parameter. (0.0.1.74)

- The `print` method of `rejection_rates()`
  output now always print the column of
  estimates. (0.0.1.75)

- Added a `print` method for the output
  of `power4test_by_n()`. (0.0.1.76)

- Added a `print` method for the output
  of `power4test_by_es()`. (0.0.1.77)

- Added `plot` methods for the output
  of `power4test_by_n()` and
  `power4test_by_es()`. (0.0.1.78)

- Added (back) `n_from_power()`, a
  wrapper of `x_from_power()` with
  `x` set to `"n"`. (0.0.1.79, 0.0.1.80)

- Updated `sim_data()` and `fit_model()`
  to try finding all indirect paths and
  store them in the output of `fit_model_i()`.
  (0.0.1.82)

- Added `test_k_indirect_effects()` to
  test two or more paths. (0.0.1.83, 0.0.1.84, 0.0.1.86)

- Added `n_region_from_power()` and
  methods for its output.
  (0.0.1.85)

- Improved the positioning of elements
  in `plot.n_region_from_power()`.
  (0.0.1.87)

- Improved the speed in computing indirect
  paths in the printout. (0.0.1.88)

- Disabled model checking in
  the plot method of `n_region_from_power`,
  which is not necessary. (0.0.1.89)

- Removed duplicated plot methods.
  (0.0.1.90)

- Updated `test_k_indirect_effects()`'
  with the option `"at_least_k_sig"`.
  (0.0.1.91)

- Added `collapse` and `at_least_k` to
  `rejection_rates()` and related
  functions. (0.0.1.92)

- Added `omnibus` to
  `test_parameters()`. (0.0.1.93)

- The argument `fit_to_all_args` is now
  correctly passed to the print method
  of `sim_data`. (0.0.1.94)

- Fixed a bug with `process_data`. The
  input is now always a data frame.
  (0.0.1.95)

- Fixed an issue with interaction terms
  involved in covariances. (0.0.1.96)

- Improved the printing of indirect
  effects. (0.0.1.96)
