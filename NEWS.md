# power4mome 0.1.1.42

- Improve the function for extending the
  initial interval before doing a
  bisection search. (0.1.1.1)

- Changed the default method for
  rejection rate confidence intervals
  to Wilson's (1927) method.
  For backward compatibility, use
  `options(power4mome.ci_method = "norm")`
  to set the default method to
  normal approximation. (0.1.1.2)

- Added the `test_method` argument for
  tests of indirect effects and their
  variants to use asymmetric *p*-values
  to do the tests. (0.1.1.3)

- Updated test functions that used
  `manymome` to store the number of
  bootstrap or Monte Carlo samples
  and the number of estimates less than
  zero. (0.1.1.4)

- Updated `summarize_tests()` and
  `rejection_rates()` to use the
  extrapolation method by Boos and
  Zhang (2000) if the number of
  resamples for bootstrapping or
  Monte Carlo is of the supported
  values. (0.1.1.5, 0.1.1.6, 0.1.1.7)

- Improved `rejection_rates_by_n()`
  and `rejection_rates_by_es()` to handle
  attempts with different number of
  columns (due to the new Boos-Zhang
  methods). (0.1.1.8)

- Boos-Zhang-2000 method is disabled
  by default. Enable it by setting
  the option `power4mome.bz` to `TRUE`.
  (0.1.1.9)

- `x_from_power()` now detects whether
  a test has more than one result (e.g.,
  testing two parameters but `omnibus`
  is `"none"`). If yes, it will throw
  an error. (0.1.1.10)

- Added two levels of effects, `sm`
  for small-to-moderate, and `ml` for
  moderate-to-large. (0.1.1.11)

- Updated `extend_interval()` to handle
  intervals with nearly equal function
  values. (0.1.1.12)

- Improved `x_from_power()` and friends
  (e.g., `n_from_power()` and
  `n_region_from_power()`) to make use
  of previous trials. (0.1.1.13)

- Optimized the search by bisection, to
  make use of value already tried and
  store all values tried. (0.1.1.14)

- Fixed duplicated values of x when
  extending the range. (0.1.1.15)

- Functions that print a call will
  replace `object` with `<hidden>`
  if it is not a symbol. (0.1.1.16)

- Functions that print a call will
  replace the function with the
  original function name
  if it is not a symbol. (0.1.1.17)

- Added `q_power_mediation()` and
  friends for common mediation models.
  (0.1.1.18, 0.1.1.19)

- The arguments `final_nrep` and
  `final_R` of `x_from_power()` and
  its wrappers will use stored values
  if available. (0.1.1.20)

- The bisection algorithm has been
  improved in handling unusual intervals.
  (0.1.1.21)

- Disable the check for the number of
  elements in `number_of_indicators`
  and `reliability` in the `q_power_mediation_*()`
  functions. (0.1.1.22)

- Skip the check for combining objects
  in the bisection algorithm because
  they must be identical in the model.
  (0.1.1.23)

- Revised `c.power4test_by_n()` to
  allow for minor differences in error
  variances when they are determined by
  Monte Carlo simulation. (0.1.1.24)

- Properly support a model with only one
  latent variable. (0.1.1.25)

- Vertically displace the labels of
  sample sizes in `plot.n_region_from_power()`
  to prevent overlapping. (0.1.1.26)

- Fixed the printing of effects in a
  multigroup model with within-group
  moderation. (0.1.1.27)

- Added `merge_all_tests` to
  `rejection_rates()` to support
  merging all tests into one. The argument
  `collapse` can then be used for collapse
  several different tests, not just for
  one test with several results.
  (0.1.1.28)

- The function `power4test()` now properly
  reuse arguments such as `parallel`
  and `ncores` when adding a new test
  to a `power4test` object.
  (0.1.1.29)

- Updated all test functions to include
  *p*-values in the output.
  (0.1.1.30)

- Added the `p_adjust_method` argument
  to some tests, as well as the
  `rejection_rates` method and
  `summarize_tests()`. Users can adjust
  *p*-values using `p.adjust()` when
  there are more than one test in a
  test function set to `test_fun`,
  or when merging several tests
  in `summarize_tests()`. This feature
  is used to estimate power when
  multiple-comparison adjustment is
  used, such as false discover rate (FDR)
  or Bonferroni correction.
  (0.1.1.31)

- Updated all tests to disable printout
  when running in a test context.
  (0.1.1.32)

- Added one model to `power_curve()`.
  (0.1.1.34)

- Modified the power curve algorithm
  to support `goal = "close_enough"`
  and all three types of `what`
  (`"point"`, `"lb"`, and `"ub"`).
  (0.1.1.34)

- Fixed a bug in extending intervals
  in the bisection algorithm, and also
  improved way intervals are extended.
  (0.1.1.35)

- Add `nls_options` to `power_curve()`
  to configure the use of `nls()`,
  such as when it should not be
  attempted. (0.1.1.36)

- Updated the bisection algorithm to
  use `power_curve()` to assist finding
  the solution. If estimated solution
  inside an interval, use it instead of
  the mean. (0.1.1.37)

- More checks for the solution in
  the bisection algorithm. When extending
  an interval, the power curve will also
  be used. (0.1.1.38)

- Added an argument `rejection_rates_args`
  to `power4test()`. When calling
  `power4test()`, users
  can in advance some settings for
  rejection rates, such as collapsing
  all tests into one. They will be used
  when calling `rejection_rates()`. They
  will also be stored internally, and used
  by `power4test_by_n()`, `x_from_power()`,
  and similar functions that used a
  `power4test` object as an input.
  (0.1.1.39)

- Updated `x_from_power()` and related
  functions to allow users specifying
  how tests will be collapsed (`"none"`
  is not allowed), by setting the
  argument `rejection_rates_args`.
  (0.1.1.39, 0.1.1.41)

- Change the default of `test_long`
  to `TRUE` for the `print` method
  of `power4test` and related objects.
  (0.1.1.41)

- Updated `rejection_rates()` to ignore
  `merge_all_tests` if there is only one
  test. (0.1.1.42)

# power4mome 0.1.1

- Updated to be compatible with the
  forthcoming version of `lavaan`,
  0.9-12. (0.1.1)

# power4mome 0.1.0

- First public version. (0.1.0)
