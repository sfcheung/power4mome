# power4mome 0.1.1.54

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

- Added a data processor: `scale_scores()`.
  It replaces the indicator scores by
  the corresponding scale scores
  before fitting a model. To be used
  in the `process_data` argument.
  (0.1.1.43)

- Updated `sim_data()`. Lines for
  the indicators will not be added to
  the model syntax if scale scores are
  used. (0.1.1.43)

- The argument `sim_data_name` of
  `process_data` is now default to
  `"data"`. It is not a required
  argument. (0.1.1.43)

- The attribute `number_of_indicators`
  will be added to the generated data
  before passing to `process_data`.
  (0.1.1.43)

- Added `n_ratio` to `power4test()` and
  related functions to supporting
  controlling the sample sizes of
  multigroup models using one single
  value for `n`. This allows functions
  such as `n_from_power()` and
  `n_region_from_power()` to support
  multigroup models.
  (0.1.1.44)

- `test_cond_indirect()` and
  `test_cond_indirect_effects()` now
  support multigroup models, although
  `test_cond_indirect_effects()` only
  support either a path with moderators
  (`wlevels`) or a path between groups,
  but not both.
  (0.1.1.45)

- Updated `test_cond_indirect_effects()`
  to support computing and testing
  group differences in indirect effects
  for multigroup models.
  (0.1.1.46)

- Updated `test_parameters()` to support
  doing likelihood ratio tests to
  test constraining pairs of parameters
  in multigroup models, by using the
  argument `compare_groups`.
  (0.1.1.47)

- Added the argument `exclude_var` to
  `test_parameters()` to exclude
  variances and error variances.
  (0.1.1.47)

- Updated `power4test()` to support
  updating a parameter in one group
  when the population model is a
  multigroup model. Use `"y ~ x.g2"`
  to denote `"y ~ x"` in Group 2.
  If the suffix is omitted, the parameter
  is assumed to be in Group 1.
  The function `power4test_by_es()` now
  also support multigroup models due to
  this change.
  (0.1.1.48)

- Added `test_group_equal()` for testing
  equality constraints between groups.
  (0.1.1.49)

- The arguments in `control` is now
  passed directly to the entry point
  of the algorithms. Potential conflicts
  due to partial matching should be
  prevented inside these algorithms
  (0.1.1.50)

- Added the algorithm
  `"probabilistic_bisection"` for
  `x_from_power()` and friends.
  (0.1.1.51)

- Adjust the `initial_nrep` in
  probabilistic bisection such that
  the rejection rate will not be exactly
  equal to the target power.
  (0.1.1.52)

- Test functions using `manymome` will
  automatically use `"pvalues"` as the
  test method if `R` is a value supported
  by the Boos-Zhang method.
  (0.1.1.53)

- Fixed `fix_many_lm_model()` to handle
  model syntax with a regression model
  spanning more than one line and fitted
  by `lm()`. (0.1.1.54)

# power4mome 0.1.1

- Updated to be compatible with the
  forthcoming version of `lavaan`,
  0.9-12. (0.1.1)

# power4mome 0.1.0

- First public version. (0.1.0)
