# Changelog

## power4mome 0.2.0

CRAN release: 2026-03-18

### New Features

#### `power4test()`, `x_from_power()`, and Related Functions

- Added an argument `rejection_rates_args` to
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
  When calling
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
  users can in advance some settings for rejection rates, such as
  collapsing all tests into one. They will be used when calling
  [`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md).
  They will also be stored internally, and used by
  [`power4test_by_n()`](https://sfcheung.github.io/power4mome/reference/power4test_by_n.md),
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
  and similar functions that used a `power4test` object as an input.
  (0.1.1.39)

- Updated
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and related functions to allow users specifying how tests will be
  collapsed (`"none"` is not allowed), by setting the argument
  `rejection_rates_args`. (0.1.1.39, 0.1.1.41)

#### “Quick” Functions

To simplify the workflows for some common mediation models, a set of
“quick” functions have been added. Users can do common tasks using just
one function.

- Added
  [`q_power_mediation()`](https://sfcheung.github.io/power4mome/reference/q_power_mediation.md)
  and friends for common mediation models. Users can do most steps in
  one call. (0.1.1.18, 0.1.1.19)

- Updated the quick functions
  ([`q_power_mediation()`](https://sfcheung.github.io/power4mome/reference/q_power_mediation.md)
  and friends) to have one more mode, `"n"`. Probabilistic bisection is
  the default algorithm for this mode. Other methods have been updated
  for this mode. (0.1.1.56)

#### Rejection Rates

- Changed the default method for rejection rate confidence intervals to
  Wilson’s (1927) method. For backward compatibility, use
  `options(power4mome.ci_method = "norm")` to set the default method to
  normal approximation. (0.1.1.2)

- Updated
  [`summarize_tests()`](https://sfcheung.github.io/power4mome/reference/summarize_tests.md)
  and
  [`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md)
  to use the extrapolation method by Boos and Zhang (2000) if the number
  of resamples for bootstrapping or Monte Carlo is of the supported
  values. (0.1.1.5, 0.1.1.6, 0.1.1.7)

- Added `merge_all_tests` to
  [`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md)
  to support merging all tests into one. The argument `collapse` can
  then be used for collapsing several different tests, not just for one
  test with several results. (0.1.1.28)

- Added the `p_adjust_method` argument to some tests, as well as the
  `rejection_rates` method and
  [`summarize_tests()`](https://sfcheung.github.io/power4mome/reference/summarize_tests.md).
  Users can adjust *p*-values using
  [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) when there are
  more than one test in a test function set to `test_fun`, or when
  merging several tests in
  [`summarize_tests()`](https://sfcheung.github.io/power4mome/reference/summarize_tests.md).
  This feature is used to estimate power when multiple-comparison
  adjustment is used, such as false discover rate (FDR) or Bonferroni
  correction. (0.1.1.31)

#### Model Specifications

- Added two levels of effects, `sm` for small-to-moderate, and `ml` for
  moderate-to-large. (0.1.1.11)

- Added `loading_difference` and `reference` for
  [`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md).
  The population standardized factor loadings of indicators of a factor
  can have different loadings, and the first indicator (the default
  reference indicator) can be the indicator with the strongest, weakest,
  or medium loadings. (0.1.1.70)

#### Tests

- Added the `test_method` argument for tests of indirect effects and
  their variants to use asymmetric *p*-values to do the tests. This
  change is for using the efficient method by Boos and Zhang (2000) to
  estimate the rejection rates for resampling methods. (0.1.1.3)

- Updated
  [`test_k_indirect_effects()`](https://sfcheung.github.io/power4mome/reference/test_k_indirect_effects.md)
  to support computing and testing the total indirect effect. (0.1.1.65)

#### Data Processors

When calling the function
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
the generated data can now be further processed by *data processors*:
functions to manipulate the generated data, such as introducing missing
values or computing scale scores. Some built-in processors are included,
but user-supplied functions can also be used.

- Added
  [`scale_scores()`](https://sfcheung.github.io/power4mome/reference/scale_scores.md)
  for replacing the indicator scores by the corresponding scale scores
  before fitting a model. This allows estimating power when the data
  generating model is a latent factor model with indicators but the
  model will be fitted to the scale scores instead of the indicators.
  (0.1.1.43)

- Added
  [`missing_values()`](https://sfcheung.github.io/power4mome/reference/missing_values.md)
  for generating missing values. This allows sample size determination
  that takes into account probable missing data. This also allows
  comparing the levels of power between two different methods to handle
  missing data, such as full information maximum likelihood versus
  listwise deletion. (0.1.1.66)

- Added
  [`ordinal_variables()`](https://sfcheung.github.io/power4mome/reference/ordinal_variables.md)
  for converting continuous indicator variables to ordinal variables.
  This allows estimating power when the indicators are ordinal
  variables, such as binary items or 5-point Likert scales. This also
  allows comparing the levels of power between different numbers of
  response options. (0.1.1.67, 0.1.1.69)

- The argument `sim_data_name` of `process_data` is now default to
  `"data"`. It is not a required argument. (0.1.1.43)

#### Multigroup Models

Support for multigroup models have been substantially improved. Users
can generate data from a multigroup models. Some tests can also be used
for estimating power in detecting group differences, such as the group
difference in an indirect effect.

- Added `n_ratio` to
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  and related functions to supporting controlling the sample sizes of
  multigroup models using one single value for `n`. This allows
  functions such as
  [`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and
  [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  to support multigroup models. (0.1.1.44)

- [`test_cond_indirect()`](https://sfcheung.github.io/power4mome/reference/test_cond_indirect.md)
  and
  [`test_cond_indirect_effects()`](https://sfcheung.github.io/power4mome/reference/test_cond_indirect_effects.md)
  now support multigroup models, although
  [`test_cond_indirect_effects()`](https://sfcheung.github.io/power4mome/reference/test_cond_indirect_effects.md)
  only support either a path with moderators (`wlevels`) or a path
  between groups, but not both. (0.1.1.45)

- Updated
  [`test_cond_indirect_effects()`](https://sfcheung.github.io/power4mome/reference/test_cond_indirect_effects.md)
  to support computing and testing group differences in indirect effects
  for multigroup models. (0.1.1.46)

- Updated
  [`test_parameters()`](https://sfcheung.github.io/power4mome/reference/test_parameters.md)
  to support doing likelihood ratio tests to test constraining pairs of
  parameters in multigroup models, by using the argument
  `compare_groups`. (0.1.1.47)

- Added the argument `exclude_var` to
  [`test_parameters()`](https://sfcheung.github.io/power4mome/reference/test_parameters.md)
  to exclude variances and error variances, useful for multigroup
  models. (0.1.1.47)

- Updated
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  to support updating a parameter in one group when the population model
  is a multigroup model. Use `"y ~ x.g2"` to denote `"y ~ x"` in
  Group 2. If the suffix is omitted, the parameter is assumed to be in
  Group 1. The function
  [`power4test_by_es()`](https://sfcheung.github.io/power4mome/reference/power4test_by_es.md)
  now also support multigroup models due to this change. (0.1.1.48)

- Added
  [`test_group_equal()`](https://sfcheung.github.io/power4mome/reference/test_group_equal.md)
  for testing equality constraints between groups. (0.1.1.49)

- If the model is a multigroup model, descriptive statistics for the
  generated data are printed by group, using
  [`psych::describeBy()`](https://rdrr.io/pkg/psych/man/describe.by.html).
  (0.1.1.76)

#### Probabilistic Bisection

The probabilistic bisection algorithm proposed by Waeber et al. (2013)
has been added. For some scenarios, it allows for a more efficient
search for a sample size with sufficient power, especially when a high
precision is desired (by using a large value of `nrep`).

- Added the algorithm `"probabilistic_bisection"` for
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and friends. (0.1.1.51)

### Miscellaneous

#### Quality-of-Life Changes

- Change the default of `test_long` to `TRUE` for the `print` method of
  `power4test` and related objects. (0.1.1.41)

- For some functions, `nrep` will be included in the output if its
  values vary across rows. (0.1.1.57)

- The default values of `n` of q-functions now depends on the `mode` and
  `algorithm`. (0.1.1.57)

- Added some helpers,
  [`R_for_bz()`](https://sfcheung.github.io/power4mome/reference/bz_helpers.md)
  and
  [`Rs_bz_supported()`](https://sfcheung.github.io/power4mome/reference/bz_helpers.md),
  for users to use the Boos-Zhang-2000 method without remembering the
  number of resamples supported. (0.1.1.61)

- Updated the quick functions
  ([`q_power_mediation()`](https://sfcheung.github.io/power4mome/reference/q_power_mediation.md)
  and friends). When `mode` is `"n"` or `"region"`, it is optional to
  set `n`. If not set, it will be determined internally. (0.1.1.63)

- Added the
  [`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md)
  method to `x_from_power`, `n_region_from_power`, and
  `q_power_mediation` objects. (0.1.1.71)

- Moved `delta_tol` and `last_k` to `variants`, such that users can
  change them through `control` when calling
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and friends. (0.1.1.72)

- Changed the default algorithms to be used in
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).
  (0.1.1.74)

- Response proportions will be printed with `data_long = TRUE` if a
  variable has 10 or fewer unique values (e.g., ordinal variables),
  using
  [`psych::responseFrequency()`](https://rdrr.io/pkg/psych/man/score.items.html).
  (0.1.1.77)

#### Bisection

- Improved the function for extending the initial interval before doing
  a bisection search. (0.1.1.1)

- Optimized the search by bisection, to make use of value already tried
  and store all values tried. (0.1.1.14)

- The bisection algorithm has been improved in handling unusual
  intervals. (0.1.1.21)

- Skipped the check for combining objects in the bisection algorithm
  because they must be identical in the model. (0.1.1.23)

- Updated the bisection algorithm to use
  [`power_curve()`](https://sfcheung.github.io/power4mome/reference/power_curve.md)
  to assist finding the solution. If estimated solution inside an
  interval, use it instead of the mean. (0.1.1.37)

- More checks for the solution in the bisection algorithm. When
  extending an interval, the power curve will also be used. (0.1.1.38)

#### Power Curve

- Modified the power curve algorithm to support `goal = "close_enough"`
  and all three types of `what` (`"point"`, `"lb"`, and `"ub"`).
  (0.1.1.34)

#### Probabilistic Bisection

- Adjust the `initial_nrep` in probabilistic bisection such that the
  rejection rate will not be exactly equal to the target power.
  (0.1.1.52)

- For probabilistic bisection, the initial interval will no longer be
  adjusted. This algorithm should be used with a wide enough initial
  interval because the interval will not be adjusted during the search
  (for now). (0.1.1.56)

- Increased `delta_tol` fo PBA (2 for `n` and .002 for `es`). (0.1.1.57)

- Added
  [`pba_diagnosis()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  to generate plots related to the search history of probabilistic
  bisection algorithm. For advanced users and developers. (0.1.1.58,
  0.1.1.62)

- Updated the help page of
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  to describe the probabilistic bisection method. (0.1.1.60)

#### Tests

- Updated test functions that used `manymome` to store the number of
  bootstrap or Monte Carlo samples and the number of estimates less than
  zero. Used by the Boos-Zhang-2000 method. (0.1.1.4)

- Updated all test functions to include *p*-values in the output.
  (0.1.1.30)

- Test functions using `manymome` will now automatically use `"pvalues"`
  as the test method if `R` is a value supported by the
  Boos-Zhang-method. (0.1.1.53)

#### Others

- Improved
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and friends (e.g.,
  [`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and
  [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md))
  to make use of previous trials. (0.1.1.13)

- [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  now detects whether a test has more than one result (e.g., testing two
  parameters but `omnibus` is `"none"`). If yes, it will throw an error.
  (0.1.1.10)

- Updated `extend_interval()` to handle intervals with nearly equal
  function values. (0.1.1.12)

- Fixed duplicated values of x when extending the interval. (0.1.1.15)

- Functions that print a call will replace `object` with `<hidden>` if
  it is not a symbol. (0.1.1.16)

- Functions that print a call will replace the function with the
  original function name if it is not a symbol. (0.1.1.17)

- The arguments `final_nrep` and `final_R` of
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and its wrappers will use stored values if available. (0.1.1.20)

- Revised
  [`c.power4test_by_n()`](https://sfcheung.github.io/power4mome/reference/power4test_by_n.md)
  to allow for minor differences in error variances when they are
  determined by Monte Carlo simulation. (0.1.1.24)

- Properly support a model with only one latent variable. (0.1.1.25)

- Vertically displace the labels of sample sizes in
  [`plot.n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/plot.x_from_power.md)
  to prevent overlapping. (0.1.1.26)

- Fixed the printing of effects in a multigroup model with within-group
  moderation. (0.1.1.27)

- The function
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  now properly reuse arguments such as `parallel` and `ncores` when
  adding a new test to a `power4test` object. (0.1.1.29)

- Added one model to
  [`power_curve()`](https://sfcheung.github.io/power4mome/reference/power_curve.md).
  (0.1.1.34)

- Added `nls_options` to
  [`power_curve()`](https://sfcheung.github.io/power4mome/reference/power_curve.md)
  to configure the use of [`nls()`](https://rdrr.io/r/stats/nls.html),
  such as when it should not be attempted. (0.1.1.36)

- Updated
  [`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md).
  Lines for the indicators will not be added to the model syntax if
  scale scores are used. (0.1.1.43)

- The attribute `number_of_indicators` will be added to the generated
  data before passing to `process_data`. Used by data processors such as
  [`scale_scores()`](https://sfcheung.github.io/power4mome/reference/scale_scores.md).
  (0.1.1.43)

- The arguments in `control` are now passed directly to the entry point
  of the algorithms. Potential conflicts due to partial matching should
  be prevented inside these algorithms (0.1.1.50)

- Updated
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  to make one cluster that will be used in all stages. (0.1.1.55)

- Load balancing is no longer used by default, to ensure the results are
  reproducible. To enable load balancing, set the option
  `"power4mome.use_lb"` to `TRUE` using
  [`options()`](https://rdrr.io/r/base/options.html). (0.1.1.56)

- Updated the internal functions `summarize_one_test_vector ()` and
  `summarize_one_test_data_frame()` to handle failed replications
  properly. (0.1.1.59)

- The internal helper `do_FUN()` for parallel processing now export
  functions defined in the global environment to clusters, because they
  may be used in arguments such as `process_data`. (0.1.1.64)

- The internal helper `do_FUN()` will reproduce the search path by
  loading the packages in the workers. (0.1.1.69)

- Added and updated technical appendices for the three algorithms used
  in
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and friends. (0.1.1.73)

- Fixed the help for the argument `mode` of
  [`q_power_mediation()`](https://sfcheung.github.io/power4mome/reference/q_power_mediation.md)
  and friends. (0.1.1.75)

- Simplify some examples to meet the five-second requirement. (0.1.1.78)

### Bug Fixes

- Fixed a bug in extending intervals in the bisection algorithm, and
  also improved the way intervals are extended. (0.1.1.35)

- Fixed `fix_many_lm_model()` to handle model syntax with a regression
  model spanning more than one line and fitted by
  [`lm()`](https://rdrr.io/r/stats/lm.html). (0.1.1.54)

## power4mome 0.1.1

CRAN release: 2025-09-21

- Updated to be compatible with the forthcoming version of `lavaan`,
  0.9-12. (0.1.1)

## power4mome 0.1.0

CRAN release: 2025-09-04

- First public version. (0.1.0)
