# Estimate the Power of a Test

An all-in-one function that receives a model specification, generates
datasets, fits a model, does the target test, and returns the test
results.

## Usage

``` r
power4test(
  object = NULL,
  nrep = NULL,
  ptable = NULL,
  model = NULL,
  pop_es = NULL,
  standardized = TRUE,
  n = NULL,
  number_of_indicators = NULL,
  reliability = NULL,
  x_fun = list(),
  e_fun = list(),
  process_data = NULL,
  fit_model_args = list(),
  R = NULL,
  ci_type = "mc",
  gen_mc_args = list(),
  gen_boot_args = list(),
  test_fun = NULL,
  test_args = list(),
  map_names = c(fit = "fit"),
  results_fun = NULL,
  results_args = list(),
  test_name = NULL,
  test_note = NULL,
  do_the_test = TRUE,
  sim_all = NULL,
  iseed = NULL,
  parallel = FALSE,
  progress = TRUE,
  ncores = max(1, parallel::detectCores(logical = FALSE) - 1),
  es1 = c(n = 0, nil = 0, s = 0.1, m = 0.3, l = 0.5, si = 0.141, mi = 0.361, li = 0.51,
    sm = 0.2, ml = 0.4),
  es2 = c(n = 0, nil = 0, s = 0.05, m = 0.1, l = 0.15, sm = 0.075, ml = 0.125),
  es_ind = c("si", "mi", "li"),
  n_std = 1e+05,
  std_force_monte_carlo = FALSE
)

# S3 method for class 'power4test'
print(
  x,
  what = c("data", "test"),
  digits = 3,
  digits_descriptive = 2,
  data_long = FALSE,
  test_long = FALSE,
  fit_to_all_args = list(),
  ...
)
```

## Arguments

- object:

  Optional. If set to a `power4test` object, it will be updated using
  the value(s) in `n`, `pop_es`, and/or `nrep` if they changed, or a new
  test will be conducted and added to `objet`. See the help page for
  details. Default is `NULL`.

- nrep:

  The number of replications to generate the simulated datasets. Default
  is `NULL`. Must be set when called to create a `power4test` object.

- ptable:

  The output of
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md),
  which is a `ptable_pop` object, representing the population model. If
  `NULL`, the default,
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md)
  will be called to generate the `ptable_pop` object using `model` and
  `pop_es`.

- model:

  The `lavaan` model syntax of the population model, to be used by
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md).
  See 'Details' of on how to specify the model. Ignored if `ptable` is
  specified.

- pop_es:

  The character vector or multiline string to specify population effect
  sizes (population values of parameters). See the help page on how to
  specify this argument. Ignored if `ptable` is specified.

- standardized:

  Logical. If `TRUE`, the default, variances and error variances are
  scaled to ensure the population variances of the endogenous variables
  are close to one, and hence the effect sizes (population values) are
  standardized effect sizes if the variances of the continuous exogenous
  variables are also equal to one.

- n:

  The sample size for each dataset. Default is 100.

- number_of_indicators:

  A named vector to specify the number of indicators for each factors.
  See the help page on how to set this argument. Default is `NULL` and
  all variables in the model syntax are observed variables. See the help
  page on how to use this argument.

- reliability:

  A named vector (for a single-group model) or a named list of named
  vectors (for a multigroup model) to set the reliability coefficient of
  each set of indicators. Default is `NULL`. See the help page on how to
  use this argument.

- x_fun:

  The function(s) used to generate the exogenous variables or error
  terms. If not supplied, or set to
  [`list()`](https://rdrr.io/r/base/list.html), the default, the
  variables are generated from a multivariate normal distribution. See
  the help page on how to use this argument.

- e_fun:

  The function(s) used to generate the error terms of indicators, if
  any. If not supplied, or set to
  [`list()`](https://rdrr.io/r/base/list.html), the default, the error
  terms of indicators are generated from a multivariate normal
  distribution. Specify in the same way as `x_fun`. Refer to the help
  page on `x_fun` on how to use this argument.

- process_data:

  If not `NULL`, it must be a named list with these elements: `fun`
  (required), the function to further processing the simulated data,
  such as generating missing data using functions such as
  `mice::ampute()`; `args` (optional), a named list of arguments to be
  passed to `fun`, except the one for the source data; `sim_data_name`
  (required) the name of the argument to receive the simulated data
  (e.g., `data` for `mice::ampute()`); `processed_data_name` (optional),
  the name of the data frame after being processed by `fun`, such as the
  data frame with missing data in the output of `fun` (e.g., `"amp"` for
  `mice::ampute()`), if omitted, the output of `fun` should be the data
  frame with missing data.

- fit_model_args:

  A list of the arguments to be passed to
  [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
  when fitting the model. Should be a named list with names being the
  names of the arguments.

- R:

  The number of replications to generate the Monte Carlo or
  bootstrapping estimates for each fit output. No Monte Carlo nor
  bootstrapping estimates will be generated if `R` is set to `NULL`.

- ci_type:

  The type of simulation-based confidence intervals to use. Can be
  either `"mc"` for Monte Carlo method (the default) or `"boot"` for
  nonparametric bootstrapping method. Relevant for test functions that
  make use of estimates generate by
  [`gen_boot()`](https://sfcheung.github.io/power4mome/reference/gen_boot.md)
  or
  [`gen_mc()`](https://sfcheung.github.io/power4mome/reference/gen_mc.md),
  such as
  [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md).

- gen_mc_args:

  A list of arguments to be passed to
  [`manymome::do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.html)
  when generating the Monte Carlo estimates. Should be a named argument
  with names being the names of the arguments. Used only if `ci_type` is
  `"mc".`

- gen_boot_args:

  A list of arguments to be passed to
  [`manymome::do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.html)
  when generating the bootstrap estimates. Should be a named argument
  with names being the names of the arguments. Used only if `ci_type` is
  \`"boot".

- test_fun:

  A function to do the test. See 'Details' for the requirement of this
  function. There are some built-in test functions in `power4mome`,
  described in 'Details'.

- test_args:

  A list of arguments to be passed to the `test_fun` function. Default
  is [`list()`](https://rdrr.io/r/base/list.html).

- map_names:

  A named character vector specifying how the content of the element
  `extra` in each replication of `sim_all` map to the argument of
  `test_fun`. Default is `c(fit = "fit")`, indicating that the element
  `fit` in the element `extra` is set to the argument `fit` of
  `test_fun`. That is, for the first replication,
  `fit = sim_out[[1]]$extra$fit` when calling `test_fun`.

- results_fun:

  The function to be used to extract the test results. See `Details` for
  the requirements of this function. Default is `NULL`, assuming that
  the output of `test_fun` can be used directly.

- results_args:

  A list of arguments to be passed to the `results_fun` function.
  Default is [`list()`](https://rdrr.io/r/base/list.html).

- test_name:

  String. The name of the test. Default is `NULL`, and the name will be
  created from `test_fun`. Note that if `sim_out` is a `power4test`
  object and already has a test of this name stored, it will be replaced
  by the new results.

- test_note:

  String. An optional note for the test, stored in the attribute
  `test_note` of the output of
  [`do_test()`](https://sfcheung.github.io/power4mome/reference/do_test.md).
  Default is `NULL`.

- do_the_test:

  If `TRUE`,
  [`do_test()`](https://sfcheung.github.io/power4mome/reference/do_test.md)
  will be called to do the test specified by `test_fun` on the fit
  output of each dataset.

- sim_all:

  If set to either a `sim_out` object (the output of
  [`sim_out()`](https://sfcheung.github.io/power4mome/reference/sim_out.md)
  or a `power4test` object (the output of `power4test()`), the stored
  datasets and fit outputs will be used for doing the test. Setting
  `object` to the output of `power4test()` is now the preferred method,
  but this argument is kept for backward compatibility.

- iseed:

  The seed for the random number generator. Default is `NULL` and the
  seed is not changed. This seed will be set only once, when calling
  [`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md).

- parallel:

  If `TRUE`, parallel processing will be used when calling other
  functions, if appropriate. Default is `FALSE`.

- progress:

  If `TRUE`, the progress of each step will be displayed. Default is
  `TRUE`.

- ncores:

  The number of CPU cores to use if parallel processing is used.

- es1:

  Set the values for each label of the effect size (population value)
  for correlations and regression paths. Used only if `pop_es` is a
  named vector or a multiline string. See the help page on how to
  specify this argument.

- es2:

  Set the values for each label of the effect size (population value)
  for product term. Used only if `pop_es` is a named vector or a
  multiline string. See the help page on how to specify this argument.

- es_ind:

  The names of labels denoting the effect size of an indirect effect.
  They will be used to determine the population values of the component
  paths along an indirect path.

- n_std:

  The sample size used to determine the error variances by simulation
  when `std_force_monte_carlo` is `TRUE`.

- std_force_monte_carlo:

  Logical. If `FALSE`, the default, standardization is done analytically
  if the model has no product terms, and by simulation if the model has
  product terms. That is, error variances required to ensure implied
  variances equal to one are determined by simulation. If `TRUE`,
  simulation will be used whether the model has product terms or not.
  Always fall back to simulation if analytical standardization failed.

- x:

  The object to be printed.

- what:

  A string vector of what to print, `"data"` for simulated data and
  `"test"` for stored test(s). Default is `c("data", "test")`.

- digits:

  The numbers of digits displayed after the decimal.

- digits_descriptive:

  The number of digits displayed after the decimal for the descriptive
  statistics table.

- data_long:

  If `TRUE`, detailed results will be printed when printing the
  simulated data.

- test_long:

  If `TRUE`, detailed results will be printed when printing test(s).

- fit_to_all_args:

  A named list of arguments to be passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) when the
  model is fitted to a sample combined from

- ...:

  Optional arguments to be passed to other print methods

## Value

An object of the class `power4test`, which is a list with two elements:

- `sim_all`: The output of
  [`sim_out()`](https://sfcheung.github.io/power4mome/reference/sim_out.md).

- `test_all`: A named list of the output of
  [`do_test()`](https://sfcheung.github.io/power4mome/reference/do_test.md).
  The names are the values of `test_name`. This list can have more than
  one test because a call to `power4test()` can add new tests to a
  `power4test` object.

The `print` method of `power4test` returns `x` invisibly. Called for its
side effect.

## Details

The function `power4test()` is an all-in-one function for estimating the
power of a test for a model, given the sample size and effect sizes
(population values of model parameters).

## Workflow

This is the workflow:

- If `object` is an output of the output of a previous call to
  `power4test()` with `do_the_test` set to `FALSE` and so has only the
  model and the simulated data, the following steps will be skipped and
  go directly to doing the test.

  - Call
    [`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md)
    to determine the population model and generate the datasets, using
    arguments such as `model` and `pop_es`.

  - Call
    [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
    to fit a model to each of the datasets, which is the population
    model by default.

  - If `R` is not `NULL` and `ci_type = "mc"`, call
    [`gen_mc()`](https://sfcheung.github.io/power4mome/reference/gen_mc.md)
    to generate Monte Carlo estimates using
    [`manymome::do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.html).
    The estimates can be used by supported functions such as
    [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md).

  - If `R` is not `NULL` and `ci_type = "boot"`, call
    [`gen_boot()`](https://sfcheung.github.io/power4mome/reference/gen_boot.md)
    to generate bootstrap estimates using
    [`manymome::do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.html).
    The estimates can be used by supported functions such as
    [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md).

  - Merge the results into a `sim_out` object by calling
    [`sim_out()`](https://sfcheung.github.io/power4mome/reference/sim_out.md).

  - If `do_the_test` is `FALSE`, skip the remaining steps and return a
    `power4test` object, which contains only the data generated and
    optionally the Monte Carlo or bootstrap estimates.

- If `do_the_test` is `TRUE`, do the test.

  - [`do_test()`](https://sfcheung.github.io/power4mome/reference/do_test.md)
    will be called to do the test in the fit output of each dataset.

- Return a `power4test` object which include the output of `sim_out`
  and, if `do_the_test` is `TRUE`, the output of
  [`do_test()`](https://sfcheung.github.io/power4mome/reference/do_test.md).

This function is to be used when users are interested only in the power
of one or several tests on a particular aspect of the model, such as a
parameter, given a specific effect sizes and sample sizes.

Detailed description on major arguments can be found in sections below.

NOTE: The technical internal workflow of of `power4test()` can be found
in this page:
<https://sfcheung.github.io/power4mome/articles/power4test_workflow.html>.

## Updating a Condition

The function `power4test()` can also be used to update a condition when
only some selected aspects is to be changed.

For example, instead of calling this function with all the arguments set
just to change the sample size, it can be called by supplying an
existing `power4test` object and set only `n` to a new sample size. The
data and the tests will be updated automatically. See the examples for
an illustration.

## Adding Another Test

The function `power4test()` can also be used to add a test to the output
from a previous call to `power4test()`.

For example, after simulating the datasets and doing one test, the
output can be set to `object` of `power4test()`, and set only `test_fun`
and, optionally, `test_fun_args` to do one more test on the generated
datasets. The output will be the original object with the results of the
new test added. See the examples for an illustration.

## Model Fitting Arguments ('fit_model_args')

For power analysis, usually, the population model (`model`) is to be
fitted, and there is no need to set `fit_model_args`.

If power analysis is to be conducted for fitting a model that is not the
population model, of if non-default settings are desired when fitting a
model, then the argument `fit_model_args` needed to be set to customize
the call to
[`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md).

For example, users may want to examine the power of a test when a
misspecified model is fitted, or the power of a test when MLR is used as
the estimator when calling
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

See the help page of
[`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
for some examples.

## Specify the Population Model by 'model'

### Single-Group Model

For a single-group model, `model` should be a `lavaan` model syntax
string of the *form* of the model. The population values of the model
parameters are to be determined by `pop_es`.

If the model has latent factors, the syntax in `model` should specify
only the *structural model* for the *latent factors*. There is no need
to specify the measurement part. Other functions will generate the
measurement part on top of this model.

For example, this is a simple mediation model:

    "m ~ x
     y ~ m + x"

Whether `m`, `x`, and `y` denote observed variables or latent factors
are determined by other functions, such as `power4test()`.

### Multigroup Model

Because the model is the population model, equality constraints are
irrelevant and the model syntax specifies only the *form* of the model.
Therefore, `model` is specified as in the case of single group models.

## Specify 'pop_es' Using Named Vectors

The argument `pop_es` is for specifying the population values of model
parameters. This section describes how to do this using named vectors.

### Single-Group Model

If `pop_es` is specified by a named vector, it must follow the
convention below.

- The names of the vectors are `lavaan` names for the selected
  parameters. For example, `m ~ x` denotes the path from `x` to `m`.

- Alternatively, the names can be either `".beta."` or `".cov."`. Use
  `".beta."` to set the default values for all regression coefficients.
  Use `".cov."` to set the default values for all correlations of
  exogenous variables (e.g., predictors).

- The names can also be of this form: `".ind.(<path>)"`, whether
  `<path>` denote path in the model. For example, `".ind.(x->m->y)"`
  denotes the path from `x` through `m` to `y`. Alternatively, the
  `lavaan` symbol `~` can also be used: `".ind.(y~m~x)"`. This form is
  used to set the indirect effect (standardized, by default) along this
  path. The value for this name will override other settings.

- If using `lavaan` names, can specify more than one parameter using
  `+`. For example, `y ~ m + x` denotes the two paths from `m` and `x`
  to `y`.

- The value of each element can be the label for the effect size: `n`
  for nil, `s` for small, `m` for medium, and `l` for large. The value
  for each label is determined by `es1` and `es2`. See the section on
  specifying these two arguments.

- The value of `pop_es` can also be set to a value, but it must be
  quoted as a string, such as `"y ~ x" = ".31"`.

This is an example:

    c(".beta." = "s",
      "m1 ~ x" = "-m",
      "m2 ~ m1" = "l",
      "y ~ x:w" = "s")

In this example,

- All regression coefficients are set to small (`s`) by default, unless
  specified otherwise.

- The path from `x` to `m1` is set to medium and negative (`-m`).

- The path from `m1` to `m2` is set to large (`l`).

- The coefficient of the product term `x:w` when predicting `y` is set
  to small (`s`).

#### Indirect Effect

When setting an indirect effect to a symbol (default: `"si"`, `"mi"`,
`"li"`, with `"i"` added to differentiate them from the labels for a
direct path), the corresponding value is used to determine the
population values of *all* component paths along the pathway. All the
values are assumed to be equal. Therefore, `".ind.(x->m->y)" = ".20"` is
equivalent to setting `m ~ x` and `y ~ m` to the square root of .20,
such that the corresponding indirect effect is equal to the designated
value.

This behavior, though restricted, is for quick manipulation of the
indirect effect. If different values along a pathway, set the value for
each path directly.

Only nonnegative value is supported. Therefore,
`".ind.(x->m->y)" = "-si"` and `".ind.(x->m->y)" = "-.20"` will throw an
error.

### Multigroup Model

The argument `pop_es` also supports multigroup models.

For `pop_es`, instead of named vectors, named *list* of named vectors
should be used.

- The names are the parameters, or keywords such as `.beta.` and
  `.cov.`, like specifying the population values for a single group
  model.

- The elements are character vectors. If it has only one element (e.g.,
  a single string), then it is the the population value for all groups.
  If it has more than one element (e.g., a vector of three strings),
  then they are the population values of the groups. For a model of *k*
  groups, each vector must have either *k* elements or one element.

This is an example:

    list("m ~ x" = "m",
         "y ~ m" = c("s", "m", "l"))

In this model, the population value of the path `m ~ x` is medium (`m`)
for all groups, while the population values for the path `y ~ m` are
small (`s`), medium (`m`), and large (`l`), respectively.

## Specify 'pop_es' Using a Multiline String

When setting the argument `pop_es`, instead of using a named vector or
named list for `pop_es`, the population values of model parameters can
also be specified using a multiline string, as illustrated below, to be
parsed by
[`pop_es_yaml()`](https://sfcheung.github.io/power4mome/reference/pop_es_yaml.md).

### Single-Group Model

This is an example of the multiline string for a single-group model:

    y ~ m: l
    m ~ x: m
    y ~ x: nil

The string must follow this format:

- Each line starts with `tag:`.

  - `tag` can be the name of a parameter, in `lavaan` model syntax
    format.

    - For example, `m ~ x` denotes the path from `x` to `m`.

  - A tag in `lavaan` model syntax can specify more than one parameter
    using `+`.

    - For example, `y ~ m + x` denotes the two paths from `m` and `x` to
      `y`.

  - Alternatively, the `tag` can be either `.beta.` or `.cov.`.

    - Use `.beta.` to set the default values for all regression
      coefficients.

    - Use `.cov.` to set the default values for all correlations of
      exogenous variables (e.g., predictors).

- After each tag is the value of the population value:

  \-`nil` for nil (zero),

  - `s` for small,

  - `m` for medium, and

  - `l` for large.

  - `si`, `mi`, and `li` for small, medium, and large a standardized
    indirect effect, respectively.

  Note: `n` *cannot* be used in this mode.

  The value for each label is determined by `es1` and `es2` as described
  in
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md).

  - The value can also be set to a numeric value, such as `.30` or
    `-.30`.

This is another example:

    .beta.: s
    y ~ m: l

In this example, all regression coefficients are `small`, while the path
from `m` to `y` is large.

### Multigroup Model

This is an example of the string for a multigroup model:

    y ~ m: l
    m ~ x:
      - nil
      - s
    y ~ x: nil

The format is similar to that for a single-group model. If a parameter
has the same value for all groups, then the line can be specified as in
the case of a single-group model: `tag: value`.

If a parameter has different values across groups, then it must be in
this format:

- A line starts with the tag, followed by two or more lines. Each line
  starts with a hyphen `-` and the value for a group.

For example:

    m ~ x:
      - nil
      - s

This denotes that the model has two groups. The values of the path from
`x` to `m` for the two groups are 0 (`nil`) and small (`s`),
respectively.

Another equivalent way to specify the values are using `[]`, on the same
line of a tag.

For example:

    m ~ x: [nil, s]

The number of groups is inferred from the number of values for a
parameter. Therefore, if a tag has more than one value, each tag must
has the same number of value, or only one value.

The tag `.beta.` and `.cov.` can also be used for multigroup models.

### Which Approach To Use

Note that using named vectors or named lists is more reliable. However,
using a multiline string is more user-friendly. If this method failed,
please use named vectors or named list instead.

### Technical Details

The multiline string is parsed by
[`yaml::read_yaml()`](https://yaml.r-lib.org/reference/read_yaml.html).
Therefore, the format requirement is actually that of YAML. Users
knowledgeable of YAML can use other equivalent way to specify the
string.

## Set the Values for Effect Size Labels ('es1' and 'es2')

The vector `es1` is for correlations, regression coefficients, and
indirect effect, and the vector `es2` is for for standardized moderation
effect, the coefficients of a product term. These labels are to be used
in interpreting the specification in `pop_es`.

## Set 'number_of_indicators' and 'reliability'

The arguments `number_of_indicators` and `reliability` are used to
specify the number of indicators (e.g., items) for each factor, and the
population reliability coefficient of each factor, if the variables in
the model syntax are latent variables.

### Single-Group Model

If a variable in the model is to be replaced by indicators in the
generated data, set `number_of_indicators` to a named numeric vector.
The names are the variables of variables with indicators, as appeared in
the `model` syntax. The value of each name is the number of indicators.

The argument `reliability` should then be set a named numeric vector (or
list, see the section on multigroup models) to specify the population
reliability coefficient ("omega") of each set of indicators. The
population standardized factor loadings are then computed to ensure that
the population reliability coefficient is of the target value.

These are examples for a single group model:

    number of indicator = c(m = 3, x = 4, y = 5)

The numbers of indicators for `m`, `x`, and `y` are 3, 4, and 5,
respectively.

    reliability = c(m = .90, x = .80, y = .70)

The population reliability coefficients of `m`, `x`, and `y` are .90,
.80, and .70, respectively.

### Multigroup Models

Multigroup models are supported. The number of groups is inferred from
`pop_es` (see the help page of
[`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md)),
or directly from `ptable`.

For a multigroup model, the number of indicators for each variable must
be the same across groups.

However, the population reliability coefficients can be different across
groups. For a multigroup model of *k* groups, with one or more
population reliability coefficients differ across groups, the argument
`reliability` should be set to a named list. The names are the variables
to which the population reliability coefficients are to be set. The
element for each name is either a single value for the common
reliability coefficient, or a numeric vector of the reliability
coefficient of each group.

This is an example of `reliability` for a model with 2 groups:

    reliability = list(x = .80, m = c(.70, .80))

The reliability coefficients of `x` are .80 in all groups, while the
reliability coefficients of `m` are .70 in one group and .80 in another.

### Equal Numbers of Indicators and/or Reliability Coefficients

If all variables in the model has the same number of indicators,
`number_of_indicators` can be set to one single value.

Similarly, if all sets of indicators have the same population
reliability in all groups, `reliability` can also be set to one single
value.

## Specify The Distributions of Exogenous Variables Or Error Terms Using 'x_fun'

By default, variables and error terms are generated from a multivariate
normal distribution. If desired, users can supply the function used to
generate an exogenous variable and error term by setting `x_fun` to a
named list.

The names of the list are the variables for which a user function will
be used to generate the data.

Each element of the list must also be a list. The first element of this
list, can be unnamed, is the function to be used. If other arguments
need to be supplied, they should be included as named elements of this
list.

For example:

    x_fun = list(x = list(power4mome::rexp_rs),
                 w = list(power4mome::rbinary_rs,
                          p1 = .70)))

The variables `x` and `w` will be generated by user-supplied functions.

For `x`, the function is
[`power4mome::rexp_rs`](https://sfcheung.github.io/power4mome/reference/rexp_rs.md).
No additional argument when calling this function.

For `w`, the function is `power4mome::rbinary_rx`. The argument
`p1 = .70` will be passed to this function when generating the values of
`w`.

If a variable is an endogenous variable (e.g., being predicted by
another variable in a model), then `x_fun` is used to generate its
*error term*. Its implied population distribution may still be different
from that generate by `x_fun` because the distribution also depends on
the distribution of other variables predicting it.

These are requirements for the user-functions:

- They must return a numeric vector.

- They mush has an argument `n` for the number of values.

- The *population* mean and standard deviation of the generated values
  must be 0 and 1, respectively.

The package `power4mome` has helper functions for generating values from
some common nonnormal distributions and then scaling them to have
population mean and standard deviation equal to 0 and 1 (by default),
respectively. These are some of them:

- [`rbinary_rs()`](https://sfcheung.github.io/power4mome/reference/rbinary_rs.md).

- [`rexp_rs()`](https://sfcheung.github.io/power4mome/reference/rexp_rs.md).

- [`rbeta_rs()`](https://sfcheung.github.io/power4mome/reference/rbeta_rs.md).

- [`rlnorm_rs()`](https://sfcheung.github.io/power4mome/reference/rlnorm_rs.md).

- [`rpgnorm_rs()`](https://sfcheung.github.io/power4mome/reference/rpgnorm_rs.md).

To use `x_fun`, the variables must have zero covariances with other
variables in the population. It is possible to generate nonnormal
multivariate data but we believe this is rarely needed when estimating
power *before* having the data.

## Major Test-Related Arguments

### The test function (test_fun)

The function set to `test_fun`, the *test function*, usually should work
on the output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html),
[`lmhelprs::many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.html),
or [`stats::lm()`](https://rdrr.io/r/stats/lm.html), but can also be a
function that works on the output of the function set to `fit_function`
when calling
[`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
or `power4test()` (see `fit_model_args`).

The function has two default requirements.

First, it has an argument `fit`, to be set to the output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) or another
output stored in the element `extra$fit` of a replication in the
`sim_all` object. (This requirement can be relaxed; see the section on
`map_names`.)

That is, the function definition should be of this from:
`FUN(fit, ...)`. This is the form of all `test_*` functions in
`power4mome`.

If other arguments are to be passed to the test function, they can be
set to `test_args` as a named list.

Second, the test function must returns an output that (a) can be
processed by the results function (see below), or (b) is of the required
format for the output of a results function (see the next section). If
it already returns an output of the required format, then there is no
need to set the results function.

### The results function (results_fun)

The test results will be extracted from the output of `test_fun` by the
function set to `results_fun`, the *results function*. If the `test_fun`
already returns an output of the expected format (see below), then set
`results_fun` to `NULL`, the default. The output of `test_fun` will be
used for estimating power.

The function set to `results_fun` must accept the output of `test_fun`,
as the first argument, and return a named list (which can be a data
frame) or a named vector with some of the following elements:

- `est`: Optional. The estimate of a parameter, if applicable.

- `se`: Optional. The standard error of the estimate, if applicable.

- `cilo`: Optional. The lower limit of the confidence interval, if
  applicable.

- `cihi`: Optional. The upper limit of the confidence interval, if
  applicable.

- `sig`: Required. If `1`, the test is significant. If `0`, the test is
  not significant. If the test cannot be done for any reason, it should
  be `NA`.

The results can then be used to estimate the power or Type I error of
the test.

For example, if the null hypothesis is false, then the proportion of
significant, that is, the mean of the values of `sig` across
replications, is the power.

### Built-in test functions

The package `power4mome` has some ready-to-use test functions:

- [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md)

- [`test_cond_indirect()`](https://sfcheung.github.io/power4mome/reference/test_cond_indirect.md)

- [`test_cond_indirect_effects()`](https://sfcheung.github.io/power4mome/reference/test_cond_indirect_effects.md)

- [`test_moderation()`](https://sfcheung.github.io/power4mome/reference/test_moderation.md)

- [`test_index_of_mome()`](https://sfcheung.github.io/power4mome/reference/test_index_of_mome.md)

- [`test_parameters()`](https://sfcheung.github.io/power4mome/reference/test_parameters.md)

Please refer to their help pages for examples.

### The argument map_names

This argument is for developers using a test function that has a
different name for the argument of the fit object (`"fit"`, the
default).

If `test_fun` is set to a function that works on an output of, say,
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) but the
argument name for the output is not `fit`, the mapping can be changed by
`map_names`.

For example,
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
receives an output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and reports
the test results of model parameters. However, the argument name for the
`lavaan` output is `object.` To instruct
[`do_test()`](https://sfcheung.github.io/power4mome/reference/do_test.md)
to do the test correctly when setting `test_fun` to
[`lavaan::parameterEstimates`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html),
add `map_names = c(object = "fit")`. The element `fit` in a replication
will then set to the argument `object` of
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).

## Examples

``` r
# Specify the model

model_simple_med <-
"
m ~ x
y ~ m + x
"

# Specify the population values

model_simple_med_es <-
"
m ~ x: m
y ~ m: l
y ~ x: n
"

# Set nrep to a large number in real analysis, such as 400
# Set `parallel` to TRUE for faster, usually much faster, analysis
# Set `progress` to TRUE to display the progress of the analysis

out <- power4test(nrep = 10,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 100,
                  test_fun = test_parameters,
                  test_args = list(pars = "m~x"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = TRUE)
#> Simulate the data:
#> Fit the model(s):
#> Do the test: test_parameters: CIs (pars: m~x) 

print(out,
      test_long = TRUE)
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> == Model on Variables/Indicators ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.300  
#>   y ~                        
#>     m                 0.500  
#>     x                 0.000  
#> 
#> Variances:
#>                    Population
#>    .m                 0.910  
#>    .y                 0.750  
#>     x                 1.000  
#> 
#> (Computing indirect effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>               ind
#> x -> m -> y 0.150
#> x -> y      0.000
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  10 
#> Sample Sizes:  100 
#> 
#> Call print with 'data_long = TRUE' for further information.
#> 
#> ==================== Extra Element(s) Found ====================
#> 
#> - fit
#> 
#> === Element(s) of the First Dataset ===
#> 
#> ============ <fit> ============
#> 
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> ========== <test_parameters: CIs (pars: m~x)> ==========
#> 
#> Mean(s) across replication:
#>   test_label lhs op rhs   est    se     z pvalue  cilo  cihi   sig
#> 1        m~x   m  ~   x 0.320 0.098 3.234  0.092 0.128 0.512 0.800
#> 
#> - The column 'sig' shows the rejection rates.
#> - If the null hypothesis is false, the rate is the power.
#> - Number of valid replications for rejection rate(s): 10 
#> - Proportion of valid replications for rejection rate(s): 1.000 

# Change the sample size

out1 <- power4test(out,
                   n = 200,
                   iseed = 1234,
                   parallel = FALSE,
                   progress = TRUE)
#> Re-simulate the data:
#> Fit the model(s):
#> Update the test(s):
#> Update test_parameters: CIs (pars: m~x) :

print(out1,
      test_long = TRUE)
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> == Model on Variables/Indicators ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.300  
#>   y ~                        
#>     m                 0.500  
#>     x                 0.000  
#> 
#> Variances:
#>                    Population
#>    .m                 0.910  
#>    .y                 0.750  
#>     x                 1.000  
#> 
#> (Computing indirect effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>               ind
#> x -> m -> y 0.150
#> x -> y      0.000
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  10 
#> Sample Sizes:  200 
#> 
#> Call print with 'data_long = TRUE' for further information.
#> 
#> ==================== Extra Element(s) Found ====================
#> 
#> - fit
#> 
#> === Element(s) of the First Dataset ===
#> 
#> ============ <fit> ============
#> 
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                           200
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> ========== <test_parameters: CIs (pars: m~x)> ==========
#> 
#> Mean(s) across replication:
#>   test_label lhs op rhs   est    se     z pvalue  cilo  cihi   sig
#> 1        m~x   m  ~   x 0.305 0.067 4.571  0.000 0.174 0.436 1.000
#> 
#> - The column 'sig' shows the rejection rates.
#> - If the null hypothesis is false, the rate is the power.
#> - Number of valid replications for rejection rate(s): 10 
#> - Proportion of valid replications for rejection rate(s): 1.000 

# Add one more test

out2 <- power4test(out,
                   test_fun = test_parameters,
                   test_args = list(pars = "y~x"),
                   parallel = FALSE,
                   progress = TRUE)
#> Do the test: test_parameters: CIs (pars: y~x) 

print(out2,
      test_long = TRUE)
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> == Model on Variables/Indicators ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.300  
#>   y ~                        
#>     m                 0.500  
#>     x                 0.000  
#> 
#> Variances:
#>                    Population
#>    .m                 0.910  
#>    .y                 0.750  
#>     x                 1.000  
#> 
#> (Computing indirect effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>               ind
#> x -> m -> y 0.150
#> x -> y      0.000
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  10 
#> Sample Sizes:  100 
#> 
#> Call print with 'data_long = TRUE' for further information.
#> 
#> ==================== Extra Element(s) Found ====================
#> 
#> - fit
#> 
#> === Element(s) of the First Dataset ===
#> 
#> ============ <fit> ============
#> 
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> ========== <test_parameters: CIs (pars: m~x)> ==========
#> 
#> Mean(s) across replication:
#>   test_label lhs op rhs   est    se     z pvalue  cilo  cihi   sig
#> 1        m~x   m  ~   x 0.320 0.098 3.234  0.092 0.128 0.512 0.800
#> 
#> - The column 'sig' shows the rejection rates.
#> - If the null hypothesis is false, the rate is the power.
#> - Number of valid replications for rejection rate(s): 10 
#> - Proportion of valid replications for rejection rate(s): 1.000 
#> 
#> ========== <test_parameters: CIs (pars: y~x)> ==========
#> 
#> Mean(s) across replication:
#>   test_label lhs op rhs    est    se      z pvalue   cilo  cihi   sig
#> 1        y~x   y  ~   x -0.069 0.093 -0.729  0.389 -0.252 0.113 0.000
#> 
#> - The column 'sig' shows the rejection rates.
#> - If the null hypothesis is false, the rate is the power.
#> - Number of valid replications for rejection rate(s): 10 
#> - Proportion of valid replications for rejection rate(s): 1.000 
```
