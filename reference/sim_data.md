# Simulate Datasets Based on a Model

Get a model matrix and effect size specification and simulate a number
of datasets, along with other information.

The function

## Usage

``` r
sim_data(
  nrep = 10,
  ptable = NULL,
  model = NULL,
  pop_es = NULL,
  ...,
  n = 100,
  iseed = NULL,
  number_of_indicators = NULL,
  reliability = NULL,
  x_fun = list(),
  e_fun = list(),
  process_data = NULL,
  parallel = FALSE,
  progress = FALSE,
  ncores = max(1, parallel::detectCores(logical = FALSE) - 1)
)

# S3 method for class 'sim_data'
print(
  x,
  digits = 3,
  digits_descriptive = 2,
  data_long = TRUE,
  fit_to_all_args = list(),
  est_type = "standardized",
  variances = NULL,
  pure_x = TRUE,
  pure_y = TRUE,
  ...
)

pool_sim_data(object, as_list = FALSE)
```

## Arguments

- nrep:

  The number of replications to generate the simulated datasets. Default
  is 10.

- ptable:

  The output of
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md),
  which is a `ptable_pop` object, representing the population model. If
  `NULL`, the default,
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md)
  will be called to generate the `ptable_pop` object, using arguments
  such as `model` and `pop_es`.

- model:

  The `lavaan` model syntax of the population model. Ignored if `ptable`
  is specified. See
  [ptable_pop](https://sfcheung.github.io/power4mome/reference/ptable_pop.md)
  on how to specify this argument.

- pop_es:

  The character to specify population effect sizes. See
  [ptable_pop](https://sfcheung.github.io/power4mome/reference/ptable_pop.md)
  on how to specify this argument. Ignored if `ptable` is specified.

- ...:

  For sim_data, parameters to be passed to
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md).
  For `print.sim_data()`, these arguments are ignored.

- n:

  The sample size for each dataset. Default is 100.

- iseed:

  The seed for the random number generator. Default is `NULL` and the
  seed is not changed.

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

- parallel:

  If `TRUE`, parallel processing will be used to simulate the datasets.
  Default is `FALSE`.

- progress:

  If `TRUE`, the progress of data simulation will be displayed. Default
  is \`FALSE.

- ncores:

  The number of CPU cores to use if parallel processing is used.

- x:

  The `sim_data` object to be printed.

- digits:

  The numbers of digits displayed after the decimal.

- digits_descriptive:

  The number of digits displayed after the decimal for the descriptive
  statistics table.

- data_long:

  If `TRUE`, detailed information will be printed.

- fit_to_all_args:

  A named list of arguments to be passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) when the
  model is fitted to a sample combined from all samples stored.

- est_type:

  The type of estimates to be printed. Can be a character vector of one
  to two elements. If only `"standardized"`, then the standardized
  estimates are printed. If only `"unstandardized"`, then the
  unstandardized estimates are printed. If a vector like
  `c("standardized", "unstandardized")`, then both unstandardized and
  standardized estimates are printed.

- variances:

  Logical. Whether variances and error variances are printed. Default
  depends on `est_type`. If `"unstandardized"` is in `est_type`, then
  default is `TRUE` If only `"standardized"` is in `est_type`, then
  default is `FALSE`.

- pure_x, pure_y:

  When Logical. When printing indirect effects, whether only "pure"
  x-variables (variables not predicted by another other variables)
  and/or "pure" y-variables (variables that do not predict any other
  variables other than indicators) will be included in enumerating the
  paths.

- object:

  Either a `sim_data` object or a `power4test` object. It extracts the
  simulated data and return them, combined to one single data frame or,
  if `as_list` is `TRUE`, as a list of data frames.

- as_list:

  Logical. If `TRUE`, the simulated datasets is returned as one single
  data frame. If `FALSE`, they are returned as a list of data frames.

## Value

The function
[`sim_out()`](https://sfcheung.github.io/power4mome/reference/sim_out.md)
returns a list of the class `sim_data`, with length `nrep`. Each element
is a `sim_data_i` object, with the following major elements:

- `ptable`: A `lavaan` parameter table of the model, with population
  values set in the column `start`. (It is the output of the function
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md).)

- `mm_out`: The population model represented by model matrices as in
  `lavaan`. (It is the output of the function
  [`model_matrices_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md).)

- `mm_lm_out`: A list of regression model formula, one for each
  endogenous variable. (It is the output of the internal function
  `mm_lm()`.)

- `mm_lm_dat_out`: A simulated dataset generated from the population
  model. (It is the output of the internal function `mm_lm_data()`).

- `model_original`: The original model syntax (i.e., the argument
  `model`).

- `model_final`: A modified model syntax if the model is a latent
  variable model. Indicators are added to the syntax.

- `fit0`: The output of
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) with
  `ptable` as the model and `do.fit` set to `FALSE`. Used for the easy
  retrieval of information about the model.

The `print` method of `sim_data` returns `x` invisibly. It is called for
its side effect.

The function `pool_sim_data()` returns either one data frame or a list
of data frames, depending on the argument `as_list`

## Details

The function `sim_data()` generates a list of datasets based on a
population model.

## The role of `sim_data()`

The function `sim_data()` is used by the all-in-one function
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
Users usually do not call this function directly, though developers can
use this function to develop other functions for power analysis, or to
build their own workflows to do the power analysis.

## Workflow

The function `sim_data()` does two tasks:

- Determine the actual population model with population values based on:

  - A model syntax for the observed variables (for a path model) or the
    latent factors (for a latent variable model).

  - A textual specification of the effect sizes of parameters.

  - The number of indicators for each latent factor if the model is a
    latent variable model.

  - The reliability of each latent factor as measured by the indicators
    if the model is a latent factor model.

- Generate *nrep* simulated datasets from the population model.

The simulated datasets can then be used to fit a model, test parameters,
and estimate power.

The output is usually used by
[`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
to fit a target model, by default the population model, to each of the
dataset.

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
are determined by other functions, such as
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

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

## See also

[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)

## Examples

``` r
# Specify the model

mod <-
"m ~ x
 y ~ m + x"

# Specify the population values

es <-
"
y ~ m: m
m ~ x: m
y ~ x: n
"

# Generate the simulated datasets

data_all <- sim_data(nrep = 5,
                     model = mod,
                     pop_es = es,
                     n = 100,
                     iseed = 1234)

data_all
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> m ~ x
#>  y ~ m + x
#> == Model on Variables/Indicators ==
#> m ~ x
#>  y ~ m + x
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.300  
#>   y ~                        
#>     m                 0.300  
#>     x                 0.000  
#> 
#> Variances:
#>                    Population
#>    .m                 0.910  
#>    .y                 0.910  
#>     x                 1.000  
#> 
#> (Computing indirect effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>               ind
#> x -> m -> y 0.090
#> x -> y      0.000
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  5 
#> Sample Sizes:  100 
#> 
#> ==== Descriptive Statistics ====
#> 
#>   vars   n  mean   sd  skew kurtosis   se
#> m    1 500 -0.03 1.00 -0.09    -0.03 0.04
#> y    2 500 -0.01 0.98  0.03     0.41 0.04
#> x    3 500  0.01 0.94 -0.21     0.17 0.04
#> 
#> ===== Parameter Estimates Based on All 5 Samples Combined =====
#> 
#> Total Sample Size: 500 
#> 
#> ==== Standardized Estimates ====
#> 
#> Variances and error variances omitted.
#> 
#> Regressions:
#>                     est.std
#>   m ~                      
#>     x                 0.323
#>   y ~                      
#>     m                 0.277
#>     x                -0.098
#> 
```
