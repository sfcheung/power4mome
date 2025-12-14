# All-in-One Power Estimation For Mediation Models

All-in-one functions for estimating power or finding the region with
target power for common mediation models.

## Usage

``` r
q_power_mediation(
  model = NULL,
  pop_es = NULL,
  number_of_indicators = NULL,
  reliability = NULL,
  test_fun = NULL,
  test_more_args = list(),
  target_power = 0.8,
  nrep = 400,
  n = 100,
  R = 1000,
  ci_type = c("mc", "boot"),
  seed = NULL,
  iseed = NULL,
  parallel = TRUE,
  progress = TRUE,
  simulation_progress = TRUE,
  max_trials = 10,
  ...,
  mode = c("power", "region")
)

# S3 method for class 'q_power_mediation'
print(x, mode = c("all", "region", "power"), ...)

# S3 method for class 'q_power_mediation'
plot(x, ...)

# S3 method for class 'q_power_mediation'
summary(object, ...)

q_power_mediation_simple(
  a = "m",
  b = "m",
  cp = "n",
  number_of_indicators = NULL,
  reliability = NULL,
  test_more_args = list(),
  target_power = 0.8,
  nrep = 400,
  n = 100,
  R = 1000,
  ci_type = c("mc", "boot"),
  seed = NULL,
  iseed = NULL,
  parallel = TRUE,
  progress = TRUE,
  simulation_progress = TRUE,
  max_trials = 10,
  ...,
  mode = c("power", "region")
)

q_power_mediation_serial(
  ab = c("m", "m"),
  ab_other = "n",
  cp = "n",
  number_of_indicators = NULL,
  reliability = NULL,
  test_more_args = list(),
  target_power = 0.8,
  nrep = 400,
  n = 100,
  R = 1000,
  ci_type = c("mc", "boot"),
  seed = NULL,
  iseed = NULL,
  parallel = TRUE,
  progress = TRUE,
  simulation_progress = TRUE,
  max_trials = 10,
  ...,
  mode = c("power", "region")
)

q_power_mediation_parallel(
  as = c("m", "m"),
  bs = c("m", "m"),
  cp = "n",
  number_of_indicators = NULL,
  reliability = NULL,
  omnibus = c("all_sig", "at_least_one_sig", "at_least_k_sig"),
  at_least_k = 1,
  test_more_args = list(),
  target_power = 0.8,
  nrep = 400,
  n = 100,
  R = 1000,
  ci_type = c("mc", "boot"),
  seed = NULL,
  iseed = NULL,
  parallel = TRUE,
  progress = TRUE,
  simulation_progress = TRUE,
  max_trials = 10,
  ...,
  mode = c("power", "region")
)
```

## Arguments

- model:

  The `lavaan` model syntax of the population model, to be used by
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md).
  See 'Details' of on how to specify the model. Ignored if `ptable` is
  specified.

- pop_es:

  The character vector or multiline string to specify population effect
  sizes (population values of parameters). See the help page on how to
  specify this argument. Ignored if `ptable` is specified.

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

- test_fun:

  A function to do the test. See 'Details' of
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  for the requirement of this function.

- test_more_args:

  A named list of additional arguments to be passed to the test function
  ([`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md)
  for simple and serial mediation models, and
  [`test_k_indirect_effects()`](https://sfcheung.github.io/power4mome/reference/test_k_indirect_effects.md)
  for parallel mediation models). Similar to `test_args` in
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

- target_power:

  The target power, a value greater than 0 and less than one.

- nrep:

  The number of replications to generate the simulated datasets. Default
  is `NULL`. Must be set when called to create a `power4test` object.

- n:

  The sample size for each dataset. Default is 100.

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

- seed:

  The seed for the random number generator. Used by
  [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).

- iseed:

  The seed for the random number generator. Used by
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

- parallel:

  If `TRUE`, parallel processing will be used when calling other
  functions, if appropriate.

- progress:

  If `TRUE`, the progress of each step will be displayed. Default is
  `TRUE`.

- simulation_progress:

  Logical. Whether the progress in each call to
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
  [`power4test_by_n()`](https://sfcheung.github.io/power4mome/reference/power4test_by_n.md),
  or
  [`power4test_by_es()`](https://sfcheung.github.io/power4mome/reference/power4test_by_es.md)
  is shown. To be passed to the `progress` argument of these functions.

- max_trials:

  The maximum number of trials in searching the value with the target
  power. Rounded up if not an integer.

- ...:

  For `q_power_mediation_*`, these are optional arguments to be passed
  to
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  and
  [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).
  For the `print` method, these are optional arguments to be passed to
  other print methods (see
  [`print.power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  and
  [`print.n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)).
  For the `plot` method, these are optional arguments to be passed to
  [`plot.n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/plot.x_from_power.md).
  For the `summary` method, these are optional arguments to be passed to
  [`summary.n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/summary.x_from_power.md).

- mode:

  What to print. If `"region"` and the output of
  [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  is available, it will print the results of
  [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).
  If `"power"`, then the output of
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  will be printed. If `"all"`, then all available output will be
  printed.

- x:

  The object for the relevant methods.

- object:

  For the `summary` method of `q_power_mediation()` outputs.

- a:

  For a simple mediation model, this is the population effect size for
  the path from `x` to `m` .

- b:

  For a simple mediation model, this is the population effect size for
  the path from `m` to `y` .

- cp:

  For a simple mediation model, this is the population effect size for
  the direct path from `c` to `y` .

- ab:

  For a serial mediation model, this is a numeric vector of the
  population effect sizes along the path `x->m1->m2->...->y`.

- ab_other:

  Should be one single value. This is the population effect sizes of all
  other paths not along `x->m1->m2->...->y`, except for the direct path
  from `x` to `y`.

- as:

  For a parallel mediation model, this is a numeric vector of the
  population effect sizes for the paths from `x` to the mediators:
  `x->m1`, `x->m2`, ... `x->mp`, for a parallel mediation model with `p`
  mediators.

- bs:

  For a parallel mediation model, this is a numeric vector of the
  population effect sizes for the paths from the mediators to `y`:
  `m1->y`, `m2->y`, ... `mp->y`, for a parallel mediation model with `p`
  mediators.

- omnibus:

  `"all_sig"`, the default, then the test is declared significant if
  *all* paths are significant. If `"at_least_one_sig"`, then only one
  row of test is stored, and the test is declared significant if at
  least one of the paths is significant. If `"at_least_k_sig"`, then
  only one row of test is stored, and the test is declared significant
  if at least `k` of the paths is significant, `k` determined by the
  argument `at_least_k`.

- at_least_k:

  The minimum number of paths required to be significant for the omnibus
  test to be considered significant. Used when `omnibus` is
  `"at_least_k_sig"`.

## Value

If `mode` is `power`, then a `power4test` object is returned. If `mode`
is `region`, then a `n_region_from_power` object is returned.

The `print` method of `q_power_mediation` returns `x` invisibly. Called
for its side effect.

The `plot`-method of `q_power_mediation` returns `x` invisibly. It is
called for its side effect.

The `summary` method for `q_power_mediation` objects returns the output
of
[`summary.n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/summary.x_from_power.md).
An error is raised if the output of
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
is not available.

## All-in-One Functions for Common Mediation Models

These functions are wrappers that call
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
to (a) estimate the level of power for a mediation model, given the
population effects and the sample size, and (b) find the region of
sample sizes with the levels of power not significantly different from
the target power.

They are convenient functions that set the argument values automatically
for common mediation models before calling
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).
Please refer to the help pages of these two functions for the details on
how the estimation and the search are conducted.

For some arguments not described in details here, please refer to the
help pages of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),

## Simple Mediation Model

The function `q_power_mediation_simple()` can be used for the power
analysis of a simple mediation model with only one mediator.

This function will fit the following model:

    "m ~ x
     y ~ m + x"

## Serial Mediation Model

The function `q_power_mediation_serial()` can be used for the power
analysis of a serial mediation model with only any number of mediators.

This is the model being fitted if the model has two mediators:

    "m1 ~ x
     m2 ~ m1 + x
     y ~ m2 + m1 + x"

## Parallel Mediation Model

The function `q_power_mediation_parallel()` can be used for the power
analysis of a parallel mediation model with only any number of
mediators.

This is the model being fitted if the model has two mediators:

    "m1 ~ x
     m2 ~ x
     y ~ m2 + m1 + x"

## An Arbitrary Mediation Model

The function `q_power_mediation()`, an advanced function, can be used
for the power analysis of an arbitrary mediation model. The model and
the population effect sizes are specified as in
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

This is an example of a model with both parallel paths and serial paths:

    model <-
     "
     m1 ~ x
     m21 ~ m1
     m22 ~ m1
     y ~ m21 + m22 + x
     "

    pop_es <-
    "
    m1 ~ x: m
    m21 ~ m1: m
    m22 ~ m1: m
    y ~ m21: m
    y ~ m22: m
    "

Knowledge of using
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
is required to use this advanced function.

If this advanced function is used, users need to specify `test_fun` as
when using
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
and need to set `test_args` correctly

## See also

See
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
for full details on how these functions work.

## Examples

``` r
if (FALSE) { # \dontrun{

# An arbitrary mediation model

model <-
"
m1 ~ x
m21 ~ m1
m22 ~ m1
y ~ m21 + m22
"
pop_es <-
"
m1 ~ x: m
m21 ~ m1: m
m22 ~ m1: m
y ~ m21: m
y ~ m22: m
"

# NOTE: In real power analysis:
# - Set R to an appropriate value.
# - Remove nrep or set nrep to the desired value.
# - Remove parallel or set it to TRUE to enable parallel processing.
# - Remove progress or set it to TRUE to see the progress.

outa1 <- q_power_mediation(
    model = model,
    pop_es = pop_es,
    n = 100,
    R = 199,
    test_fun = test_k_indirect_effects,
    test_more_args = list(x = "x",
                          y = "y",
                          omnibus = "all"),
    seed = 1234,
    mode = "region",
    nrep = 20,
    parallel = FALSE,
    progress = FALSE
  )
outa1
summary(outa1)
plot(outa1)

} # }


# Simple mediation model

# NOTE: In real power analysis:
# - Set R to an appropriate value.
# - Remove nrep or set nrep to the desired value.
# - Remove parallel or set it to TRUE to enable parallel processing.
# - Remove progress or set it to TRUE to see the progress.

out <- q_power_mediation_simple(
    a = "m",
    b = "m",
    cp = "n",
    n = 50,
    R = 199,
    seed = 1234,
    nrep = 20,
    parallel = FALSE,
    progress = FALSE
  )
out
#> 
#> ========== power4test Results ==========
#> 
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> m ~ x
#> y ~ m + x
#> 
#> == Model on Variables/Indicators ==
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
#> Number of Replications:  20 
#> Sample Sizes:  50 
#> 
#> Call print with 'data_long = TRUE' for further information.
#> 
#> ==================== Extra Element(s) Found ====================
#> 
#> - fit
#> - mc_out
#> 
#> === Element(s) of the First Dataset ===
#> 
#> ============ <fit> ============
#> 
#> lavaan 0.6-20 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                            50
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> =========== <mc_out> ===========
#> 
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 199 
#> 
#> 
#> ====================== Test(s) Conducted ======================
#> 
#> - test_indirect: x->m->y
#> 
#> Call print() and set 'test_long = TRUE' for a detailed report.
#> 
#> ========== power4test Power ==========
#> 
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     est   p.v reject r.cilo r.cihi
#> 1 0.071 1.000  0.250  0.112  0.469
#> Notes:
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
#> 
#> ========== n_region_from_power Results ==========
#> 
#> 
#> 'mode' is not 'region' and results not available.
#> 

# If mode = "region" is added, can call the following
# summary(out)
# plot(out)


if (FALSE) { # \dontrun{
# Serial mediation model

# NOTE: In real power analysis:
# - Set R to an appropriate value.
# - Remove nrep or set nrep to the desired value.
# - Remove parallel or set it to TRUE to enable parallel processing.
# - Remove progress or set it to TRUE to see the progress.

outs <- q_power_mediation_serial(
    ab = c("s", "m", "l"),
    ab_others = "n",
    cp = "s",
    n = 50,
    R = 199,
    seed = 1234,
    mode = "region",
    nrep = 20,
    parallel = FALSE,
    progress = FALSE
  )
outs
summary(outs)
plot(outs)
} # }


if (FALSE) { # \dontrun{
# Parallel mediation model

# NOTE: In real power analysis:
# - Set R to an appropriate value.
# - Remove nrep or set nrep to the desired value.
# - Remove parallel or set it to TRUE to enable parallel processing.
# - Remove progress or set it to TRUE to see the progress.

outp <- q_power_mediation_parallel(
    as = c("s", "m"),
    bs = c("m", "s"),
    cp = "n",
    n = 100,
    R = 199,
    seed = 1234,
    mode = "region",
    nrep = 20,
    parallel = FALSE,
    progress = FALSE
  )
outp
summary(outp)
plot(outp)
} # }
```
