# Power Analysis for Moderation, Mediation, and Moderated Mediation

## Introduction

This article is a brief illustration of how to use
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
from the package [power4mome](https://sfcheung.github.io/power4mome/) to
do power analysis of mediation, moderation, and moderated mediation in a
model to be fitted by structural equation modeling using `lavaan`.

## Prerequisite

Basic knowledge about fitting models by `lavaan` is required. Readers
are also expected to have basic knowledge of mediation, moderation,
and/or moderated mediation.

## Scope

This is a brief illustration. More complicated scenarios and other
features of `power4mome` will be described in other vignettes.

## Package

This introduction only needs the following package:

``` r
library(power4mome)
```

## Workflow

Two functions are sufficient for estimating power given a model,
population values, sample size, and the test to be used. This is the
workflow:

1.  Specify the model syntax for the population model, in `lavaan`
    style, and set the population values of the model parameters.

2.  Call
    [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
    to examine the setup and the datasets generated. Repeat this and
    previous steps until the model is specified correctly.

3.  Call
    [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
    again, with the test to do specified.

4.  Call
    [`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md)
    to compute the power.

## Mediation

Let’s consider a simple mediation model. We would like to estimate the
power of testing a mediation effect by Monte Carlo confidence interval.

### Specify the Population Model

This is the model syntax

``` r
mod <-
"
m ~ x
y ~ m + x
"
```

Note that, even if we are going to test mediation, moderation, or
moderated mediation effects, we do not need to add any labels to this
model. This will be taken care of by the test functions, through the use
of the package [`manymome`](https://sfcheung.github.io/manymome/)
(Cheung & Cheung, 2024).

### Specify The Population Values

There are two approaches to do this:

- Using named vectors or lists.

- Using a multiline string similar to `lavaan` model syntax.

The second approach is demonstrated below.

Suppose we want to estimate the power when:

- The path from `x` to `m` are “large” in strength.

- The path from `m` to `y` are “medium” in strength.

- The path from `x` to `m` are “small” in strength.

By default, `power4mome` uses this convention for regression path and
correlation:[¹](#fn1)

- Small: .10 (or -.10)

- Medium: .30 (or -.30)

- Large: .50 (or -.50)

For a product term in moderation, this is the convention:

- Small: .05 (or -.05)

- Medium: .10 (or -.10)

- Large: .15 (or -.15)

All these values are for the standardized solution (the correlations and
so-called “betas”).

The following string denotes the desired values:

``` r
mod_es <-
"
m ~ x: l
y ~ m: m
y ~ x: s
"
```

Each line starts with a *tag*, which is the parameter presented in
`lavaan` syntax. The tag ends with a colon, `:`.

After the colon is *population value*, which can be:

- A string denoting the value. By default:

  - `s`: Small. (`-s` for small and negative.)

  - `m`: Medium. (`-m` for medium and negative.)

  - `l`: Large. (`-l` for large and negative.)

  - `nil`: Zero.

All other regression coefficients and covariances, if not specified in
this string, are set to zero.

### Call `power4test()` to Check the Model

``` r
out <- power4test(nrep = 2,
                  model = mod,
                  pop_es = mod_es,
                  n = 50000,
                  iseed = 1234)
```

These are the arguments used:

- `nrep`: The number of replications. In this stage, a small number can
  be used. It is more important to have a large sample size than to have
  many replications.

- `model`: The model syntax.

- `pop_es`: The string setting the population values.

- `n`: The sample size in each replications. In this stage, just for
  checking the model and the data generation, this number can be set to
  a large value unless the model is slow to fit when the sample size is
  large.

- `iseed`: If supplied, it is used to set the seed for the random number
  generator. It is advised to always set this to an arbitrary integer,
  to make the results reproducible.[²](#fn2)

The population values can be shown by printing this object:

``` r
out
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
#>     x                 0.500  
#>   y ~                        
#>     m                 0.300  
#>     x                 0.100  
#> 
#> Variances:
#>                    Population
#>    .m                 0.750  
#>    .y                 0.870  
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
#> x -> y      0.100
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  2 
#> Sample Sizes:  50000 
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
#> lavaan 0.6-20 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                         50000
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
```

By default, the population model will be fitted to each dataset, hence
the section `<fit>`. Perfect fit is expected if the population model is
a saturated model.

We check the section `Population Values` to see whether the values are
what we expected.

- In this example, the population values for the regression paths are
  what we specified.

If they are different from what we expect, check the string for `pop_es`
to see whether we set the population values correctly.

If necessary, we can check the data generation by adding
`data_long = TRUE` when printing the output:

``` r
print(out,
      data_long = TRUE)
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
#>     x                 0.500  
#>   y ~                        
#>     m                 0.300  
#>     x                 0.100  
#> 
#> Variances:
#>                    Population
#>    .m                 0.750  
#>    .y                 0.870  
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
#> x -> y      0.100
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  2 
#> Sample Sizes:  50000 
#> 
#> ==== Descriptive Statistics ====
#> 
#>   vars     n mean sd skew kurtosis se
#> m    1 1e+05 0.00  1 0.01     0.03  0
#> y    2 1e+05 0.01  1 0.01     0.00  0
#> x    3 1e+05 0.00  1 0.01     0.01  0
#> 
#> ===== Parameter Estimates Based on All 2 Samples Combined =====
#> 
#> Total Sample Size: 100000 
#> 
#> ==== Standardized Estimates ====
#> 
#> Variances and error variances omitted.
#> 
#> Regressions:
#>                     est.std
#>   m ~                      
#>     x                 0.500
#>   y ~                      
#>     m                 0.295
#>     x                 0.102
#> 
#> 
#> ==================== Extra Element(s) Found ====================
#> 
#> - fit
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
#>   Number of observations                         50000
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
```

The section `Descriptive Statistics`, generated by
[`psych::describe()`](https://rdrr.io/pkg/psych/man/describe.html),
shows basic descriptive statistics for the observed variables. As
expected, they have means close to zero and standard deviations close to
one, because the datasets were generated using the standardized model.

The section `Parameter Estimates Based on` shows the parameter estimates
when the *population model* is fitted to *all the datasets combined*.
When the total sample size is large, these estimates should be close to
the population values.

The results show that we have specified the population model correctly.
We can proceed to specify the test and estimate the power.

### Call `power4test()` to Do the Target Test

We can now do the simulation to estimate power. A large number of
datasets (e.g., 500) of the target sample size are to be generated, and
then the target test will be conducted in each of these datasets.

Suppose we would like to estimate the power of using Monte Carlo
confidence intervals to test the indirect effect from `x` to `y` through
`m`, when sample size is 50. This is the call:

``` r
out <- power4test(nrep = 400,
                  model = mod,
                  pop_es = mod_es,
                  n = 50,
                  R = 2000,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 1234,
                  parallel = TRUE)
```

These are the new arguments used:

- `R`: The number of replications used to generate the Monte Carlo
  simulated estimates, 2000 in this example. In real studies, this
  number should be 10000 or even 20000 for Monte Carlo confidence
  intervals. However, 2000 is sufficient because the goal is to estimate
  power by generating many intervals, rather than to have one single
  stable interval.

- `ci_type`: The method used to generate estimates. Support both Monte
  Carlo (`"mc"`) and nonparametric bootstrapping (`"boot"`).[³](#fn3)
  Although bootstrapping is usually used to test an indirect effect, it
  is very slow to do `R` bootstrapping in `nrep` datasets (the model
  will be fitted `R * nrep` times). Therefore, it is preferable to use
  Monte Carlo confidence intervals to do the initial estimation.

- `test_fun`: The function to be used to do the test for each
  replication. Any function following a specific requirement can be
  used, and `power4mome` comes with several built-in functions for some
  common tests. The function
  [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md)
  is used to test an indirect effect in the model.

- `test_args`: A named list of arguments to be supplied to `test_fun`.
  For
  [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md),
  it is a named list specifying the predictor (`x`), the mediator(s)
  (`m`), and the outcome (`y`). A path with any number of mediators can
  be supported. Please refer to the help page of
  [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md).[⁴](#fn4)

- `parallel`: If the test to be conducted is slow, which is the case for
  tests done by Monte Carlo or nonparametric bootstrapping confidence
  intervals, it is advised to enable parallel processing by setting
  `parallel` to `TRUE`.[⁵](#fn5)

For `nrep = 400`, the 95% confidence limits for a power of .80 are about
.04 below and above .80. This should be precise enough for determining
whether a sample size has sufficient power.

This is the default printout:

``` r
out
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
#>     x                 0.500  
#>   y ~                        
#>     m                 0.300  
#>     x                 0.100  
#> 
#> Variances:
#>                    Population
#>    .m                 0.750  
#>    .y                 0.870  
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
#> x -> y      0.100
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  400 
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
#> Number of Monte Carlo replications: 2000 
#> 
#> 
#> ====================== Test(s) Conducted ======================
#> 
#> - test_indirect: x->m->y
#> 
#> Call print() and set 'test_long = TRUE' for a detailed report.
```

If `test_long = TRUE` is added when printing the output by
[`print()`](https://rdrr.io/r/base/print.html), a summary of the test
will also be printed.

``` r
print(out,
      test_long = TRUE)
```

The summary of the test:

    #>                                                       
    #>   Test statistic                                 0.000
    #>   Degrees of freedom                                 0
    #> 
    #> =========== <mc_out> ===========
    #> 
    #> 
    #> == A 'mc_out' class object ==
    #> 
    #> Number of Monte Carlo replications: 2000

The mean of the estimates across all the replications is 0.152, close to
the population value.

### Compute the Power

The power estimate is simply the proportion of significant results, the
*rejection* *rate*, because the null hypothesis is false. In addition to
using `test_long = TRUE` in
[`print()`](https://rdrr.io/r/base/print.html), the rejection rate can
also be retrieved by
[`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md).

``` r
out_power <- rejection_rates(out)
out_power
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     est   p.v reject r.cilo r.cihi
#> 1 0.152 1.000  0.468  0.419  0.516
#> Notes:
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
```

In the example above, the estimated power of the test of the indirect
effect, conducted by Monte Carlo confidence intervals, is 0.468, under
the column `reject`.

`p.v` is the proportion of valid results across replications. `1.000`
means that the test conducted normally in all replications.

By default, the 95% confidence interval of the rejection rate (power)
based on normal approximation is also printed, under the column `r.cilo`
and `r.cihi`. In this example, the 95% confidence interval is \[0.419;
0.516\].

## Moderation

Let’s consider a moderation model, with some control variables.

### Specify the Population Model and Values

``` r
mod2 <-
"
y ~ x + w + x:w + control
"
```

This model has only moderation, with the predictor `x` and the moderator
`w`. The product term is included in the `lavaan` style, `x:w`.

It is unrealistic to specific the population values for all control
variables. Therefore, we can just add a *proxy*, `control` to represent
the *set of control variables* that may be included.

This is the syntax for the population values:

``` r
mod2_es <-
"
.beta.: s
x ~~ control: s
y ~ control: s
y ~ x:w: l
"
```

This example introduces one useful tag, `.beta.` For a model with many
paths, it is inconvenient to specify all of them manually. The tag
`.beta.` specifies the *default value* for *all regression paths not
specified explicitly*, which is small (.10) in this example. If a path
is explicitly included (such as `y ~ control` and `y ~ x:w`), the
manually specified value will be used instead of `.beta.`.

This example also illustrates that we can set the population values for
*correlations* (covariances in the standardized solution). Control
variables are included usually because they may correlate with the
predictors. Therefore, in this example, it is hypothesized that there is
a small correlation between `x` and the proxy control variable
(`x ~~ control: s`).

Last, recall from [this section](#es_convention) that the convention for
product term values is different: `l` denotes .15 for product terms.

### Call `power4test()` to Check the Model

We check the model first:

``` r
out2 <- power4test(nrep = 2,
                   model = mod2,
                   pop_es = mod2_es,
                   n = 50000,
                   iseed = 1234)
```

``` r
print(out2,
      data_long = TRUE)
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> 
#> y ~ x + w + x:w + control
#> 
#> == Model on Variables/Indicators ==
#> 
#> y ~ x + w + x:w + control
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   y ~                        
#>     x                 0.100  
#>     w                 0.100  
#>     x:w               0.150  
#>     control           0.100  
#> 
#> Covariances:
#>                    Population
#>   x ~~                       
#>     w                 0.000  
#>     x:w               0.000  
#>     control           0.100  
#>   w ~~                       
#>     x:w               0.000  
#>     control           0.000  
#>   x:w ~~                     
#>     control           0.000  
#> 
#> Variances:
#>                    Population
#>    .y                 0.945  
#>     x                 1.000  
#>     w                 1.000  
#>     x:w               1.000  
#>     control           1.000  
#> 
#> (Computing indirect effects for 1 paths ...)
#> 
#> (Computing conditional effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Effect(s) ==
#> 
#>                ind
#> control -> y 0.100
#> 
#>  - The 'ind' column shows the effect(s).
#>  
#> == Conditional effects ==
#> 
#>  Path: x -> y
#>  Conditional on moderator(s): w
#>  Moderator(s) represented by: w
#> 
#>       [w] (w)    ind
#> 1 M+1.0SD   1  0.250
#> 2 Mean      0  0.100
#> 3 M-1.0SD  -1 -0.050
#> 
#>  - The 'ind' column shows the conditional effects.
#>  
#> 
#> == Conditional effects ==
#> 
#>  Path: w -> y
#>  Conditional on moderator(s): x
#>  Moderator(s) represented by: x
#> 
#>       [x] (x)    ind
#> 1 M+1.0SD   1  0.250
#> 2 Mean      0  0.100
#> 3 M-1.0SD  -1 -0.050
#> 
#>  - The 'ind' column shows the conditional effects.
#>  
#> 
#> ======================= Data Information =======================
#> 
#> Number of Replications:  2 
#> Sample Sizes:  50000 
#> 
#> ==== Descriptive Statistics ====
#> 
#>         vars     n mean sd  skew kurtosis se
#> y          1 1e+05 0.00  1  0.02     0.01  0
#> x          2 1e+05 0.00  1  0.01     0.01  0
#> w          3 1e+05 0.00  1  0.00    -0.02  0
#> x:w        4 1e+05 0.00  1 -0.03     6.01  0
#> control    5 1e+05 0.01  1  0.00     0.00  0
#> 
#> ===== Parameter Estimates Based on All 2 Samples Combined =====
#> 
#> Total Sample Size: 100000 
#> 
#> ==== Standardized Estimates ====
#> 
#> Variances and error variances omitted.
#> 
#> Regressions:
#>                     est.std
#>   y ~                      
#>     x                 0.097
#>     w                 0.104
#>     x:w               0.152
#>     control           0.099
#> 
#> Covariances:
#>                     est.std
#>   x ~~                     
#>     w                -0.003
#>     x:w               0.002
#>     control           0.101
#>   w ~~                     
#>     x:w              -0.004
#>     control           0.001
#>   x:w ~~                   
#>     control           0.004
#> 
#> 
#> ==================== Extra Element(s) Found ====================
#> 
#> - fit
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
#>   Number of observations                         50000
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
```

The population values for the regression paths are what we specified,
and the estimates based on 5 × 10⁴ by 2 or 100000 support that the
dataset were generated correctly.

NOTE: If a product term is involved, and the component terms (`x` and
`w` in this example) are correlated, the population standard deviation
of this product term may not be equal to one (Bohrnstedt & Goldberger,
1969). Therefore, the model can be specified correctly even if the
standard deviations of product terms in the section
`Descriptive Statistics` are not close to one.

### Call `power4test()` to Test The Moderation Effect

We can now do the simulation to estimate power. In this simple model,
the test is just a test of the product term, `x:w`. This model can be
fitted by linear regression using
[`lm()`](https://rdrr.io/r/stats/lm.html). Let’s estimate the power when
the sample size is 50 and the model is fitted by
[`lm()`](https://rdrr.io/r/stats/lm.html):

``` r
out2 <- power4test(nrep = 400,
                   model = mod2,
                   pop_es = mod2_es,
                   n = 100,
                   fit_model_args = list(fit_function = "lm"),
                   test_fun = test_moderation,
                   iseed = 1234,
                   parallel = TRUE)
```

These are the new arguments used:

- `fit_model_args`: This named list stores additional arguments for
  [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md).
  By default, [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)
  is used. To fit the model by linear regression using
  [`lm()`](https://rdrr.io/r/stats/lm.html), add `fit_function = "lm"`
  to the list.[⁶](#fn6)

- `test_fun`: It is set to `test_moderation`, provided by `power4mome`.
  This function automatically identifies all product terms in a model
  and test them. The test used depends on method used to fit the model.
  If [`lm()`](https://rdrr.io/r/stats/lm.html) is used, then the usual
  *t* test is used.[⁷](#fn7)

### Compute the Power

We can ues
[`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md)
again to estimate the power:

``` r
out2_power <- rejection_rates(out2)
out2_power
#> [test]: test_moderation: CIs  
#> [test_label]: y~x:w 
#>     est   p.v reject r.cilo r.cihi
#> 1 0.158 1.000  0.347  0.302  0.395
#> Notes:
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
```

The estimated power of the test of the product term, `x:w`, is 0.347,
with 95% confidence interval \[0.302; 0.395\].

## Moderated mediation

Let’s consider a moderated mediation model.

### Specify the Population Model and Values

``` r
mod3 <-
"
m ~ x + w + x:w
y ~ m + x
"
```

This model is a mediation model with the *a*-path, `m ~ x`, moderated by
`w`. As explained before, there is no need to use any label nor define
and parameters. This will be handled by the test function to be used.

This is the syntax for the population values:

``` r
mod3_es <-
"
.beta.: s
m ~ x: m
y ~ m: m
m ~ x:w: s
"
```

Please refer to [the previous section](#pop_es_xw) on setting up this
syntax.

### Call `power4test()` to Check the Model

We check the model first:

``` r
out3 <- power4test(nrep = 2,
                   model = mod3,
                   pop_es = mod3_es,
                   n = 50000,
                   iseed = 1234)
```

``` r
print(out3,
      data_long = TRUE)
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> 
#> m ~ x + w + x:w
#> y ~ m + x
#> 
#> == Model on Variables/Indicators ==
#> 
#> m ~ x + w + x:w
#> y ~ m + x
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.300  
#>     w                 0.100  
#>     x:w               0.050  
#>   y ~                        
#>     m                 0.300  
#>     x                 0.100  
#> 
#> Covariances:
#>                    Population
#>   x ~~                       
#>     w                 0.000  
#>     x:w               0.000  
#>   w ~~                       
#>     x:w               0.000  
#> 
#> Variances:
#>                    Population
#>    .m                 0.898  
#>    .y                 0.881  
#>     x                 1.000  
#>     w                 1.000  
#>     x:w               1.000  
#> 
#> (Computing indirect effects for 1 paths ...)
#> 
#> (Computing conditional effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Effect(s) ==
#> 
#>          ind
#> x -> y 0.100
#> 
#>  - The 'ind' column shows the effect(s).
#>  
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on moderator(s): w
#>  Moderator(s) represented by: w
#> 
#>       [w] (w)   ind   m~x   y~m
#> 1 M+1.0SD   1 0.105 0.350 0.300
#> 2 Mean      0 0.090 0.300 0.300
#> 3 M-1.0SD  -1 0.075 0.250 0.300
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - 'm~x','y~m' is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: w -> m -> y
#>  Conditional on moderator(s): x
#>  Moderator(s) represented by: x
#> 
#>       [x] (x)   ind   m~w   y~m
#> 1 M+1.0SD   1 0.045 0.150 0.300
#> 2 Mean      0 0.030 0.100 0.300
#> 3 M-1.0SD  -1 0.015 0.050 0.300
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - 'm~w','y~m' is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 
#> 
#> ======================= Data Information =======================
#> 
#> Number of Replications:  2 
#> Sample Sizes:  50000 
#> 
#> ==== Descriptive Statistics ====
#> 
#>     vars     n mean sd skew kurtosis se
#> m      1 1e+05    0  1 0.03     0.03  0
#> y      2 1e+05    0  1 0.01    -0.01  0
#> x      3 1e+05    0  1 0.00    -0.02  0
#> w      4 1e+05    0  1 0.00     0.01  0
#> x:w    5 1e+05    0  1 0.04     5.92  0
#> 
#> ===== Parameter Estimates Based on All 2 Samples Combined =====
#> 
#> Total Sample Size: 100000 
#> 
#> ==== Standardized Estimates ====
#> 
#> Variances and error variances omitted.
#> 
#> Regressions:
#>                     est.std
#>   m ~                      
#>     x                 0.303
#>     w                 0.099
#>     x:w               0.052
#>   y ~                      
#>     m                 0.299
#>     x                 0.098
#> 
#> Covariances:
#>                     est.std
#>   x ~~                     
#>     w                 0.003
#>     x:w              -0.001
#>   w ~~                     
#>     x:w               0.008
#> 
#> 
#> ==================== Extra Element(s) Found ====================
#> 
#> - fit
#> 
#> === Element(s) of the First Dataset ===
#> 
#> ============ <fit> ============
#> 
#> lavaan 0.6-20 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         7
#> 
#>   Number of observations                         50000
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.007
#>   Degrees of freedom                                 2
#>   P-value (Chi-square)                           0.997
```

The population values and the estimates based on 5 × 10⁴ by 2 or 100000
are what we expect.

### Call `power4test()` to Test The Moderated Mediation Effect

To estimate the power of a moderated mediation effect, we can test the
*index of moderated mediation* (Hayes, 2015). In this example, it is the
product of the coefficient `m ~ x:w` and the coefficient `y ~ m`. This
can be done by the test function
[`test_index_of_mome()`](https://sfcheung.github.io/power4mome/reference/test_index_of_mome.md),
provided by `power4mome`. Again, Monte Carlo confidence interval is
used.

Let’s estimate the power when sample size is 100.

``` r
out3 <- power4test(nrep = 400,
                   model = mod3,
                   pop_es = mod3_es,
                   n = 100,
                   R = 2000,
                   ci_type = "mc",
                   test_fun = test_index_of_mome,
                   test_args = list(x = "x",
                                    m = "m",
                                    y = "y",
                                    w = "w",
                                    mc_ci = TRUE),
                   iseed = 1234,
                   parallel = TRUE)
```

The call is similar to the one used in [testing mediation](#med_power).

This is the new argument used:

- `test_fun`: It is set to
  [`test_index_of_mome()`](https://sfcheung.github.io/power4mome/reference/test_index_of_mome.md)
  in this example. This function is similar to
  [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md),
  with one more argument, `w`, for the moderator. Although this example
  has only one mediator, it support any number of mediators along a
  path.[⁸](#fn8)

### Compute the Power

We can ues
[`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md)
again to estimate the power:

``` r
out3_power <- rejection_rates(out3)
out3_power
#> [test]: test_index_of_mome: x->m->y, moderated by w 
#> [test_label]: Test 
#>     est   p.v reject r.cilo r.cihi
#> 1 0.016 1.000  0.055  0.037  0.082
#> Notes:
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
```

The estimated power of the test of moderated mediation effect, conducted
by a test of the index of moderated mediation, is 0.055, 95% confidence
interval \[0.037; 0.082\].

Unlike the previous example on moderation tested by regression,
estimating the power of Monte Carlo confidence intervals is
substantially slower. However, this is necessary because Monte Carlo or
nonparametric bootstrapping confidence interval is the test usually used
in moderated mediation (and mediation).

## Repeating a Simulation With A Different Sample Size

The function
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
also supports *redoing* an analysis using a new value for the sample
size (or population effect sizes set to `pop_es`). Simply

- set the output of `power4test` as the first argument, and

- set the *new value* for `n`.

For example, we can repeat the simulation for the test of moderation
[above](#pop_es_xw), but for a sample size of 200. We simply call
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
again, set the previous output (`out2` in the example for moderation) as
the first argument, and set `n` to a new value (200 in this example):

``` r
out2_new_n <- power4test(out2,
                         n = 200)
out2_new_n
```

This is the estimated power when the sample size is 200.

``` r
out2_new_n_reject <- rejection_rates(out2_new_n)
out2_new_n_reject
#> [test]: test_moderation: CIs  
#> [test_label]: y~x:w 
#>     est   p.v reject r.cilo r.cihi
#> 1 0.148 1.000  0.527  0.479  0.576
#> Notes:
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
```

The estimated power is 0.527, 95% confidence interval \[0.479; 0.576\],
when the sample size is 200.

This technique can be repeated to find the required sample size for a
target power, and can be used for all the other scenarios covered above,
such as mediation and moderated mediation.

## Find the Sample Size With The Desired Power

There are several more efficient ways to find the sample size with the
desired power.

### Using `n_region_from_power()`

The function
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
can be used to find the *region* of sample sizes likely to have the
desired power. If the default settings are to be used, then it can be
called directly on the output of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md):

``` r
out2_region <- n_region_from_power(out2,
                                   seed = 2345)
```

This is the recommended way for sample size planning, when there is no
predetermined range of sample sizes.

See [the templates](https://sfcheung.github.io/power4mome/articles/) for
examples on using
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
for common models.

### Using `power4test_by_n()`

The function
[`power4test_by_n()`](https://sfcheung.github.io/power4mome/reference/power4test_by_n.md)
can be used To estimate the power for a sequence of sample sizes. For
example, we can estimate the power in the moderation model above for
these sample sizes: 250, 300, 350, 400.

``` r
out2_several_ns <- power4test_by_n(out2,
                                   n = c(250, 300, 350, 400),
                                   by_seed = 4567)
```

The first argument is the output of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
for an arbitrary sample size.

The argument `n` is a numeric vector of sample sizes to examine.

The argument `by_seed`, if set to an integer, try to make the results
reproducible.

The call will take some times to run because it is equivalent to calling
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
once for each sample size.

The rejection rates for each sample size can be retrieved by
[`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md)
too:

``` r
rejection_rates(out2_several_ns)
#> [test]: test_moderation: CIs  
#> [test_label]: y~x:w 
#>     n   est   p.v reject r.cilo r.cihi
#> 1 250 0.149 1.000  0.660  0.612  0.705
#> 2 300 0.150 1.000  0.733  0.687  0.774
#> 3 350 0.151 1.000  0.810  0.769  0.845
#> 4 400 0.150 1.000  0.850  0.812  0.882
#> Notes:
#> - n: The sample size in a trial.
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
```

The results show that, to have a power of about .800 to detect the
moderation effect, a sample size of about 350 is needed.

This approach is used when the range of sample sizes has already been
decided and the levels of power are needed to determine the final sample
size.

Please refer to the help page of
[`power4test_by_n()`](https://sfcheung.github.io/power4mome/reference/power4test_by_n.md)
for other examples.

### Using `x_from_power()`

The function
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
can be used to systematically search within an interval the sample size
with the target power. This takes longer to run but, instead of manually
trying different sample size, this function do the search automatically.

This approach can be used when the goal is to find the probable minimum
or maximum sample size with the desired level of power. The first
approach, using
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
simply uses this approach twice to find the region of sample sizes.

See [this
article](https://sfcheung.github.io/power4mome/articles/x_from_power_for_n.html)
for an illustration of how to use
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).

## Other Advanced Features

This brief illustration only covers the basic features of `power4mome`.
These are other advanced features to be covered in other articles:

- There is no inherent restriction on the form of the model. Typical
  models that can be specified in `lavaan` model syntax can be the
  population model, although there may be special models in which
  `power4test` does not yet support.

- The population model can be a model with latent factors and
  indicators. Nevertheless, users can specify only the relation among
  the factors. There is no need to include indicators in the model
  syntax, and also no need to manually specify the factor loadings. The
  number of indicators for each factor and the factor loadings are set
  by the argument `number_of_indicators` and `reliability` (see the help
  page of
  [`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md)
  on how to set them). The model syntax used to fit to the data will
  automatically include the indicators. An introduction can be found in
  [`vignette("power4test_latent_mediation")`](https://sfcheung.github.io/power4mome/articles/power4test_latent_mediation.md).
  Examples can be found in
  [templates](https://sfcheung.github.io/power4mome/articles/) for
  models with latent variables.

- Though not illustrated above, estimating the power of tests conducted
  by nonparametric bootstrapping is supported, although it will take
  longer to run.

- Although this package focuses on moderation, mediation, and moderated
  mediation, in principle, the power of any test can be estimated, as
  long as a test function for `test_fun` is available. Some other
  functions are provided with `power4mome` (e.g.,
  [`test_parameters()`](https://sfcheung.github.io/power4mome/reference/test_parameters.md)
  for testing all free model parameters). See the help page of
  [`do_test()`](https://sfcheung.github.io/power4mome/reference/do_test.md)
  on how to write a function to do a test not available in `power4mome`.

- When estimating power, usually the population model is fitted to the
  data. However, it is possible to fit any other model to the generated
  data. This can be done by using the argument `fit_model_args` to set
  the argument `model` of
  [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md).

- Preliminary support for multigroup model is available. See the help
  pages of
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md)
  and
  [`pop_es_yaml()`](https://sfcheung.github.io/power4mome/reference/pop_es_yaml.md)
  on how to specify the population value syntax. Functions will be added
  for tests relevant to multigroup models (e.g., testing the
  between-group difference in an indirect effect).

- Although we illustrated only rerunning an analysis with a new sample
  size (`n`), it is also possible to rerun an analysis using a new
  population value for a parameter. This can be done by using the
  previous output of
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  as the first argument, and setting only `pop_es` to a named vector:

``` r
out2_new_xw <- power4test(out2,
                          pop_es = c("y ~ x:w" = ".30"))
```

- Basic support for generating nonnormal variables, including
  dichotomous variables is available. See the argument `x_fun` of
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  for details.

## Limitations

- Monte Carlo confidence interval is not supported for models fitted by
  [`lm()`](https://rdrr.io/r/stats/lm.html) (regression). To estimate
  power of testing mediation or moderated mediation effects in models
  fitted by [`lm()`](https://rdrr.io/r/stats/lm.html),
  `ci_type = "boot"` is needed.

## References

Bohrnstedt, G. W., & Goldberger, A. S. (1969). On the exact covariance
of products of random variables. *Journal of the American Statistical
Association*, *64*(328), 1439–1442. <https://doi.org/10.2307/2286081>

Cheung, S. F., & Cheung, S.-H. (2024). *Manymome*: An R package for
computing the indirect effects, conditional effects, and conditional
indirect effects, standardized or unstandardized, and their bootstrap
confidence intervals, in many (though not all) models. *Behavior
Research Methods*, *56*(5), 4862–4882.
<https://doi.org/10.3758/s13428-023-02224-z>

Hayes, A. F. (2015). An index and test of linear moderated mediation.
*Multivariate Behavioral Research*, *50*(1), 1–22.
<https://doi.org/10.1080/00273171.2014.962683>

------------------------------------------------------------------------

1.  Users can specify the values directly if necessary.

2.  The functions used are
    [`parallel::clusterSetRNGStream()`](https://rdrr.io/r/parallel/RngStream.html)
    for parallel processing, and
    [`set.seed()`](https://rdrr.io/r/base/Random.html) for serial
    processing.

3.  They are implemented by
    [`manymome::do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.html)
    and
    [`manymome::do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.html),
    respectively.

4.  The test is implemented by `manymome::indirect()`.

5.  The number of cores is determined automatically but can be set
    directly by the `ncores` argument.

6.  See the help page of
    [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
    on other available arguments.

7.  The test name has `CIs` in it but this is equivalent to using the
    *t* test when the model is fitted by
    [`lm()`](https://rdrr.io/r/stats/lm.html).

8.  The test is implemented by `manymome::test_index_of_mome()`.
