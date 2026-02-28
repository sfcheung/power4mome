# Sample Size Given Desired Power (Probabilistic Bisection)

## Introduction

This article is a brief illustration of how to use
[`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
and
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
from the package [power4mome](https://sfcheung.github.io/power4mome/) to
find by simulation the sample size with power close to a desired level
to detect an effect, given the level of population effect size.

The illustration will use an indirect effect tested by Monte Carlo
confidence interval as an example, though the procedure is similar for
other tests supported by
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

NOTE: This is one version of two nearly-identical articles. This version
introduces how to use the probabilistic bisection algorithm (Waeber et
al., 2013), which sometimes takes longer to run but can be used for a
higher level of precision using a larger number of replications. The
[other
version](https://sfcheung.github.io/power4mome/articles/x_from_power_for_n.md)
uses the (informal) bisection algorithm (see Chalmers, 2024 for a review
of this method) to identify the approximate sample size with a limited
number of replications.

## Prerequisite

Basic knowledge about fitting models by `lavaan` is required. Readers
are also expected to know how to use
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
(see this
[get-started](https://sfcheung.github.io/power4mome/articles/power4mome.md)
for an introduction).

## Scope

This is a brief illustration. More complicated scenarios and other
features of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
will be described in other vignettes and articles.

Some sections are repeated from other vignettes and articles to make
this vignette self-contained.

## Package

This illustration needs the following package(s):

``` r
library(power4mome)
```

## Workflow

Three functions, along with some methods, are sufficient for estimating
the sample size, given the desired power, along with other factors such
as the test, the model, and population values. This is the workflow:

1.  Specify the model syntax for the population model, in `lavaan`
    style, and set the population values of the model parameters.

2.  Call
    [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
    to examine the setup and the datasets generated. Repeat this and
    previous steps until the model is specified correctly.

3.  Call
    [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
    again, with the test to do specified, using an initial sample size.

4.  Call
    [`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md)
    to compute the power and verify that the test is specified
    correctly.

5.  Call
    [`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
    to estimate the sample size required for the desired power.

## Mediation

Let’s consider a simple mediation model. We would like to estimate the
sample size required to detect a mediation effect by Monte Carlo
confidence interval, with 95% level of significance.

Because sampling (simulation) error is involved when there is no
analytic solution to find the sample size, instead of estimating the
unknown sample size, we can also estimate, approximately, the region of
sample sizes with their levels of power

1.  significantly *lower* than the target power, or

2.  significantly *higher* than the target power.

That is, we find the sample size that has its *confidence interval*, 95%
by default, just below (Case 1) or just above (Case 2) the target power,
approximately.

We will consider Case 1 first.

### Specify the Population Model

This is the model syntax

``` r
mod <-
"
m ~ x
y ~ m + x
"
```

Note that, even if we are going to test mediation, we do not need to
label the parameters and define the indirect effect as usual in
`lavaan`. This will be taken care of by the test functions, through the
use of the package [`manymome`](https://sfcheung.github.io/manymome/)
(Cheung & Cheung, 2024).

### Specify The Population Values

There are two approaches to do this: using named vectors or lists, or
using a multiline string similar to `lavaan` model syntax. The second
approach is demonstrated below.

Suppose we want to estimate the power when:

- The path from `x` to `m` are “large” in strength.

- The path from `m` to `y` are “medium” in strength.

- The path from `x` to `m` are “small” in strength.

By default, `power4mome` use this convention for regression path and
correlation:[¹](#fn1)

- Small: .10 (or -.10)

- Medium: .30 (or -.30)

- Large: .50 (or -.50)

All these values are for the standardized solution (the so-called
“betas”).

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

After the colon is a population value, which can be:

- A word denoting the value. By default:

  - `s`: Small. (`-s` for small and negative.)

  - `m`: Medium. (`-m` for medium and negative.)

  - `l`: Large. (`-l` for large and negative.)

  - `nil`: Zero.

All regression coefficients and covariances, if not specified, are set
to zero.

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
  more replications.

- `model`: The model syntax.

- `pop_es`: The string setting the population values.

- `n`: The sample size in each replications. In this stage, just for
  checking the model and the data generation, this number can be set to
  a large one unless the model is slow to fit when the sample size is
  large.

- `iseed`: If supplied, it is used to set the seed for the random number
  generator. It is advised to always set this to an arbitrary integer,
  to make the results reproducible.[²](#fn2)

The population values can be shown by print this object:

``` r
print(out,
      data_long = TRUE)
#>
#> ============================== Model Information ==============================
#>
#> ====== Model on Factors/Variables ======
#>
#> m ~ x
#> y ~ m + x
#>
#> ==== Model on Variables/Indicators ====
#>
#> m ~ x
#> y ~ m + x
#>
#> ========== Population Values ==========
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
#> =============================== Data Information ===============================
#>
#> Number of Replications:  2
#> Sample Sizes:  50000
#>
#> ======== Descriptive Statistics ========
#>
#>   vars     n mean sd skew kurtosis se
#> m    1 1e+05 0.00  1 0.01     0.03  0
#> y    2 1e+05 0.01  1 0.01     0.00  0
#> x    3 1e+05 0.00  1 0.01     0.01  0
#>
#> ============= Parameter Estimates Based on All 2 Samples Combined =============
#>
#> Total Sample Size: 100000
#>
#> ======== Standardized Estimates ========
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
#> ============================ Extra Element(s) Found ============================
#>
#> - fit
#>
#> ======== Element(s) of the First Dataset ========
#>
#> ================ <fit> ================
#>
#> lavaan 0.6-21 ended normally after 1 iteration
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

The argument `data_long = TRUE` is used to verify the simulation.

The population values for the regression paths in the section
`Population Values` are what we specified. So the model is specified
correctly.

The section `Descriptive Statistics`, generated by
[`psych::describe()`](https://rdrr.io/pkg/psych/man/describe.html),
shows basic descriptive statistics for the observed variables. As
expected, they have means close to zero and standard deviations close to
one, because the datasets were generated using the standardized model.

The section `Parameter Estimates Based on` shows the parameter estimates
when the population model is fitted to all the datasets combined. When
the total sample size is large, these estimates should be close to the
population values.

By the default, the population model will be fitted to each dataset,
hence the section `<fit>`. This section just verifies that the
population can be fitted

The results show that population model is the desired one. We can
proceed to the next stage

### Call `power4test()` to Do the Target Test

We can now do the simulation to estimate power for an initial sample
size, to verify the test we want to do. A large number of datasets
(e.g., 500) of the target sample size are to be generated, and then the
target test will be conducted in each of these datasets.

Suppose we would like to estimate the power of using Monte Carlo
confidence interval to test the indirect effect from `x` to `y` through
`m`, when sample size is 50. This is the call, based on the previous
one:

``` r
out <- power4test(nrep = 50,
                  model = mod,
                  pop_es = mod_es,
                  n = 50,
                  R = 199,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 2345,
                  parallel = TRUE)
```

If our goal is to find a sample size for a specific level of power, with
sufficient precision, we do not need a large number of replications
(`nrep`) in this stage. We can use as few as 50. We can set the target
number of replications when calling the function
[`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
which is a wrapper to the general function
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).

These are the new arguments used:

- `R`: The number of replications used to generate the Monte Carlo
  simulated estimates, 199 in this example. This may look small, but
  this value is acceptable because the goal is not to have a stable
  result in *one* replication, but to estimate the power *across*
  replications. The method proposed by Boos & Zhang (2000), detailed
  below, will be used to estimate the power.

- `ci_type`: The method used to generate estimates. Support Monte Carlo
  (`"mc"`) and nonparametric bootstrapping (`"boot"`).[³](#fn3) Although
  bootstrapping is usually used to test an indirect effect, it is very
  slow to do `R` bootstrapping in `nrep` datasets (the model will be
  fitted `R * nrep` times). Therefore, it is preferable to use Monte
  Carlo to do the initial estimation.

- `test_fun`: The function to be used to do the test for each
  replication. Any function following a specific requirement can be
  used, and `power4mome` comes with several built-in function for some
  tests. The function
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
  test done by Monte Carlo or nonparametric bootstrapping confidence
  interval, it is advised to enable parallel processing by setting
  `parallel` to `TRUE`.[⁵](#fn5)

In power analysis involving resampling methods such as Monte Carlo
confidence interval and bootstrapping, the processing time can be very
long. Boos & Zhang (2000) proposed a method to extrapolate the rejection
rates of these methods using a limited number of (re)sampling. This
method requires specific numbers of `R`, determined by the level of
significance (1 minus the level of confidence, for confidence interval
methods). For a level of significance of .05, the default, these are the
first few supported values of `R`:

    #> [1]  39  79 119 159 199 239 279 319 359

In our experience, an `R` of 199 is large enough for the search.

Note that the simulation can take some time to run. Progress will be
printed when run in an interactive session.

This is the default printout:

``` r
print(out)
#>
#> ============================== Model Information ==============================
#>
#> ====== Model on Factors/Variables ======
#>
#> m ~ x
#> y ~ m + x
#>
#> ==== Model on Variables/Indicators ====
#>
#> m ~ x
#> y ~ m + x
#>
#> ========== Population Values ==========
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
#> =============================== Data Information ===============================
#>
#> Number of Replications:  50
#> Sample Sizes:  50
#>
#> Call print with 'data_long = TRUE' for further information.
#>
#> ============================ Extra Element(s) Found ============================
#>
#> - fit
#> - mc_out
#>
#> ======== Element(s) of the First Dataset ========
#>
#> ================ <fit> ================
#>
#> lavaan 0.6-21 ended normally after 1 iteration
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
#> =============== <mc_out> ===============
#>
#>
#> == A 'mc_out' class object ==
#>
#> Number of Monte Carlo replications: 199
#>
#>
#> ====================== <test_indirect: x->m->y> ======================
#>
#> Mean(s) across replication:
#>    est cilo cihi   sig pvalue       R   nlt0 alpha bz_39 bz_79 bz_119 bz_159 bz_199
#>  0.167  NaN  NaN 0.548  0.129 199.000 16.860 0.050 0.496 0.523  0.528  0.536  0.540
#>
#> - The value 'sig' is the rejection rate.
#> - If the null hypothesis is false, this is the power.
#> - Number of valid replications for rejection rate: 50
#> - Proportion of valid replications for rejection rate: 1.000
```

As shown above, the settings are correct. The columns after `R` (e.g.,
`nlt0` and `bz_39`) are for the method by Boos & Zhang (2000). The
presence of these columns confirms that the method by Boos & Zhang
(2000) is used.

We can now call
[`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
to do the search.

### Call `n_from_power()` to Estimate the Sample Size

This is a simplified description of how
[`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
works when our goal is to find a sample size with its power *close to*
the target power, with a desired level of precision determined by the
number of replications (`nrep`).

The probabilistic bisection algorithm (Waeber et al., 2013) will be
used. It takes some time to run, sometimes ten minutes or more, but it
is particularly suitable for determining the sample size given a target
power with a high level of precision, which requires a large number of
replications. See Chalmers (2024) for an illustration of this method in
power analysis.

This is the function call:

``` r
out_n <- n_from_power(out,
                      target_power = .80,
                      final_nrep = 2000,
                      final_R = 2000,
                      seed = 1234,
                      algorithm = "probabilistic_bisection")
```

The argument used above:

- `target_power`: The target power. Default is .80.

- `final_nrep`: The number of replications desired in the solution. For
  `nrep = 2000`, the width of the 95% confidence limits based on normal
  approximation for a power of .80 is about .035. This should be precise
  enough for estimating the sample size required. Increase or decrease
  `nrep` to get the desired level of precision.

- `final_R`: It is preferable to use the method by Boos & Zhang (2000)
  with a small `R` during the search. Nevertheless, if a larger number
  of `R` is desired during the final check, we can set `final_R` to this
  value. If omitted, the `R` in the object from
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  will be used.

- `seed`: To make the search reproducible, if possible, set this seed to
  an integer.

By default, the progress of the search will be displayed on screen (add
`progress = FALSE` to hide the progress). Each iteration only uses a
small number of replications (`nrep`), but the information will be
accumulated across iterations to determine the sample size. When a
potential solution (the sample size with the desired power) is found, a
final check will be conducted using `final_nrep` replications and
`final_R` (if set). If the estimated power of this solution is “close
enough” to the target power, the search will stop. If not, the search
will continue. These steps will be continued until some termination
criteria are met. (Details to be described in another article).

#### WHat if the Search Fails

The search is probabilistic. Therefore, it is possible that a search may
fail given the termination criteria (such as the maximum number of
iterations). If a search fails, try another value for `seed`.

### Examine the Output

The example above will take several minutes to run, depending on factors
such as whether `parallel` is `TRUE` and the number of CPU cores used
when calling
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
as well as the target number of replications (`final_nrep`). The larger
the `final_nrep` is, the higher the precision of the solution, but the
longer the search.

This is the basic output:

``` r
out_n
#> Call:
#> power4mome::x_from_power(object = out, x = "n", target_power = 0.8,
#>     what = "point", goal = "ci_hit", final_nrep = 2000, final_R = 2000,
#>     seed = 1234, algorithm = "probabilistic_bisection")
#>
#>                                      Setting
#> Predictor(x):                    Sample Size
#> Parameter:                               N/A
#> goal:                           close_enough
#> what:                                  point
#> algorithm:           probabilistic_bisection
#> Level of confidence:                  95.00%
#> Target Power:                          0.800
#>
#> - Final Value of Sample Size (n): 101
#>
#> - Final Estimated Power (CI): 0.785 [0.766, 0.802]
#>
#> Call `summary()` for detailed results.
```

The estimated sample size is 101. The estimated power based on
simulation is 0.785, with its confidence interval equal to \[0.766,
0.802\].

To obtain a more detailed results for the search, we can use the
[`summary()`](https://rdrr.io/r/base/summary.html) method:

``` r
summary(out_n)
#>
#> ====== x_from_power Results ======
#>
#> Call:
#> x_from_power(object = out, x = "n", target_power = 0.8, what = "point",
#>     goal = "ci_hit", final_nrep = 2000, final_R = 2000, seed = 1234,
#>     algorithm = "probabilistic_bisection")
#>
#> Predictor (x): Sample Size
#>
#> - Target Power: 0.800
#> - Goal: Find 'x' with estimated estimated power close enough to the target power.
#>
#> === Major Results ===
#>
#> - Final Value (Sample Size): 101
#>
#> - Final Estimated Power: 0.785
#> - Confidence Interval: [0.766; 0.802]
#> - Level of confidence: 95.0%
#> - Based on 2000 replications.
#>
#> === Technical Information ===
#>
#> - Algorithm: probabilistic_bisection
#> - Tolerance for 'close enough': Within 0.01525 of 0.800
#> - The range of values explored: 50 to 150
#> - Time spent in the search: 5.746 mins
#> - The final crude model for the power-predictor relation:
#>
#> Model Type: Logistic Regression
#>
#> Call:
#> power_curve(object = by_x_1, formula = power_model, start = power_curve_start,
#>     lower_bound = lower_bound, upper_bound = upper_bound, nls_args = nls_args,
#>     nls_control = nls_control, verbose = progress)
#>
#> Predictor: n (Sample Size)
#>
#> Model:
#>
#> Call:  stats::glm(formula = reject ~ x, family = "binomial", data = reject1)
#>
#> Coefficients:
#> (Intercept)            x
#>    -1.09778      0.02399
#>
#> Degrees of Freedom: 7415 Total (i.e. Null);  7414 Residual
#> Null Deviance:       7763
#> Residual Deviance: 7706  AIC: 7710
#>
#> - Detailed Results:
#>
#> [test]: test_indirect: x->m->y
#> [test_label]: Test
#>      n   est   p.v reject r.cilo r.cihi
#> 1   50 0.167 1.000  0.548  0.404  0.670
#> 2   50 0.150 1.000  0.502  0.377  0.641
#> 3   73 0.138 1.000  0.664  0.530  0.780
#> 4   94 0.134 1.000  0.719  0.591  0.829
#> 5   94 0.149 1.000  0.771  0.632  0.860
#> 6   94 0.143 1.000  0.697  0.570  0.813
#> 7   95 0.167 1.000  0.843  0.720  0.918
#> 8   95 0.154 1.000  0.788  0.654  0.875
#> 9   95 0.159 1.000  0.783  0.654  0.875
#> 10  95 0.158 1.000  0.787  0.654  0.875
#> 11  96 0.157 1.000  0.816  0.697  0.904
#> 12  96 0.131 1.000  0.646  0.510  0.764
#> 13  97 0.160 1.000  0.825  0.697  0.904
#> 14  97 0.143 1.000  0.778  0.654  0.875
#> 15  97 0.153 1.000  0.799  0.675  0.890
#> 16  97 0.140 1.000  0.676  0.530  0.780
#> 17  97 0.152 1.000  0.777  0.654  0.875
#> 18  97 0.145 1.000  0.694  0.550  0.797
#> 19  97 0.148 1.000  0.768  0.750  0.786
#> 20  97 0.149 1.000  0.825  0.697  0.904
#> 21  97 0.160 1.000  0.867  0.743  0.932
#> 22  97 0.145 1.000  0.868  0.743  0.932
#> 23  97 0.145 1.000  0.794  0.654  0.875
#> 24  97 0.132 1.000  0.664  0.530  0.780
#> 25  97 0.131 1.000  0.758  0.632  0.860
#> 26  98 0.163 1.000  0.878  0.766  0.945
#> 27 100 0.148 1.000  0.781  0.654  0.875
#> 28 100 0.155 1.000  0.863  0.743  0.932
#> 29 101 0.139 1.000  0.743  0.611  0.845
#> 30 101 0.143 1.000  0.751  0.611  0.845
#> 31 101 0.146 1.000  0.793  0.654  0.875
#> 32 101 0.135 1.000  0.805  0.675  0.890
#> 33 101 0.155 1.000  0.782  0.654  0.875
#> 34 101 0.149 1.000  0.726  0.591  0.829
#> 35 101 0.152 1.000  0.830  0.697  0.904
#> 36 101 0.140 1.000  0.675  0.530  0.780
#> 37 101 0.143 1.000  0.755  0.632  0.860
#> 38 101 0.149 1.000  0.785  0.766  0.802
#> 39 102 0.155 1.000  0.802  0.675  0.890
#> 40 102 0.160 1.000  0.939  0.841  0.980
#> 41 102 0.156 1.000  0.854  0.743  0.932
#> 42 103 0.170 1.000  0.861  0.743  0.932
#> 43 104 0.135 1.000  0.700  0.570  0.813
#> 44 104 0.141 1.000  0.743  0.611  0.845
#> 45 104 0.163 1.000  0.872  0.743  0.932
#> 46 104 0.149 1.000  0.833  0.697  0.904
#> 47 104 0.152 1.000  0.785  0.654  0.875
#> 48 104 0.153 1.000  0.814  0.675  0.890
#> 49 104 0.147 1.000  0.795  0.675  0.890
#> 50 104 0.148 1.000  0.714  0.570  0.813
#> 51 104 0.141 1.000  0.824  0.697  0.904
#> 52 104 0.138 1.000  0.765  0.632  0.860
#> 53 104 0.166 1.000  0.865  0.743  0.932
#> 54 104 0.158 1.000  0.762  0.632  0.860
#> 55 104 0.142 1.000  0.779  0.654  0.875
#> 56 104 0.145 1.000  0.808  0.675  0.890
#> 57 104 0.154 1.000  0.795  0.675  0.890
#> 58 104 0.157 1.000  0.919  0.815  0.969
#> 59 104 0.153 1.000  0.845  0.720  0.918
#> 60 104 0.177 1.000  0.840  0.720  0.918
#> 61 105 0.152 1.000  0.833  0.697  0.904
#> 62 105 0.154 1.000  0.916  0.815  0.969
#> 63 107 0.145 1.000  0.797  0.675  0.890
#> 64 107 0.150 1.000  0.865  0.743  0.932
#> 65 112 0.147 1.000  0.826  0.697  0.904
#> 66 115 0.157 1.000  0.934  0.841  0.980
#> 67 121 0.153 1.000  0.770  0.632  0.860
#> 68 127 0.142 1.000  0.881  0.766  0.945
#> 69 150 0.150 1.000  0.897  0.790  0.957
#> Notes:
#> - n: The sample size in a trial.
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the rejection rate. If
#>   the null hypothesis is true, this is the Type I error rate. If the null hypothesis is
#>   false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based on Wilson's (1927)
#>   method.
#> - Refer to the tests for the meanings of other columns.
```

It also reports major technical information regarding the search, such
as the range of sample sizes tried, the time spent, and the table with
all the sample sizes examined, along with the estimated power levels and
confidence intervals.

Note that it is normal for the probabilistic bisection method to try the
same value (same sample size) several times. Each trial is independent
of other trials, and provides unique information to the search.

It also prints the model, the “power curve”, used to estimate the
relation between the power and the sample size. Note that this is only a
crude model intended only for the values of sample size examined (69 in
this example). It is not intended to estimate power for sample sizes
outside the range studied.

Unlike the (informal) bisection method, the final solution is usually
validated by a large number of replications (`final_nrep`) when
probabilistic bisection is used. Therefore, it is not necessary to use
the search process or the power curve to evaluate the final solution.

## Advanced Features

This brief illustration only covers basic features of
[`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).
There are other ways to customize the search, such as the range of
sample sizes to examine, the level of confidence for the confidence
interval, and the termination criteria (e.g., the maximum number of
trials, 100 by default for probabilistic bisection). Please refer to the
help page of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
which
[`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
calls, for these and other options.

## References

Boos, D. D., & Zhang, J. (2000). Monte Carlo evaluation of
resampling-based hypothesis tests. *Journal of the American Statistical
Association*, *95*(450), 486–492.
<https://doi.org/10.1080/01621459.2000.10474226>

Chalmers, R. P. (2024). Solving variables with Monte Carlo simulation
experiments: A stochastic root-solving approach. *Psychological
Methods*. <https://doi.org/10.1037/met0000689>

Cheung, S. F., & Cheung, S.-H. (2024). *manymome*: An R package for
computing the indirect effects, conditional effects, and conditional
indirect effects, standardized or unstandardized, and their bootstrap
confidence intervals, in many (though not all) models. *Behavior
Research Methods*, *56*(5), 4862–4882.
<https://doi.org/10.3758/s13428-023-02224-z>

Waeber, R., Frazier, P. I., & Henderson, S. G. (2013). Bisection search
with noisy responses. *SIAM Journal on Control and Optimization*,
*51*(3), 2261–2279. <https://doi.org/10.1137/120861898>

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
