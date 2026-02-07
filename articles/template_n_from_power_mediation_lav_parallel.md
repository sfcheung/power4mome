# Quick Template: Parallel Mediation with Latent Variables

## Introduction

This and other “Quick Template” articles are examples of R code to
determine the range of sample sizes for a target level of power in
typical models using
[power4mome](https://sfcheung.github.io/power4mome/). Users can quickly
adapt them for their scenarios. A summary of the code examples can be
found in the section [Code Template](#code_template) at the end of this
document.

## Prerequisite

Basic knowledge about fitting models by `lavaan` and `power4mome` is
required.

This file is not intended to be an introduction on how to use functions
in `power4mome`. For details on how to use
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
refer to the [Get-Started
article](https://sfcheung.github.io/power4mome/articles/power4mome.html).
Please also refer to the help page of
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
and the
[article](https://sfcheung.github.io/power4mome/articles/x_from_power_for_n.html)
on
[`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
which is called twice by
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
to find the regions described below.

## Scope

This file is for parallel mediation models with latent variables.

## Functions Used in This Template

- [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)

  - Set up the model and the population values, generate the data, and
    generate the Monte Carlo simulated estimates for Monte Carlo
    confidence interval.

- [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)

  - Find the regions of sample sizes based on the target power.

- [`test_k_indirect_effects()`](https://sfcheung.github.io/power4mome/reference/test_k_indirect_effects.md)

  - Test several indirect effects as one test, using Monte Carlo or
    bootstrap confidence intervals. Used by
    [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

## Common Flow

The following chart summarizes the steps covered below.

Common Workflow

In practice, steps can be repeated, and population values changed, until
the desired goal is achieved (e.g., the region of sample sizes with
power close to the target power is found).

## Set Up The Model and Test

Load the package first:

``` r
library(power4mome)
```

Estimate the power for a sample size.

A parallel mediation model has several parallel paths (three in the
following example). The test function
[`test_k_indirect_effects()`](https://sfcheung.github.io/power4mome/reference/test_k_indirect_effects.md)
can be used to estimate the power for having all selected indirect paths
significant if `omnibus` is set to `"all_sig"`.

The code:

``` r
# ====== Model: Form ======

model <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
"

# ====== Model: Population Values ======
# l: large (.50 by default)
# m: medium (.30 by default)
# s: small (.10 by default)
# n: nil (.00 by default)
# -l, -m, and -s denote negative values
# Omitted paths are zero by default
# Can also set to a number directly
# Set each path to the hypothesized magnitude

model_es <-
"
m1 ~ x: m
m2 ~ x: m
y ~ m1: l
y ~ m2: l
y ~ x: s
"
```

![The Model](template_n_from_power_mediation_lav_parallel_model-1.png)

The Model

``` r

# ====== Test the Model Specification ======

out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 50000,
                  iseed = 1234)

# ====== Check the Data Generated ======

print(out,
      data_long = TRUE)

# ====== Estimate the Power ======

# For n = 150,
# when testing all indirect effects by
# Monte Carlo confidence intervals

out <- power4test(nrep = 600,
                  model = model,
                  pop_es = model_es,
                  n = 150,
                  R = 1000,
                  number_of_indicators = c(x = 3,
                                           m1 = 4,
                                           m2 = 3,
                                           y = 3),
                  reliability = c(x = .80,
                                  m1 = .70,
                                  m2 = .70,
                                  y = .80),
                  ci_type = "mc",
                  test_fun = test_k_indirect_effects,
                  test_args = list(x = "x",
                                   y = "y",
                                   mc_ci = TRUE,
                                   omnibus = "all_sig"),
                  iseed = 1234,
                  parallel = TRUE)

# ====== Compute the Rejection Rate ======

rejection_rates(out)
```

The results:

``` r
print(out,
      data_long = TRUE)
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> 
#> m1 ~ x
#> m2 ~ x
#> y ~ m1 + m2 + x
#> 
#> == Model on Variables/Indicators ==
#> 
#> m1 ~ x
#> m2 ~ x
#> y ~ m1 + m2 + x
#> 
#> x =~ x1 + x2 + x3
#> m1 =~ m11 + m12 + m13 + m14
#> m2 =~ m21 + m22 + m23
#> y =~ y1 + y2 + y3
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m1 ~                       
#>     x                 0.300  
#>   m2 ~                       
#>     x                 0.300  
#>   y ~                        
#>     m1                0.500  
#>     m2                0.500  
#>     x                 0.100  
#> 
#> Variances:
#>                    Population
#>    .m1                0.910  
#>    .m2                0.910  
#>    .y                 0.385  
#>     x                 1.000  
#> 
#> (Computing indirect effects for 3 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>                ind
#> x -> m1 -> y 0.150
#> x -> m2 -> y 0.150
#> x -> y       0.100
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ==== Population Reliability ====
#> 
#>    x  m1  m2   y
#>  0.8 0.7 0.7 0.8
#> 
#> == Population Standardized Loadings ==
#> 
#>      x    m1    m2     y
#>  0.756 0.607 0.661 0.756
#> ======================= Data Information =======================
#> 
#> Number of Replications:  600 
#> Sample Sizes:  150 
#> 
#> ==== Descriptive Statistics ====
#> 
#>     vars     n mean   sd  skew kurtosis se
#> x1     1 90000    0 1.00  0.00    -0.02  0
#> x2     2 90000    0 1.00  0.01     0.01  0
#> x3     3 90000    0 1.00  0.01     0.00  0
#> m11    4 90000    0 1.00  0.00    -0.01  0
#> m12    5 90000    0 1.00  0.00     0.00  0
#> m13    6 90000    0 1.00 -0.01     0.00  0
#> m14    7 90000    0 1.00 -0.01    -0.01  0
#> m21    8 90000    0 1.00  0.00    -0.01  0
#> m22    9 90000    0 1.01 -0.01     0.00  0
#> m23   10 90000    0 1.00  0.00     0.03  0
#> y1    11 90000    0 1.00  0.00     0.01  0
#> y2    12 90000    0 1.00  0.00     0.03  0
#> y3    13 90000    0 1.00  0.00     0.02  0
#> 
#> ==== Parameter Estimates Based on All 600 Samples Combined ====
#> 
#> Total Sample Size: 90000 
#> 
#> ==== Standardized Estimates ====
#> 
#> Variances and error variances omitted.
#> 
#> Latent Variables:
#>                     est.std
#>   x =~                     
#>     x1                0.753
#>     x2                0.754
#>     x3                0.761
#>   m1 =~                    
#>     m11               0.605
#>     m12               0.612
#>     m13               0.606
#>     m14               0.605
#>   m2 =~                    
#>     m21               0.662
#>     m22               0.665
#>     m23               0.661
#>   y =~                     
#>     y1                0.756
#>     y2                0.753
#>     y3                0.754
#> 
#> Regressions:
#>                     est.std
#>   m1 ~                     
#>     x                 0.300
#>   m2 ~                     
#>     x                 0.303
#>   y ~                      
#>     m1                0.506
#>     m2                0.491
#>     x                 0.105
#> 
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
#> lavaan 0.6-21 ended normally after 33 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        31
#> 
#>   Number of observations                           150
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                54.181
#>   Degrees of freedom                                60
#>   P-value (Chi-square)                           0.687
#> 
#> =========== <mc_out> ===========
#> 
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 1000 
#> 
#> 
#> ============== <test_indirects: x-...->y> ==============
#> 
#> Mean(s) across replication:
#>           test_label  est cilo cihi pvalue   sig
#> 1 x-...->y (All sig)  NaN  NaN  NaN  0.047 0.620
#> 
#> - The column 'sig' shows the rejection rates.
#> - If the null hypothesis is false, the rate is the power.
#> - Number of valid replications for rejection rate(s): 600 
#> - Proportion of valid replications for rejection rate(s): 1.000
rejection_rates(out)
#> [test]: test_indirects: x-...->y 
#> [test_label]: x-...->y (All sig) 
#>    est   p.v reject r.cilo r.cihi
#> 1  NaN 1.000  0.620  0.581  0.658
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

(`est` is `NaN` because the test is for all three paths and so there is
no single point estimate for the test.)

## Find the Regions of *N* Based on the Target Power

Search, by simulation, the following two regions of sample sizes:

- Sample sizes with estimated levels of power significantly below the
  target level (e.g., .80), tested by the confidence interval (95% by
  default).

- Sample sizes with estimated levels of power significantly above the
  target level (e.g., .80), tested by the confidence interval (95% by
  default).

In practice, we rarely need high precision for these regions for sample
size planning. Therefore, we only need to find the two sample sizes with
the corresponding confidence bounds *close* *enough* to the target
power, defined by a tolerance value. In the function below, this value
is .02 by default.

It can take some time to run if the estimated power of the sample size
is too different from the target power.

We can find the two regions by
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).

The code:

``` r
#
# ===== Reuse the output of power4test() =====
#
# Call n_region_from_power()
# - Set target power: target_power = .80 (Default, can be omitted)
# - Set the seed for the simulation: Integer. Should always be set.
# To set desired precision:
# - Set final number of R: final_R. If omitted,
#   it is set to 1000 or set to R in the original object.
# - Set final number of replications: final_nrep. If omitted,
#   it is set to 400 or set to nrep in the original object.

n_power_region <- n_region_from_power(out,
                                      seed = 1357)

# ===== Basic Results =====

n_power_region

# ===== Plot the (Crude) Power Curve and the Regions =====

plot(n_power_region)
```

The results:

``` r
# ===== Basic Results =====

n_power_region
#> Call:
#> n_region_from_power(object = out, seed = 1357)
#> 
#>                      Setting                                      
#> Predictor(x)         Sample Size                                  
#> Goal:                Power significantly below or above the target
#> algorithm:           bisection                                    
#> Level of confidence: 95.00%                                       
#> Target Power:        0.800                                        
#> 
#> Solution: 
#> 
#> Approximate region of sample sizes with power:
#> - not significantly different from 0.800: 194 to 212
#> - significantly lower than 0.800: 194
#> - significantly higher than 0.800: 212
#> 
#> Confidence intervals of the estimated power:
#> - for the lower bound (194): [0.733, 0.800]
#> - for the upper bound (212): [0.807, 0.866]
#> 
#> Call `summary()` for detailed results.

# ===== Plot the (Crude) Power Curve and the Regions =====

plot(n_power_region)
```

![Power Curve](template_n_from_power_mediation_lav_parallel_plot-1.png)

Power Curve

As shown above, approximately:

- sample sizes lower than 194 have power significantly lower than .80,
  and

- sample sizes higher than 212 have power significantly higher than .80.

In other words, sample sizes between 194 and 212 have power not
significantly different from .80.

If necessary, detailed results can be printed by
[`summary()`](https://rdrr.io/r/base/summary.html):

``` r
# ===== Detailed Results =====
summary(n_power_region)
#> 
#> ======<< Summary for the Lower Region >>======
#> 
#> 
#> ====== x_from_power Results ======
#> 
#> Call:
#> x_from_power(object = out, x = "n", what = "ub", goal = "close_enough", 
#>     final_nrep = 600, final_R = 1000, seed = 1357)
#> 
#> Predictor (x): Sample Size 
#> 
#> - Target Power: 0.800 
#> - Goal: Find 'x' with estimated upper confidence bound close enough to
#>   the target power.
#> 
#> === Major Results ===
#> 
#> - Final Value (Sample Size): 194
#> 
#> - Final Estimated Power: 0.768 
#> - Confidence Interval: [0.733; 0.800]
#> - Level of confidence: 95.0%
#> - Based on 600 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 150 to 194 
#> - Time spent in the search: 52.05 secs 
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
#>    -1.92878      0.01612  
#> 
#> Degrees of Freedom: 1199 Total (i.e. Null);  1198 Residual
#> Null Deviance:       1478 
#> Residual Deviance: 1446  AIC: 1450
#> 
#> - Detailed Results:
#> 
#> [test]: test_indirects: x-...->y 
#> [test_label]: x-...->y (All sig) 
#>     n  est   p.v reject r.cilo r.cihi
#> 1 150  NaN 1.000  0.620  0.581  0.658
#> 2 194  NaN 1.000  0.768  0.733  0.800
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
#> 
#> 
#> 
#> ======<< Summary for the Upper Region >>======
#> 
#> 
#> ====== x_from_power Results ======
#> 
#> Call:
#> x_from_power(object = out, seed = 1357, final_nrep = 600, final_R = 1000, 
#>     x = "n", what = "lb", goal = "close_enough")
#> 
#> Predictor (x): Sample Size 
#> 
#> - Target Power: 0.800 
#> - Goal: Find 'x' with estimated lower confidence bound close enough to
#>   the target power.
#> 
#> === Major Results ===
#> 
#> - Final Value (Sample Size): 212
#> 
#> - Final Estimated Power: 0.838 
#> - Confidence Interval: [0.807; 0.866]
#> - Level of confidence: 95.0%
#> - Based on 600 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 150 to 212 
#> - Time spent in the search: 53.19 secs 
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
#>    -2.21401      0.01793  
#> 
#> Degrees of Freedom: 1799 Total (i.e. Null);  1798 Residual
#> Null Deviance:       2055 
#> Residual Deviance: 1978  AIC: 1982
#> 
#> - Detailed Results:
#> 
#> [test]: test_indirects: x-...->y 
#> [test_label]: x-...->y (All sig) 
#>     n  est   p.v reject r.cilo r.cihi
#> 1 150  NaN 1.000  0.620  0.581  0.658
#> 2 194  NaN 1.000  0.768  0.733  0.800
#> 3 212  NaN 1.000  0.838  0.807  0.866
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

## Code Template

This is the code used above:

``` r
library(power4mome)

# ====== Model and Effect Size (Population Values) ======

model <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
"

model_es <-
"
m1 ~ x: m
m2 ~ x: m
y ~ m1: l
y ~ m2: l
y ~ x: s
"

# Test the Model Specification

out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 50000,
                  number_of_indicators = c(x = 3,
                                           m1 = 4,
                                           m2 = 3,
                                           y = 3),
                  reliability = c(x = .80,
                                  m1 = .70,
                                  m2 = .70,
                                  y = .80),
                  iseed = 1234)

# Check the Data Generated

print(out,
      data_long = TRUE)

# ====== Try One N and Estimate the Power ======

# For n = 200,
# when testing all indirect effects by
# Monte Carlo confidence intervals

out <- power4test(nrep = 600,
                  model = model,
                  pop_es = model_es,
                  n = 200,
                  number_of_indicators = c(x = 3,
                                           m1 = 4,
                                           m2 = 3,
                                           y = 3),
                  reliability = c(x = .80,
                                  m1 = .70,
                                  m2 = .70,
                                  y = .80),
                  R = 1000,
                  ci_type = "mc",
                  test_fun = test_k_indirect_effects,
                  test_args = list(x = "x",
                                   y = "y",
                                   mc_ci = TRUE,
                                   omnibus = "all_sig"),
                  iseed = 1234,
                  parallel = TRUE)

rejection_rates(out)

# ====== Regions of Ns ======

# Call n_region_from_power()
# - Set target power: target_power = .80 (Default, can be omitted)
# - Set the seed for the simulation: Integer. Should always be set.
# To set desired precision:
# - Set final number of R: final_R. If omitted,
#   it is set to 1000 or set to R in the original object.
# - Set final number of replications: final_nrep. If omitted,
#   it is set to 400 or set to nrep in the original object.

n_power_region <- n_region_from_power(out,
                                      seed = 1357)
n_power_region
plot(n_power_region)
summary(n_power_region)
```

## Final Remarks

For a moderate to small `nrep`, the results may be sensitive to the
`seed`. It is advised to do a final check of the sample size to be used
using
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and an `nrep` of 1000 or 2000.

For other options of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
please refer to their help pages, as well as the [Get-Started
article](https://sfcheung.github.io/power4mome/articles/power4mome.md)
and this
[article](https://sfcheung.github.io/power4mome/articles/x_from_power_for_n.md)
for
[`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
which is the function to find one of the regions, called twice by
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).
