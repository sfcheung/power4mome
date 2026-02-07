# Quick Template: Simple Moderated Mediation with Observed Variables: b-Path Moderated

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

This file is for moderated mediation models with only one mediator, and
the *b* path moderated.

## Functions Used in This Template

- [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)

  - Set up the model and the population values, generate the data, and
    generate the Monte Carlo simulated estimates for Monte Carlo
    confidence interval.

- [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)

  - Find the regions of sample sizes based on the target power.

- [`test_index_of_mome()`](https://sfcheung.github.io/power4mome/reference/test_index_of_mome.md)

  - Test the moderated mediation by the index of moderated mediation.
    Used by
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

The code for the model:

``` r
# ====== Model: Form ======

model <-
"
m ~ x
y ~ m + x + w + m:w
"

# ====== Model: Population Values ======

# For a regression coefficient
# l: large (.50 by default)
# m: medium (.30 by default)
# s: small (.10 by default)
# n: nil (.00 by default)
# For the product term:
# l: large (.15 by default)
# m: medium (.10 by default)
# s: small (.05 by default)
# -l, -m, and -s denote negative values
# Omitted paths are zero by default
# Can also set to a number directly
# Set each path to the hypothesized magnitude

# For a path moderated, the coefficient
# of a predictor is its standardized
# effect when the moderator equal to
# its mean.

model_es <-
"
m ~ x: m
y ~ w: s
y ~ m:w: l
y ~ m: m
y ~ x: s
"
```

![The Model](template_n_from_power_mome_obs_b_model-1.png)

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

# For n = 400,
# when testing the index of moderated mediation by
# Monte Carlo confidence interval.

out <- power4test(nrep = 600,
                  model = model,
                  pop_es = model_es,
                  n = 400,
                  R = 1000,
                  ci_type = "mc",
                  test_fun = test_index_of_mome,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   w = "w",
                                   mc_ci = TRUE),
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
#> m ~ x
#> y ~ m + x + w + m:w
#> 
#> m ~~ m:w
#> m:w ~~ w
#> m ~~ w
#> m:w ~~ x
#> w ~~ x
#> 
#> 
#> == Model on Variables/Indicators ==
#> 
#> m ~ x
#> y ~ m + x + w + m:w
#> 
#> m ~~ m:w
#> m:w ~~ w
#> m ~~ w
#> m:w ~~ x
#> w ~~ x
#> 
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.300  
#>   y ~                        
#>     m                 0.300  
#>     x                 0.100  
#>     w                 0.100  
#>     m:w               0.150  
#> 
#> Covariances:
#>                    Population
#>  .m ~~                       
#>     m:w               0.000  
#>   m:w ~~                     
#>     w                 0.000  
#>  .m ~~                       
#>     w                 0.000  
#>   m:w ~~                     
#>     x                 0.000  
#>   w ~~                       
#>     x                 0.000  
#> 
#> Variances:
#>                    Population
#>    .m                 0.910  
#>    .y                 0.849  
#>     x                 1.000  
#>     w                 1.000  
#>     m:w               1.000  
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
#> 1 M+1.0SD   1 0.135 0.300 0.450
#> 2 Mean      0 0.090 0.300 0.300
#> 3 M-1.0SD  -1 0.045 0.300 0.150
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - 'm~x','y~m' is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 
#> 
#> ======================= Data Information =======================
#> 
#> Number of Replications:  600 
#> Sample Sizes:  400 
#> 
#> ==== Descriptive Statistics ====
#> 
#>     vars      n mean sd  skew kurtosis se
#> m      1 240000    0  1 -0.01     0.01  0
#> y      2 240000    0  1  0.03     0.03  0
#> m:w    3 240000    0  1 -0.03     6.14  0
#> x      4 240000    0  1  0.00    -0.01  0
#> w      5 240000    0  1  0.00    -0.01  0
#> 
#> ==== Parameter Estimates Based on All 600 Samples Combined ====
#> 
#> Total Sample Size: 240000 
#> 
#> ==== Standardized Estimates ====
#> 
#> Variances and error variances omitted.
#> 
#> Regressions:
#>                     est.std
#>   m ~                      
#>     x                 0.300
#>   y ~                      
#>     m                 0.302
#>     x                 0.101
#>     w                 0.101
#>     m:w               0.147
#> 
#> Covariances:
#>                     est.std
#>  .m ~~                     
#>     m:w               0.003
#>   m:w ~~                   
#>     w                -0.003
#>  .m ~~                     
#>     w                -0.002
#>   m:w ~~                   
#>     x                 0.002
#>   x ~~                     
#>     w                -0.003
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
#> lavaan 0.6-21 ended normally after 14 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        15
#> 
#>   Number of observations                           400
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
#> Number of Monte Carlo replications: 1000 
#> 
#> 
#> ==== <test_index_of_mome: x->m->y, moderated by w> ====
#> 
#> Mean(s) across replication:
#>    est  cilo  cihi   sig pvalue
#>  0.044 0.015 0.077 0.870  0.031
#> 
#> - The value 'sig' is the rejection rate.
#> - If the null hypothesis is false, this is the power.
#> - Number of valid replications for rejection rate: 600 
#> - Proportion of valid replications for rejection rate: 1.000
rejection_rates(out)
#> [test]: test_index_of_mome: x->m->y, moderated by w 
#> [test_label]: Test 
#>     est   p.v reject r.cilo r.cihi
#> 1 0.044 1.000  0.870  0.841  0.895
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
#> - not significantly different from 0.800: 300 to 343
#> - significantly lower than 0.800: 300
#> - significantly higher than 0.800: 343
#> 
#> Confidence intervals of the estimated power:
#> - for the lower bound (300): [0.726, 0.794]
#> - for the upper bound (343): [0.798, 0.858]
#> 
#> Call `summary()` for detailed results.

# ===== Plot the (Crude) Power Curve and the Regions =====

plot(n_power_region)
```

![Power Curve](template_n_from_power_mome_obs_b_plot-1.png)

Power Curve

As shown above, approximately:

- sample sizes lower than 300 have power significantly lower than .80,
  and

- sample sizes higher than 343 have power significantly higher than .80.

In other words, sample sizes between 300 and 343 have power not
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
#> - Final Value (Sample Size): 300
#> 
#> - Final Estimated Power: 0.762 
#> - Confidence Interval: [0.726; 0.794]
#> - Level of confidence: 95.0%
#> - Based on 600 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 200 to 400 
#> - Time spent in the search: 1.89 mins 
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
#>   -0.878687     0.006894  
#> 
#> Degrees of Freedom: 1799 Total (i.e. Null);  1798 Residual
#> Null Deviance:       2016 
#> Residual Deviance: 1917  AIC: 1921
#> 
#> - Detailed Results:
#> 
#> [test]: test_index_of_mome: x->m->y, moderated by w 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1 200 0.046 1.000  0.625  0.586  0.663
#> 2 300 0.044 1.000  0.762  0.726  0.794
#> 3 400 0.044 1.000  0.870  0.841  0.895
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
#> - Final Value (Sample Size): 343
#> 
#> - Final Estimated Power: 0.830 
#> - Confidence Interval: [0.798; 0.858]
#> - Level of confidence: 95.0%
#> - Based on 600 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 200 to 400 
#> - Time spent in the search: 2.833 mins 
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
#>   -1.021018     0.007634  
#> 
#> Degrees of Freedom: 3599 Total (i.e. Null);  3598 Residual
#> Null Deviance:       3693 
#> Residual Deviance: 3551  AIC: 3555
#> 
#> - Detailed Results:
#> 
#> [test]: test_index_of_mome: x->m->y, moderated by w 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1 200 0.046 1.000  0.625  0.586  0.663
#> 2 300 0.044 1.000  0.762  0.726  0.794
#> 3 300 0.044 1.000  0.785  0.750  0.816
#> 4 343 0.045 1.000  0.830  0.798  0.858
#> 5 355 0.046 1.000  0.873  0.844  0.898
#> 6 400 0.044 1.000  0.870  0.841  0.895
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
m ~ x
y ~ m + x + w + m:w
"
model_es <-
"
m ~ x: m
y ~ w: s
y ~ m:w: l
y ~ m: m
y ~ x: s
"

# Test the Model Specification

out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 50000,
                  fit_model_args = list(fit_function = "lm"),
                  iseed = 1234)

# Check the Data Generated

print(out,
      data_long = TRUE)

# ====== Try One N and Estimate the Power ======

# For n = 400,
# when testing the index of moderated mediation by
# Monte Carlo confidence interval.

out <- power4test(nrep = 600,
                  model = model,
                  pop_es = model_es,
                  n = 400,
                  R = 1000,
                  ci_type = "mc",
                  test_fun = test_index_of_mome,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   w = "w",
                                   mc_ci = TRUE),
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
