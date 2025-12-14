# Quick Template: Serial Mediation with Latent Variables

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

This file is for serial mediation models with latent variables.

## Functions Used in This Template

- [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)

  - Set up the model and the population values, generate the data, and
    generate the Monte Carlo simulated estimates for Monte Carlo
    confidence interval.

- [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)

  - Find the regions of sample sizes based on the target power.

- [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md)

  - Test the indirect effect using Monte Carlo or bootstrap confidence
    intervals. Used by
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

# Omit any paths hypothesized to be zero

model <-
"
m1 ~ x
m2 ~ m1 + x
m3 ~ m2 + m1 + x
y ~ m3 + m2 + m1 + x
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
m1 ~ x: l
m2 ~ m1: l
m3 ~ m2: l
y ~ m3: m
y ~ x: s
"
```

![The Model](template_n_from_power_mediation_lav_serial_model-1.png)

The Model

Refer to this
[article](https://sfcheung.github.io/power4mome/articles/power4test_latent_mediation.html)
on how to set `number_of_indicators` and `reliability` when calling
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

``` r

# ====== Test the Model Specification ======
out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 50000,
                  number_of_indicators = c(x = 3,
                                           m1 = 4,
                                           m2 = 3,
                                           m3 = 4,
                                           y = 3),
                  reliability = c(x = .80,
                                  m1 = .70,
                                  m2 = .70,
                                  m3 = .70,
                                  y = .80),
                  iseed = 1234)

# ====== Check the Data Generated ======

print(out,
      data_long = TRUE)

# ====== Estimate the Power ======

# For n = 150,
# when testing the indirect effect by
# Monte Carlo confidence interval

out <- power4test(nrep = 400,
                  model = model,
                  pop_es = model_es,
                  n = 150,
                  R = 1000,
                  number_of_indicators = c(x = 3,
                                           m1 = 4,
                                           m2 = 3,
                                           m3 = 4,
                                           y = 3),
                  reliability = c(x = .80,
                                  m1 = .70,
                                  m2 = .70,
                                  m3 = .70,
                                  y = .80),
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = c("m1", "m2", "m3"),
                                   y = "y",
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
#> m1 ~ x
#> m2 ~ m1 + x
#> m3 ~ m2 + m1 + x
#> y ~ m3 + m2 + m1 + x
#> 
#> == Model on Variables/Indicators ==
#> 
#> m1 ~ x
#> m2 ~ m1 + x
#> m3 ~ m2 + m1 + x
#> y ~ m3 + m2 + m1 + x
#> 
#> x =~ x1 + x2 + x3
#> m1 =~ m11 + m12 + m13 + m14
#> m2 =~ m21 + m22 + m23
#> m3 =~ m31 + m32 + m33 + m34
#> y =~ y1 + y2 + y3
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m1 ~                       
#>     x                 0.500  
#>   m2 ~                       
#>     m1                0.500  
#>     x                 0.000  
#>   m3 ~                       
#>     m2                0.500  
#>     m1                0.000  
#>     x                 0.000  
#>   y ~                        
#>     m3                0.300  
#>     m2                0.000  
#>     m1                0.000  
#>     x                 0.100  
#> 
#> Variances:
#>                    Population
#>    .m1                0.750  
#>    .m2                0.750  
#>    .m3                0.750  
#>    .y                 0.893  
#>     x                 1.000  
#> 
#> (Computing indirect effects for 8 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>                            ind
#> x -> m1 -> m2 -> m3 -> y 0.037
#> x -> m1 -> m2 -> y       0.000
#> x -> m1 -> m3 -> y       0.000
#> x -> m1 -> y             0.000
#> x -> m2 -> m3 -> y       0.000
#> x -> m2 -> y             0.000
#> x -> m3 -> y             0.000
#> x -> y                   0.100
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ==== Population Reliability ====
#> 
#>    x  m1  m2  m3   y
#>  0.8 0.7 0.7 0.7 0.8
#> 
#> == Population Standardized Loadings ==
#> 
#>      x    m1    m2    m3     y
#>  0.756 0.607 0.661 0.607 0.756
#> ======================= Data Information =======================
#> 
#> Number of Replications:  400 
#> Sample Sizes:  150 
#> 
#> ==== Descriptive Statistics ====
#> 
#>     vars     n  mean   sd  skew kurtosis se
#> x1     1 60000  0.00 1.00  0.02     0.01  0
#> x2     2 60000 -0.01 1.00 -0.01     0.00  0
#> x3     3 60000  0.00 1.00  0.02    -0.01  0
#> m11    4 60000  0.00 1.00  0.00    -0.01  0
#> m12    5 60000  0.00 1.00 -0.01    -0.01  0
#> m13    6 60000 -0.01 1.00  0.01    -0.01  0
#> m14    7 60000  0.00 1.00  0.00     0.03  0
#> m21    8 60000  0.00 1.00  0.00     0.01  0
#> m22    9 60000  0.00 1.00 -0.02     0.02  0
#> m23   10 60000  0.00 1.00 -0.02    -0.03  0
#> m31   11 60000  0.00 1.00  0.00     0.00  0
#> m32   12 60000  0.00 1.00 -0.02    -0.01  0
#> m33   13 60000  0.00 1.00 -0.01     0.00  0
#> m34   14 60000  0.00 1.00 -0.02    -0.01  0
#> y1    15 60000  0.00 1.00  0.00     0.00  0
#> y2    16 60000  0.00 1.00  0.00     0.01  0
#> y3    17 60000  0.00 0.99 -0.03    -0.01  0
#> 
#> ==== Parameter Estimates Based on All 400 Samples Combined ====
#> 
#> Total Sample Size: 60000 
#> 
#> ==== Standardized Estimates ====
#> 
#> Variances and error variances omitted.
#> 
#> Latent Variables:
#>                     est.std
#>   x =~                     
#>     x1                0.756
#>     x2                0.754
#>     x3                0.753
#>   m1 =~                    
#>     m11               0.608
#>     m12               0.607
#>     m13               0.612
#>     m14               0.608
#>   m2 =~                    
#>     m21               0.664
#>     m22               0.662
#>     m23               0.661
#>   m3 =~                    
#>     m31               0.610
#>     m32               0.612
#>     m33               0.611
#>     m34               0.604
#>   y =~                     
#>     y1                0.755
#>     y2                0.755
#>     y3                0.750
#> 
#> Regressions:
#>                     est.std
#>   m1 ~                     
#>     x                 0.504
#>   m2 ~                     
#>     m1                0.496
#>     x                -0.001
#>   m3 ~                     
#>     m2                0.492
#>     m1               -0.000
#>     x                 0.012
#>   y ~                      
#>     m3                0.301
#>     m2                0.006
#>     m1                0.003
#>     x                 0.089
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
#> lavaan 0.6-20 ended normally after 31 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        44
#> 
#>   Number of observations                           150
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                               123.231
#>   Degrees of freedom                               109
#>   P-value (Chi-square)                           0.166
#> 
#> =========== <mc_out> ===========
#> 
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 1000 
#> 
#> 
#> ====================== Test(s) Conducted ======================
#> 
#> - test_indirect: x->m1->m2->m3->y
#> 
#> Call print() and set 'test_long = TRUE' for a detailed report.
rejection_rates(out)
#> [test]: test_indirect: x->m1->m2->m3->y 
#> [test_label]: Test 
#>     est   p.v reject r.cilo r.cihi
#> 1 0.040 1.000  0.407  0.360  0.456
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
# - Set final number of R: final_R = 1000 (Default, can be omitted)
# - Set final number of replications: final_nrep = 400 (Default, can be omitted)

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
#> - not significantly different from 0.800: 222 to 283
#> - significantly lower than 0.800: 222
#> - significantly higher than 0.800: 283
#> 
#> Confidence intervals of the estimated power:
#> - for the lower bound (222): [0.705, 0.790]
#> - for the upper bound (283): [0.817, 0.886]
#> 
#> Call `summary()` for detailed results.

# ===== Plot the (Crude) Power Curve and the Regions =====

plot(n_power_region)
```

![Power Curve](template_n_from_power_mediation_lav_serial_plot-1.png)

Power Curve

As shown above, approximately:

- sample sizes lower than 222 have power significantly lower than .80,
  and

- sample sizes higher than 283 have power significantly higher than .80.

In other words, sample sizes between 222 and 283 have power not
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
#> power4mome::x_from_power(object = out, x = "n", what = "ub", 
#>     goal = "close_enough", seed = 1357)
#> 
#> Predictor (x): Sample Size 
#> 
#> - Target Power: 0.800 
#> - Goal: Find 'x' with estimated upper confidence bound close enough to
#>   the target power.
#> 
#> === Major Results ===
#> 
#> - Final Value (Sample Size): 222
#> 
#> - Final Estimated Power: 0.750 
#> - Confidence Interval: [0.705; 0.790]
#> - Level of confidence: 95.0%
#> - Based on 400 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 150 to 294 
#> - Time spent in the search: 59.16 secs 
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
#>    -2.74890      0.01639  
#> 
#> Degrees of Freedom: 1199 Total (i.e. Null);  1198 Residual
#> Null Deviance:       1513 
#> Residual Deviance: 1309  AIC: 1313
#> 
#> - Detailed Results:
#> 
#> [test]: test_indirect: x->m1->m2->m3->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1 150 0.040 1.000  0.407  0.360  0.456
#> 2 222 0.043 1.000  0.750  0.705  0.790
#> 3 294 0.040 1.000  0.868  0.831  0.897
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
#> power4mome::x_from_power(object = out, seed = 1357, x = "n", 
#>     what = "lb", goal = "close_enough")
#> 
#> Predictor (x): Sample Size 
#> 
#> - Target Power: 0.800 
#> - Goal: Find 'x' with estimated lower confidence bound close enough to
#>   the target power.
#> 
#> === Major Results ===
#> 
#> - Final Value (Sample Size): 283
#> 
#> - Final Estimated Power: 0.855 
#> - Confidence Interval: [0.817; 0.886]
#> - Level of confidence: 95.0%
#> - Based on 400 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 271 to 294 
#> - Time spent in the search: 51.52 secs 
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
#>    -4.59795      0.02221  
#> 
#> Degrees of Freedom: 1199 Total (i.e. Null);  1198 Residual
#> Null Deviance:       1052 
#> Residual Deviance: 1045  AIC: 1049
#> 
#> - Detailed Results:
#> 
#> [test]: test_indirect: x->m1->m2->m3->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1 271 0.038 1.000  0.800  0.758  0.836
#> 2 283 0.039 1.000  0.855  0.817  0.886
#> 3 294 0.040 1.000  0.868  0.831  0.897
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
m2 ~ m1 + x
m3 ~ m2 + m1 + x
y ~ m3 + m2 + m1 + x
"

model_es <-
"
m1 ~ x: l
m2 ~ m1: l
m3 ~ m2: l
y ~ m3: m
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
                                           m3 = 4,
                                           y = 3),
                  reliability = c(x = .80,
                                  m1 = .70,
                                  m2 = .70,
                                  m3 = .70,
                                  y = .80),
                  iseed = 1234)

# Check the Data Generated

print(out,
      data_long = TRUE)

# ====== Try One N and Estimate the Power ======

# For n = 150,
# when testing the indirect effect by
# Monte Carlo confidence interval

out <- power4test(nrep = 400,
                  model = model,
                  pop_es = model_es,
                  n = 150,
                  R = 1000,
                  number_of_indicators = c(x = 3,
                                           m1 = 4,
                                           m2 = 3,
                                           m3 = 4,
                                           y = 3),
                  reliability = c(x = .80,
                                  m1 = .70,
                                  m2 = .70,
                                  m3 = .70,
                                  y = .80),
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = c("m1", "m2", "m3"),
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 1234,
                  parallel = TRUE)

rejection_rates(out)

# ====== Regions of Ns ======

# Call n_region_from_power()
# - Set target power: target_power = .80 (Default, can be omitted)
# - Set the seed for the simulation: Integer. Should always be set.
# To set desired precision:
# - Set final number of R: final_R = 1000 (Default, can be omitted)
# - Set final number of replications: final_nrep = 400 (Default, can be omitted)

n_power_region <- n_region_from_power(out,
                                      seed = 1357)
n_power_region
plot(n_power_region)
summary(n_power_region)
```

## Final Remarks

For other options of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
please refer to their help pages, as well as the [Get-Started
article](https://sfcheung.github.io/power4mome/articles/power4mome.html)
and this
[article](https://sfcheung.github.io/power4mome/articles/x_from_power_for_n.html)
for
[`n_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
which is the function to find one of the regions, called twice by
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).
