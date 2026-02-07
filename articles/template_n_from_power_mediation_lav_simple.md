# Quick Template: Simple Mediation with Latent Variables

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

This file is for simple mediation models with latent variables.

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

model <-
"
m ~ x
y ~ m + x
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
m ~ x: m
y ~ m: l
y ~ x: s
"
```

![The Model](template_n_from_power_mediation_lav_simple_model-1.png)

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
                  number_of_indicators = c(x = 4,
                                           m = 3,
                                           y = 3),
                  reliability = c(x = .80,
                                  m = .70,
                                  y = .80),
                  iseed = 1234)

# ====== Check the Data Generated ======

print(out,
      data_long = TRUE)

# ====== Estimate the Power ======

# For n = 100,
# when testing the indirect effect by
# Monte Carlo confidence interval

out <- power4test(nrep = 600,
                  model = model,
                  pop_es = model_es,
                  n = 100,
                  number_of_indicators = c(x = 4,
                                           m = 3,
                                           y = 3),
                  reliability = c(x = .80,
                                  m = .70,
                                  y = .80),
                  R = 1000,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
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
#> m ~ x
#> y ~ m + x
#> 
#> == Model on Variables/Indicators ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> x =~ x1 + x2 + x3 + x4
#> m =~ m1 + m2 + m3
#> y =~ y1 + y2 + y3
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.300  
#>   y ~                        
#>     m                 0.500  
#>     x                 0.100  
#> 
#> Variances:
#>                    Population
#>    .m                 0.910  
#>    .y                 0.710  
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
#> ==== Population Reliability ====
#> 
#>    x   m   y
#>  0.8 0.7 0.8
#> 
#> == Population Standardized Loadings ==
#> 
#>      x     m     y
#>  0.707 0.661 0.756
#> ======================= Data Information =======================
#> 
#> Number of Replications:  600 
#> Sample Sizes:  100 
#> 
#> ==== Descriptive Statistics ====
#> 
#>    vars     n  mean   sd  skew kurtosis se
#> x1    1 60000  0.00 1.00 -0.01    -0.01  0
#> x2    2 60000 -0.01 1.00 -0.01    -0.02  0
#> x3    3 60000  0.00 1.01  0.01     0.00  0
#> x4    4 60000  0.00 1.00  0.01    -0.01  0
#> m1    5 60000  0.00 1.00  0.00     0.03  0
#> m2    6 60000  0.00 1.00  0.00     0.00  0
#> m3    7 60000  0.00 1.00  0.00     0.03  0
#> y1    8 60000  0.00 1.00  0.00    -0.01  0
#> y2    9 60000  0.00 1.00  0.00     0.04  0
#> y3   10 60000  0.00 1.00 -0.01     0.00  0
#> 
#> ==== Parameter Estimates Based on All 600 Samples Combined ====
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
#>     x1                0.709
#>     x2                0.705
#>     x3                0.707
#>     x4                0.704
#>   m =~                     
#>     m1                0.662
#>     m2                0.663
#>     m3                0.667
#>   y =~                     
#>     y1                0.762
#>     y2                0.751
#>     y3                0.759
#> 
#> Regressions:
#>                     est.std
#>   m ~                      
#>     x                 0.307
#>   y ~                      
#>     m                 0.499
#>     x                 0.097
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
#> lavaan 0.6-21 ended normally after 27 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        23
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                23.902
#>   Degrees of freedom                                32
#>   P-value (Chi-square)                           0.848
#> 
#> =========== <mc_out> ===========
#> 
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 1000 
#> 
#> 
#> =============== <test_indirect: x->m->y> ===============
#> 
#> Mean(s) across replication:
#>    est  cilo  cihi   sig pvalue
#>  0.171 0.005 0.376 0.531  0.109
#> 
#> - The value 'sig' is the rejection rate.
#> - If the null hypothesis is false, this is the power.
#> - Number of valid replications for rejection rate: 599 
#> - Proportion of valid replications for rejection rate: 0.998
rejection_rates(out)
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     est   p.v reject r.cilo r.cihi
#> 1 0.171 0.998  0.531  0.491  0.571
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
#> - not significantly different from 0.800: 156 to 176
#> - significantly lower than 0.800: 156
#> - significantly higher than 0.800: 176
#> 
#> Confidence intervals of the estimated power:
#> - for the lower bound (156): [0.733, 0.800]
#> - for the upper bound (176): [0.800, 0.859]
#> 
#> Call `summary()` for detailed results.

# ===== Plot the (Crude) Power Curve and the Regions =====

plot(n_power_region)
```

![Power Curve](template_n_from_power_mediation_lav_simple_plot-1.png)

Power Curve

As shown above, approximately:

- sample sizes lower than 156 have power significantly lower than .80,
  and

- sample sizes higher than 176 have power significantly higher than .80.

In other words, sample sizes between 156 and 176 have power not
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
#> - Final Value (Sample Size): 156
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
#> - The range of values explored: 100 to 242 
#> - Time spent in the search: 2.425 mins 
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
#>     -1.8516       0.0195  
#> 
#> Degrees of Freedom: 2999 Total (i.e. Null);  2998 Residual
#> Null Deviance:       3211 
#> Residual Deviance: 2850  AIC: 2854
#> 
#> - Detailed Results:
#> 
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1 100 0.171 0.998  0.531  0.491  0.571
#> 2 151 0.161 1.000  0.737  0.700  0.770
#> 3 156 0.159 1.000  0.768  0.733  0.800
#> 4 197 0.163 1.000  0.880  0.852  0.904
#> 5 242 0.165 1.000  0.950  0.930  0.965
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
#> - Final Value (Sample Size): 176
#> 
#> - Final Estimated Power: 0.832 
#> - Confidence Interval: [0.800; 0.859]
#> - Level of confidence: 95.0%
#> - Based on 600 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 100 to 242 
#> - Time spent in the search: 1.144 mins 
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
#>     -1.8276       0.0195  
#> 
#> Degrees of Freedom: 4199 Total (i.e. Null);  4198 Residual
#> Null Deviance:       4421 
#> Residual Deviance: 4050  AIC: 4054
#> 
#> - Detailed Results:
#> 
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1 100 0.171 0.998  0.531  0.491  0.571
#> 2 149 0.166 1.000  0.765  0.729  0.797
#> 3 151 0.161 1.000  0.737  0.700  0.770
#> 4 156 0.159 1.000  0.768  0.733  0.800
#> 5 176 0.162 1.000  0.832  0.800  0.859
#> 6 197 0.163 1.000  0.880  0.852  0.904
#> 7 242 0.165 1.000  0.950  0.930  0.965
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
y ~ m + x
"
model_es <-
"
m ~ x: m
y ~ m: l
y ~ x: s
"

# Test the Model Specification

out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 50000,
                  number_of_indicators = c(x = 4,
                                           m = 3,
                                           y = 3),
                  reliability = c(x = .80,
                                  m = .70,
                                  y = .80),
                  iseed = 1234)

# Check the Data Generated

print(out,
      data_long = TRUE)

# ====== Try One N and Estimate the Power ======

# For n = 100,
# when testing the indirect effect by
# Monte Carlo confidence interval

out <- power4test(nrep = 600,
                  model = model,
                  pop_es = model_es,
                  n = 100,
                  number_of_indicators = c(x = 4,
                                           m = 3,
                                           y = 3),
                  reliability = c(x = .80,
                                  m = .70,
                                  y = .80),
                  R = 1000,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
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
