# Quick Template: Simple Moderation with Observed Variables

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

This file is for moderation models with one moderator.

## Functions Used in This Template

- [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)

  - Set up the model and the population values, generate the data, and
    generate the Monte Carlo simulated estimates for Monte Carlo
    confidence interval.

- [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)

  - Find the regions of sample sizes based on the target power.

- [`test_parameters()`](https://sfcheung.github.io/power4mome/reference/test_parameters.md)

  - Test selected parameters. Used by
    [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
    to test a selected product term (interaction term).

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
y ~ x + w + x:w
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
y ~ x: m
y ~ w: s
y ~ x:w: l
"
```

![The Model](template_n_from_power_moderation_obs_simple_model-1.png)

The Model

``` r

# ====== Test the Model Specification ======

# Fit the model by regression using lm()
# Add: fit_model_args = list(fit_function = "lm")

out <- power4test(nrep = 2,
                  model = model,
                  pop_es = model_es,
                  n = 50000,
                  fit_model_args = list(fit_function = "lm"),
                  iseed = 1234)

# ====== Check the Data Generated ======

print(out,
      data_long = TRUE)

# ====== Estimate the Power ======

# For n = 50.
# Test the coefficient of the product term
# x:w in the regression results of lm().
# The test by CI is equivalent to the two-tailed t-test.

out <- power4test(nrep = 400,
                  model = model,
                  pop_es = model_es,
                  n = 50,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = "y~x:w"),
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
#> y ~ x + w + x:w
#> 
#> == Model on Variables/Indicators ==
#> 
#> y ~ x + w + x:w
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   y ~                        
#>     x                 0.300  
#>     w                 0.100  
#>     x:w               0.150  
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
#>    .y                 0.877  
#>     x                 1.000  
#>     w                 1.000  
#>     x:w               1.000  
#> 
#> (Computing conditional effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Conditional effects ==
#> 
#>  Path: x -> y
#>  Conditional on moderator(s): w
#>  Moderator(s) represented by: w
#> 
#>       [w] (w)   ind
#> 1 M+1.0SD   1 0.450
#> 2 Mean      0 0.300
#> 3 M-1.0SD  -1 0.150
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
#> Number of Replications:  400 
#> Sample Sizes:  50 
#> 
#> ==== Descriptive Statistics ====
#> 
#>     vars     n  mean   sd  skew kurtosis   se
#> y      1 20000 -0.01 1.00  0.03     0.02 0.01
#> x      2 20000 -0.01 1.00  0.04    -0.01 0.01
#> w      3 20000  0.00 1.01 -0.02     0.02 0.01
#> x:w    4 20000 -0.01 1.01 -0.01     5.04 0.01
#> 
#> ==== Parameter Estimates Based on All 400 Samples Combined ====
#> 
#> Total Sample Size: 20000 
#> 
#> ==== Standardized Estimates ====
#> 
#> Variances and error variances omitted.
#> 
#> Regressions:
#>                     est.std
#>   y ~                      
#>     x                 0.296
#>     w                 0.091
#>     x:w               0.153
#> 
#> Covariances:
#>                     est.std
#>   x ~~                     
#>     w                -0.011
#>     x:w               0.000
#>   w ~~                     
#>     x:w              -0.014
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
#> 
#> The models:
#> y ~ x + w + x:w
#> <environment: 0x0000020858857e40>
#> 
#> 
#> ====================== Test(s) Conducted ======================
#> 
#> - test_parameters: CIs (pars: y~x:w)
#> 
#> Call print() and set 'test_long = TRUE' for a detailed report.
rejection_rates(out)
#> [test]: test_parameters: CIs (pars: y~x:w) 
#> [test_label]: y~x:w 
#>     est   p.v reject r.cilo r.cihi
#> 1 0.146 1.000  0.172  0.139  0.213
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
#> - not significantly different from 0.800: 303 to 336
#> - significantly lower than 0.800: 303
#> - significantly higher than 0.800: 336
#> 
#> Confidence intervals of the estimated power:
#> - for the lower bound (303): [0.716, 0.799]
#> - for the upper bound (336): [0.809, 0.879]
#> 
#> Call `summary()` for detailed results.

# ===== Plot the (Crude) Power Curve and the Regions =====

plot(n_power_region)
```

![Power Curve](template_n_from_power_moderation_obs_simple_plot-1.png)

Power Curve

As shown above, approximately:

- sample sizes lower than 303 have power significantly lower than .80,
  and

- sample sizes higher than 336 have power significantly higher than .80.

In other words, sample sizes between 303 and 336 have power not
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
#> - Final Value (Sample Size): 303
#> 
#> - Final Estimated Power: 0.760 
#> - Confidence Interval: [0.716; 0.799]
#> - Level of confidence: 95.0%
#> - Based on 400 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 232 to 50 
#> - Time spent in the search: 21.68 secs 
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
#>     -1.9878       0.0111  
#> 
#> Degrees of Freedom: 1599 Total (i.e. Null);  1598 Residual
#> Null Deviance:       2104 
#> Residual Deviance: 1582  AIC: 1586
#> 
#> - Detailed Results:
#> 
#> [test]: test_parameters: CIs (pars: y~x:w) 
#> [test_label]: y~x:w 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.146 1.000  0.172  0.139  0.213
#> 2 232 0.154 1.000  0.708  0.661  0.750
#> 3 303 0.148 1.000  0.760  0.716  0.799
#> 4 374 0.156 1.000  0.890  0.856  0.917
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
#> - Final Value (Sample Size): 336
#> 
#> - Final Estimated Power: 0.848 
#> - Confidence Interval: [0.809; 0.879]
#> - Level of confidence: 95.0%
#> - Based on 400 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 336 to 374 
#> - Time spent in the search: 6.833 secs 
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
#>   -1.606100     0.009885  
#> 
#> Degrees of Freedom: 799 Total (i.e. Null);  798 Residual
#> Null Deviance:       622 
#> Residual Deviance: 618.8     AIC: 622.8
#> 
#> - Detailed Results:
#> 
#> [test]: test_parameters: CIs (pars: y~x:w) 
#> [test_label]: y~x:w 
#>     n   est   p.v reject r.cilo r.cihi
#> 1 336 0.154 1.000  0.848  0.809  0.879
#> 2 374 0.156 1.000  0.890  0.856  0.917
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
y ~ x + w + x:w
"
model_es <-
"
y ~ x: m
y ~ w: s
y ~ x:w: l
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

# For n = 50.
# Test the coefficient of the product term
# x:w in the regression results of lm().
# The test by CI is equivalent to the two-tailed t-test.

out <- power4test(nrep = 400,
                  model = model,
                  pop_es = model_es,
                  n = 50,
                  fit_model_args = list(fit_function = "lm"),
                  test_fun = test_parameters,
                  test_args = list(pars = "y~x:w"),
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
