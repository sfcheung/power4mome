# Summarize 'x_from_power' Results

The summary method of the output of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).

## Usage

``` r
# S3 method for class 'x_from_power'
summary(object, ...)

# S3 method for class 'n_region_from_power'
summary(object, ...)

# S3 method for class 'summary.x_from_power'
print(x, digits = 3, ...)

# S3 method for class 'summary.n_region_from_power'
print(x, digits = 3, ...)
```

## Arguments

- object:

  An `x_from_power`-class object, such as the output of
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
  or an object of the class `n_region_from_power`, such as the output of
  [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).

- ...:

  Additional arguments. Not used for now.

- x:

  The output of `summary.x_from_power()`, the `summary` method of an
  `x_from_power` object, which is the output of
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
  or the output of `summary.n_region_from_power()`, the `summary` method
  of an `n_region_from_power` object (the output of
  [`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)).

- digits:

  The number of digits after the decimal when printing the results.

## Value

The `summary` method for `x_from_power` objects returns an object of the
class `summary.x_from_power`, which is simply the output of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
with a `print` method dedicated for detailed summary. Please refer to
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
for the contents.

The `print`-method of `summary.x_from_power` objects returns the object
`x` invisibly. It is called for its side effect.

The `print`-method of `summary.n_region_from_power` objects returns the
object `x` invisibly. It is called for its side effect.

## Details

The `summary` method simply prepares the results of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
to be printed in details.

## See also

[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
[`n_region_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)

## Examples

``` r
# Specify the population model

mod <-
"
m ~ x
y ~ m + x
"

# Specify the population values

mod_es <-
"
m ~ x: m
y ~ m: l
y ~ x: n
"

# Generate the datasets

sim_only <- power4test(nrep = 5,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       do_the_test = FALSE,
                       iseed = 2345)
#> Simulate the data:
#> Fit the model(s):

# Do a test

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters,
                       test_args = list(pars = "m~x"))
#> Do the test: test_parameters: CIs (pars: m~x) 

# Determine the sample size with a power of .80 (default)

power_vs_n <- x_from_power(test_out,
                           x = "n",
                           progress = TRUE,
                           target_power = .80,
                           final_nrep = 5,
                           max_trials = 1,
                           seed = 1234)
#> 
#> --- Setting ---
#> 
#> Algorithm:  bisection 
#> Goal:  ci_hit 
#> What:  point   (Estimated Power) 
#> 
#> --- Progress  ---
#> 
#> - Set 'progress = FALSE' to suppress displaying the progress.
#> - Set 'simulation progress = FALSE' to suppress displaying the progress
#>   in the simulation.
#> 
#> Initial interval: [50, 100] 
#> 
#> 
#> Do the simulation for the lower bound:
#> 
#> Try x = 50 
#> 
#> Updating the simulation for sample size: 50 
#> Re-simulate the data:
#> Fit the model(s):
#> Update the test(s):
#> Update test_parameters: CIs (pars: m~x) :
#> 
#> Estimated power at 50: 0.400, 95.0% confidence interval: [0.118,0.769]
#> 
#> Initial interval: [50, 100] 
#> 
#> - Rejection Rates:
#> [test]: test_parameters: CIs (pars: m~x) 
#> [test_label]: m~x 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.222 1.000  0.400  0.118  0.769
#> 2 100 0.298 1.000  0.800  0.376  0.964
#> 
#> 
#> == Enter extending interval ...
#> The interval is already valid: [50, 100] 
#> == Exit extending interval ...
#> 
#> Iteration # 1 
#> 
#> Try x = 75 
#> 
#> Updating the simulation for sample size: 75 
#> Re-simulate the data:
#> Fit the model(s):
#> Update the test(s):
#> Update test_parameters: CIs (pars: m~x) :
#> 
#> Estimated power at 75: 1.000, 95.0% confidence interval: [0.566,1.000]
#> - Rejection Rates:
#> [test]: test_parameters: CIs (pars: m~x) 
#> [test_label]: m~x 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.222 1.000  0.400  0.118  0.769
#> 2  75 0.334 1.000  1.000  0.566  1.000
#> 3 100 0.298 1.000  0.800  0.376  0.964
#> 
#> - 'nls()' estimation skipped when less than 4 values of predictor examined.
#> Solution found.
#> 
#> ========== Final Stage ==========
#> 
#> - Start at 2026-02-28 14:54:14 
#> - Rejection Rates:
#> 
#> [test]: test_parameters: CIs (pars: m~x) 
#> [test_label]: m~x 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.222 1.000  0.400  0.118  0.769
#> 2  75 0.334 1.000  1.000  0.566  1.000
#> 3 100 0.298 1.000  0.800  0.376  0.964
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
#> - Estimated Power Curve:
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
#>    -2.25384      0.04624  
#> 
#> Degrees of Freedom: 14 Total (i.e. Null);  13 Residual
#> Null Deviance:       17.4 
#> Residual Deviance: 15.23     AIC: 19.23
#> 
#> 
#> - Final Value: 75 
#> 
#> - Final Estimated Power: 1.0000 
#> - Confidence Interval: [0.5655; 1.0000]
#> - CI Level: 95.00%
summary(power_vs_n)
#> 
#> ====== x_from_power Results ======
#> 
#> Call:
#> x_from_power(object = test_out, x = "n", target_power = 0.8, 
#>     progress = TRUE, max_trials = 1, final_nrep = 5, seed = 1234)
#> 
#> Predictor (x): Sample Size 
#> 
#> - Target Power: 0.800 
#> - Goal: Find 'x' with the confidence interval of the estimated power
#>   enclosing the target power.
#> 
#> === Major Results ===
#> 
#> - Final Value (Sample Size): 75
#> 
#> - Final Estimated Power: 1.000 
#> - Confidence Interval: [0.566; 1.000]
#> - Level of confidence: 95.0%
#> - Based on 5 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - The range of values explored: 50 to 100 
#> - Time spent in the search: 0.9539 secs 
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
#>    -2.25384      0.04624  
#> 
#> Degrees of Freedom: 14 Total (i.e. Null);  13 Residual
#> Null Deviance:       17.4 
#> Residual Deviance: 15.23     AIC: 19.23
#> 
#> - Detailed Results:
#> 
#> [test]: test_parameters: CIs (pars: m~x) 
#> [test_label]: m~x 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.222 1.000  0.400  0.118  0.769
#> 2  75 0.334 1.000  1.000  0.566  1.000
#> 3 100 0.298 1.000  0.800  0.376  0.964
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
```
