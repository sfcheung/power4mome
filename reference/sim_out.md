# Create a 'sim_out' Object

Combine the outputs of
[`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md),
[`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md),
and optionally
[`gen_mc()`](https://sfcheung.github.io/power4mome/reference/gen_mc.md)
and/or
[`gen_boot()`](https://sfcheung.github.io/power4mome/reference/gen_boot.md)
to one single object.

## Usage

``` r
sim_out(data_all, ...)

# S3 method for class 'sim_out'
print(x, digits = 3, digits_descriptive = 2, fit_to_all_args = list(), ...)
```

## Arguments

- data_all:

  The output of
  [`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md).

- ...:

  Named arguments of objects to be added to each replication under the
  element `extra`. For example, if set to `fit = fit_all`, where
  `fit_all` is the output of
  [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md),
  then `data_all[[1]]$extra$fit` will be set to the first output in
  `fit_all`.

- x:

  The `sim_out` object to be printed.

- digits:

  The numbers of digits displayed after the decimal.

- digits_descriptive:

  The number of digits displayed after the decimal for the descriptive
  statistics table.

- fit_to_all_args:

  A named list of arguments to be passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) when the
  model is fitted to a sample combined from all samples stored.

## Value

The function `sim_out()` returns a `sim_out` object, which is a list of
length equal to the length of `data_all`. Each element of the list is a
`sim_data` object with the element `extra` added to it. Other named
elements will be added under this name. For example. the output of
[`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
for this replication can be added to `fit`, under `extra`. See the
description of the argument `...` for details.

The `print` method of `sim_out` returns `x` invisibly. Called for its
side effect.

## Details

It merges into one object the output of
[`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md),
which is a list of `nrep` simulated datasets,
[`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md),
which is a list of the
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) output for
the `nrep` datasets, and optionally the output of
[`gen_mc()`](https://sfcheung.github.io/power4mome/reference/gen_mc.md)
or
[`gen_boot()`](https://sfcheung.github.io/power4mome/reference/gen_boot.md),
which is a list of the `R` sets of Monte Carlo or bootstrap estimates
based on the results of
[`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md).
The list has `nrep` elements, each element with the data, the model fit
results, and optionally the Monte Carlo estimates matched.

This object can then be used for testing effects of interests, which are
further processed to estimate the power of this test.

The function `sim_out()` is used by the all-in-one function
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
Users usually do not call this function directly, though developers can
use this function to develop other functions for power analysis, or to
build their own workflows to do the power analysis.

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

dats <- sim_data(nrep = 5,
                 model = mod,
                 pop_es = es,
                 n = 100,
                 iseed = 1234)

# Fit the population model to each dataset

fits <- fit_model(dats)

# Combine the results to one object

sim_out_all <- sim_out(data_all = dats,
                       fit = fits)
sim_out_all
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

# Verify that the elements of fits are set to extra$fit

library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
parameterEstimates(fits[[1]])
#>   lhs op rhs   est    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x 0.382 0.099 3.837  0.000    0.187    0.577
#> 2   y  ~   m 0.266 0.103 2.591  0.010    0.065    0.467
#> 3   y  ~   x 0.108 0.109 0.990  0.322   -0.106    0.322
#> 4   m ~~   m 0.903 0.128 7.071  0.000    0.653    1.153
#> 5   y ~~   y 0.951 0.134 7.071  0.000    0.687    1.214
#> 6   x ~~   x 0.913 0.000    NA     NA    0.913    0.913
parameterEstimates(sim_out_all[[1]]$extra$fit)
#>   lhs op rhs   est    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x 0.382 0.099 3.837  0.000    0.187    0.577
#> 2   y  ~   m 0.266 0.103 2.591  0.010    0.065    0.467
#> 3   y  ~   x 0.108 0.109 0.990  0.322   -0.106    0.322
#> 4   m ~~   m 0.903 0.128 7.071  0.000    0.653    1.153
#> 5   y ~~   y 0.951 0.134 7.071  0.000    0.687    1.214
#> 6   x ~~   x 0.913 0.000    NA     NA    0.913    0.913
parameterEstimates(fits[[2]])
#>   lhs op rhs    est    se      z pvalue ci.lower ci.upper
#> 1   m  ~   x  0.383 0.108  3.559  0.000    0.172    0.594
#> 2   y  ~   m  0.208 0.104  1.995  0.046    0.004    0.413
#> 3   y  ~   x -0.172 0.119 -1.442  0.149   -0.406    0.062
#> 4   m ~~   m  0.988 0.140  7.071  0.000    0.714    1.262
#> 5   y ~~   y  1.078 0.153  7.071  0.000    0.780    1.377
#> 6   x ~~   x  0.854 0.000     NA     NA    0.854    0.854
parameterEstimates(sim_out_all[[2]]$extra$fit)
#>   lhs op rhs    est    se      z pvalue ci.lower ci.upper
#> 1   m  ~   x  0.383 0.108  3.559  0.000    0.172    0.594
#> 2   y  ~   m  0.208 0.104  1.995  0.046    0.004    0.413
#> 3   y  ~   x -0.172 0.119 -1.442  0.149   -0.406    0.062
#> 4   m ~~   m  0.988 0.140  7.071  0.000    0.714    1.262
#> 5   y ~~   y  1.078 0.153  7.071  0.000    0.780    1.377
#> 6   x ~~   x  0.854 0.000     NA     NA    0.854    0.854
```
