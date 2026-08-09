# Test Model Fit

"Test" the model fit of a model.

## Usage

``` r
test_fit_measure(
  fit = fit,
  model_to_fit = NULL,
  fit_measure = "chisq",
  sig_value = c(chisq = "pvalue", chisq.scaled = "pvalue.scaled", rmsea = "rmsea.pvalue",
    rmsea.scaled = "rmsea.pvalue.scaled", rmsea.robust = "rmsea.pvalue.robust"),
  sig_if = "<.05",
  check_post_check = TRUE,
  refit_args = list(se = "none"),
  always_refit = FALSE,
  fitmeasures_args = list(),
  override_measurement_model = FALSE,
  model_measurement = NULL,
  fit_name = "fit",
  get_map_names = FALSE,
  get_test_name = FALSE
)
```

## Arguments

- fit:

  The fit object. Must be the output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
  its wrappers, such as
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- model_to_fit:

  The model to be fitted, specified by `lavaan` model syntax. Can
  contain only the structural part, with the measurement part, if any,
  retrieved from the data generation model. If `NULL`, then the model
  used by
  [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
  will be used.

- fit_measure:

  The name of the fit measure to be used to do the test. Must be a name
  that can be accepted by
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
  It can also be the test statistic, such as the model chi-square.

- sig_value:

  The name of the element of the output of
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
  to be used to do the test. Must be a name that can be accepted by
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
  Used when the value used to do the test (e.g., a p-value) is different
  from the value specified in `fit_measure` (e.g., `"pvalue"` is the
  p-value for model chi-square, `"chisq"`). It can be a named character
  vector, with the names being possible values for `fit_measure`. The
  name to be used will then be retrieved based on `fit_measure`.

- sig_if:

  The criterion for doing the test, as a character string. For example,
  it is `"<.05"` if the test is "significant" when the p-value is less
  than .05. It is `"<.95"` if the "test" is "significant" when the fit
  measure (e.g., CFI) is less than .95. It must be a string that, if
  appended to the value retrieved by `sig_value`, is an expression that
  can be evaluated in R (e.g., ".023\<.05").

- check_post_check:

  Logical. If `TRUE`, the default, and the model is fitted by `lavaan`,
  the test will be conducted only if the model passes the `post.check`
  conducted by
  [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
  (with `what = "post.check"`).

- refit_args:

  A named list of arguments to be passed to
  [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
  if `model_to_fit` is set. If `model_to_fit` is `NULL` and
  `always_refit` is `TRUE`, this list of argument values will be used
  when calling
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html), overriding
  values stored, if any.

- always_refit:

  Whether the model will always be fitted again. If `TRUE` and
  `model_to_fit` is `NULL`, then the stored model will be fitted again,
  but with `refit_args` used. Ignored if `model_to_fit` is explicitly
  set to a `lavaan` model because this model will always be fitted to
  the data. Useful when the same stored model is fitted but with
  different argument values.

- fitmeasures_args:

  A named list of arguments to be passed to
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).

- override_measurement_model:

  Whether `model_to_fit` already has the measurement part and so will
  override the stored measurement part of the model, if any. If `FALSE`,
  `model_to_fit` can only specify the structural part of the model.

- model_measurement:

  The model syntax for the measurement model. Ignored because its value
  will be determined by
  [`do_test()`](https://sfcheung.github.io/power4mome/reference/do_test.md).

- fit_name:

  The name of the model fit object to be extracted. Default is `"fit"`.
  Used only when more than one model is fitted in each replication. This
  should be the name of the model on which the test is to be conducted.

- get_map_names:

  Logical. Used by
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  to determine how to extract stored information and assign them to this
  function. Users should not use this argument.

- get_test_name:

  Logical. Used by
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  to get the default name of this test. Users should not use this
  argument.

## Value

In its normal usage, it returns a one-row data frame with the following
columns:

- `est`: The fit measure used for the "test", such as the model
  chi-square or the CFI.

- `cilo` and `cihi`: `NA`. Not used.

- `sig`: Whether the "test" is significant. That is, whether the
  criterion is met (e.g., the *p*-value of the model chi-square is less
  than .05, or the CFI is less than .90).

- `test_label`: An automatically generated label for the test.

## Details

This function is to be used in
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
for "testing" the model fit of a model , by setting it to the `test_fun`
argument.

## What "Test" Means for This Function

The term "test" is used in this function merely to be consistent with
other test functions. What this function does is to check whether a
certain numeric criterion based on a fit measure is met.

If the fit measure is the model chi-square, then this is a test in the
conventional sense, using the *p*-value of the chi-square. Similarly,
the test of close fit using RMSEA is also a test.

However, the fit measure can also be a descriptive measure such as CFI
or TLI. A descriptive measure is not used to "test" the goodness of fit
of a model. Nevertheless, we can still estimate the probability that
this measure meets a criterion (e.g., CFI less than .90). The function
`test_fit_measure()` can be used for this purpose. The empirical
"rejection rate" is then the proportion of replications with this fit
measure meeting the criterion.

## Typical Scenarios

### The Fit Measure "Test" for the Fitted Model

When used with
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
this function can be used to estimate the "rejection rate" of a
criterion (e.g., the *p*-value of the model chi-square less than .05, or
the CFI less than .90) for the model fitted when calling
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

### The Fit Measure "Test" for an Alternative Model

This function can also be used to estimate the "rejection rate" of a
criterion when a model different from the stored fitted model. For
example, the data generation model is a simple mediation model with a
non-nil direct path from the independent variable to the outcome.
However, we want to estimate the rejection rate using "CFI\<.90" when a
complete mediation model (the direct path is fixed to zero) is fitted.
This can be done by setting this model to the argument `model_to_fit`.

## See also

[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)

## Examples

``` r

# Specify the model

mod <-
"
m ~ x
y ~ m + x
"

# Specify the population values

mod_es <-
"
y ~ m: l
m ~ x: s
y ~ x: m
"

# Simulate the data

sim_only <- power4test(
  nrep = 2,
  model = mod,
  pop_es = mod_es,
  n = 100,
  iseed = 1234
)
#> Recommend setting 'parallel' to TRUE for faster analysis
#> Simulate the data:
#> Fit the model(s):

# Do the tests in each replication

mod_complete <-
"
m ~ x
y ~ m
"

test_out <- power4test(
  object = sim_only,
  test_fun = test_fit_measure,
  test_args = list(
    model_to_fit = mod_complete,
    fit_measure = "cfi",
    sig_if = "<.90"
  )
)
#> Recommend setting 'parallel' to TRUE for faster analysis
#> Do the test: test_fit_measure 

print(test_out,
      test_long = TRUE)
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
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.100  
#>   y ~                        
#>     m                 0.500  
#>     x                 0.300  
#> 
#> Variances:
#>                    Population
#>    .m                 0.990  
#>    .y                 0.630  
#>     x                 1.000  
#> 
#> (Computing indirect effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>               ind
#> x -> m -> y 0.050
#> x -> y      0.300
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  2 
#> Sample Sizes:  100 
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
#> lavaan 0.7-2 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> ================== <test_fit_measure> ==================
#> 
#> Mean(s) across replication:
#>     test_label   est cilo cihi   sig pvalue
#> 1 cfi(cfi<.90) 0.806   NA   NA 0.500     NA
#> 
#> - The column 'sig' shows the rejection rates.
#> - If the null hypothesis is false, the rate is the power.
#> - Number of valid replications for rejection rate(s): 2 
#> - Proportion of valid replications for rejection rate(s): 1.000 

```
