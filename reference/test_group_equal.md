# Test Group Constraints

Test the model fit change when one or more between-group constraints are
imposed.

## Usage

``` r
test_group_equal(
  fit = fit,
  group.equal = NULL,
  group.partial = NULL,
  check_post_check = TRUE,
  ...,
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
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html). The model
  must be a multigroup model.

- group.equal:

  The same argument used by `lavaan`. A character vector with one or
  more of these values: `"regressions"`, `"loadings"`,
  `"lv.covariances"`, `"lv.variances"`, `"intercepts"`, `"means"`,
  `"thresholds"`, `"residual.covariances"`, `"composite.weights"`, and
  `"residuals"`.

- group.partial:

  The same argument used by `lavaan`. The parameters that should be free
  across groups. Used with `group.equal` to exclude some parameters from
  those requested to be equal across groups by `group.equal`.

- check_post_check:

  Logical. If `TRUE`, the default, and the model is fitted by `lavaan`,
  the test will be conducted only if the model passes the `post.check`
  conducted by
  [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
  (with `what = "post.check"`).

- ...:

  Optional arguments to be passed to
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).

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

- `est`: The chi-square difference.

- `cilo` and `cihi`: `NA`. Not used.

- `sig`: Whether the chi-square difference test is significant

- `test_label`: The constraints imposted.

## Details

This function is to be used in
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
for testing the difference in model fit when one or more between-group
constraints are imposed , by setting it to the `test_fun` argument.

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
m ~ x:
  - nil
  - s
y ~ x: nil
"

# Simulate the data

sim_only <- power4test(nrep = 2,
                       model = mod,
                       pop_es = mod_es,
                       n = 100,
                       iseed = 1234)
#> Simulate the data:
#> Fit the model(s):

# Do the tests in each replication

test_out <- power4test(object = sim_only,
                       test_fun = test_group_equal,
                       test_args = list(group.equal = "regressions"))
#> Do the test: test_group_equal 

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
#> ====== Population Values ======
#> 
#> 
#> Group 1 [Group1]:
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.000  
#>   y ~                        
#>     m                 0.500  
#>     x                 0.000  
#> 
#> Variances:
#>                    Population
#>    .m                 1.000  
#>    .y                 0.750  
#>     x                 1.000  
#> 
#> 
#> Group 2 [Group2]:
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.100  
#>   y ~                        
#>     m                 0.500  
#>     x                 0.000  
#> 
#> Variances:
#>                    Population
#>    .m                 0.990  
#>    .y                 0.750  
#>     x                 1.000  
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>                      ind
#> Group1.x -> m -> y 0.000
#> Group2.x -> m -> y 0.050
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  2 
#> Sample Sizes:  200 
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
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        14
#> 
#>   Number of observations per group:                   
#>     Group1                                         100
#>     Group2                                         100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#>   Test statistic for each group:
#>     Group1                                       0.000
#>     Group2                                       0.000
#> 
#> ================== <test_group_equal> ==================
#> 
#> Mean(s) across replication:
#>           test_label   est cilo cihi   sig pvalue
#> 1 equal(regressions) 3.469   NA   NA 0.000  0.343
#> 
#> - The column 'sig' shows the rejection rates.
#> - If the null hypothesis is false, the rate is the power.
#> - Number of valid replications for rejection rate(s): 2 
#> - Proportion of valid replications for rejection rate(s): 1.000 

```
