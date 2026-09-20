# Get a Fit Object From a 'power4test' Object

A helper to get the fit object (e.g., a `lavaan` output) for a
replication the output of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and friends.

## Usage

``` r
get_sim_fit(
  object,
  which = "fit",
  fit_class = c("lavaan", "lm_list_lmhelprs"),
  i = 1
)
```

## Arguments

- object:

  A `power4test` object, such as the output of
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

- which:

  The name of the fit object to be retrieved. If set to `NULL`, it
  returns the names of supported fit objects.

- fit_class:

  A character vector of the classes of fit objects to be retrieved.

- i:

  The replication from which the fit object is to be retrieved.

## Value

If a specific object is requested, it returns the fit object, such as
the output of [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)
or
[`lmhelprs::many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.html).

If `which` is set to `NULL`, then it returns a character vector of the
names of the supported fit objects.

## Details

There are cases in which users would like to examine the fit results in
a replication. If the sample size of a replication is large enough, the
fit results can also be used to check the specification of the model.
The helper `get_sim_fit()` is for extracting the stored fit results from
the output of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and friends.

## See also

See
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
for the all-in-one function, on which this function is to be used.

## Examples

``` r

# Specify the model

model_simple_med <-
"
m ~ x
y ~ m + x
"

# Specify the population values

model_simple_med_es <-
"
m ~ x: m
y ~ m: l
y ~ x: n
"

# Just a test with only two replications
out <- power4test(nrep = 2,
                  model = model_simple_med,
                  pop_es = model_simple_med_es,
                  n = 100,
                  test_fun = test_parameters,
                  test_args = list(pars = "m~x"),
                  iseed = 1234,
                  parallel = FALSE,
                  progress = TRUE)
#> Recommend setting 'parallel' to TRUE for faster analysis
#> Simulate the data:
#> Fit the model(s):
#> Do the test(s): test_parameters: CIs (pars: m~x) 

get_sim_fit(out)
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
```
