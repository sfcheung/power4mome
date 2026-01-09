# Fit a Model to a List of Datasets

Get the output of
[`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md)
and fit a model to each of the stored datasets.

## Usage

``` r
fit_model(
  data_all = NULL,
  model = NULL,
  fit_function = "lavaan",
  arg_data_name = "data",
  arg_model_name = "model",
  arg_group_name = "group",
  ...,
  fit_out = NULL,
  parallel = FALSE,
  progress = FALSE,
  ncores = max(1, parallel::detectCores(logical = FALSE) - 1)
)
```

## Arguments

- data_all:

  The output of
  [`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md),
  or a `sim_data` class object.

- model:

  The model to be fitted. If `NULL`, the default, the model stored in
  `data_all`, which should be the data generation model, will be used.

- fit_function:

  The function to be used to fit the model. Can also be a string:
  `"lavaan"` (the default) for
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html), and `"lm"`
  or `many_lm` for
  [`lmhelprs::many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.html).
  Other functions can also be used if necessary.

- arg_data_name:

  The name of the argument of `fit_function` expecting the dataset.
  Default is `"data"`.

- arg_model_name:

  The name of the argument of `fit_function` expecting the model
  definition. Default is `"model"`.

- arg_group_name:

  The name of the argument of `fit_function` expecting the name of the
  group variable. Used only for multigroup models. Default is `"group"`.

- ...:

  Optional arguments to be passed to `fit_function` when fitting the
  model.

- fit_out:

  If set to a `fit_out` object (the output of `fit_model()`), then all
  missing arguments will be retrieved from `fit_out`. That is, users can
  use `fit_model(data_all = new_data, fit_out = old_out)` to re-fit a
  model originally fitted in `old_out` on a new list of dataset
  (`new_data`). No need to include all other arguments.

- parallel:

  If `TRUE`, parallel processing will be used to fit the models. Default
  is `FALSE`.

- progress:

  If `TRUE`, the progress of model fitting will be displayed. Default is
  \`FALSE.

- ncores:

  The number of CPU cores to use if parallel processing is used.

## Value

An object of the class `fit_out`, which is a list of the output of
`fit_function`
([`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) by default).
If an error occurred when fitting the model to a dataset, then this
element will be the error message from the fit function.

## Details

By default, the function `fit_model()`

- extracts the model stored in the output of
  [`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md),

- fits the model to each dataset simulated using `fit_function`, default
  to `"lavaan"` and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) will be
  called,

- and returns the results.

If the datasets were generated from a multigroup model when calling
[`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md),
a multigroup model is fitted.

## The role of `fit_model()`

This function is used by the all-in-one function
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
Users usually do not call this function directly, though developers can
use this function to customize the model fitting step in power analysis.

## See also

See
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
for the all-in-one function that uses this function, and
[`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md)
for the function generating datasets for this function.

## Examples

``` r
# Specify the population model

mod <-
"m ~ x
 y ~ m + x"

# Specify the effect sizes (population parameter values)

es <-
"
y ~ m: m
m ~ x: m
y ~ x: n
"

# Generate several simulated datasets

data_all <- sim_data(nrep = 5,
                     model = mod,
                     pop_es = es,
                     n = 100,
                     iseed = 1234)

# Fit the population model to each datasets

fit_all <- fit_model(data_all)
fit_all[[1]]
#> lavaan 0.6-21 ended normally after 1 iteration
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

# Fit the population model using the MLR estimator

fit_all_mlr <- fit_model(data_all,
                         estimator = "MLR")
fit_all_mlr[[1]]
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                 0.000       0.000
#>   Degrees of freedom                                 0           0

# Fit a model different from the population model,
# with the MLR estimator

mod2 <-
"m ~ x
 y ~ m"

fit_all_mlr2 <- fit_model(data_all,
                          mod2,
                          estimator = "MLR")
fit_all_mlr2[[1]]
#> lavaan 0.6-21 ended normally after 2 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         4
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                 0.975       1.128
#>   Degrees of freedom                                 1           1
#>   P-value (Chi-square)                           0.323       0.288
#>   Scaling correction factor                                  0.864
#>     Yuan-Bentler correction (Mplus variant)                       
```
