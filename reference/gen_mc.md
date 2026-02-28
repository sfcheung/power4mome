# Generate Monte Carlo Estimates

Get a list of the output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and generate
Monte Carlo estimates of model parameters.

## Usage

``` r
gen_mc(
  fit_all,
  R = 100,
  ...,
  iseed = NULL,
  parallel = FALSE,
  progress = FALSE,
  ncores = max(1, parallel::detectCores(logical = FALSE) - 1),
  compute_implied_stats = FALSE,
  cl = NULL
)
```

## Arguments

- fit_all:

  The output of
  [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)
  or an object of the class `fit_out`.

- R:

  The number of replications to generate the Monte Carlo estimates for
  each fit output.

- ...:

  Optional arguments to be passed to
  [`manymome::do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.html)
  when generating the Monte Carlo estimates.

- iseed:

  The seed for the random number generator. Default is `NULL` and the
  seed is not changed.

- parallel:

  If `TRUE`, parallel processing will be used to generate Monte Carlo
  estimates for the fit outputs. Default is `FALSE`.

- progress:

  If `TRUE`, the progress will be displayed. Default is \`FALSE.

- ncores:

  The number of CPU cores to use if parallel processing is used.

- compute_implied_stats:

  Whether implied statistics are computed in each Monte Carlo
  replication. Usually not needed and so default to `FALSE`.

- cl:

  A cluster, such as one created by
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  If `NULL`, a cluster will be created, but will be stopped on exit. If
  set to an existing cluster, it will not be stopped when the function
  exits; users need to stop it manually.

## Value

An `mc_list` object, which is a list of the output of
[`manymome::do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.html).

## Details

The function `gen_mc()` simply calls
[`manymome::do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.html)
on each output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) in `fit_all`.
The simulated estimates can then be used to test effects such as
indirect effects, usually by functions from the `manymome` package, such
as
[`manymome::indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.html).

## The role of `gen_mc()`

This function is used by the all-in-one function
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
Users usually do not call this function directly, though developers can
use this function to customize the workflow of the power analysis.

## See also

See
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
for the all-in-one function that uses this function.

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

# Generate Monte Carlo estimates for each replication

mc_all <- gen_mc(fit_all,
                 R = 100,
                 iseed = 4567)

mc_all
#> [[1]]
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 100 
#> 
#> 
#> [[2]]
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 100 
#> 
#> 
#> [[3]]
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 100 
#> 
#> 
#> [[4]]
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 100 
#> 
#> 
#> [[5]]
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 100 
#> 
#> 
#> attr(,"class")
#> [1] "mc_list" "list"   
```
