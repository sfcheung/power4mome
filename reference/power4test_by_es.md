# Power By Effect Sizes

Estimate power for a set of effect sizes (population values of a model
parameter).

## Usage

``` r
power4test_by_es(
  object,
  pop_es_name = NULL,
  pop_es_values = NULL,
  progress = TRUE,
  ...,
  by_seed = NULL,
  by_nrep = NULL,
  save_sim_all = TRUE
)

# S3 method for class 'power4test_by_es'
c(..., sort = TRUE, skip_checking_models = FALSE)

as.power4test_by_es(original_object, pop_es_name)

# S3 method for class 'power4test_by_es'
print(x, print_all = FALSE, digits = 3, ...)
```

## Arguments

- object:

  A `power4test` object, or a `power4test_by_es` object. If it is a
  `power4test_by_es` object, the first element, which is a `power4test`
  object, will be used as the value of this argument.

- pop_es_name:

  The name of the parameter. See the help page of
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md)
  on the names for the argument `pop_es`.

- pop_es_values:

  A numeric vector of the population values of the parameter specified
  in `pop_es_names`.

- progress:

  Logical. Whether progress of the simulation will be displayed.

- ...:

  For `power4test_by_es()`, they are arguments to be passed to
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
  For `c.power4test_by_es()`, they are `power4test_by_es()` outputs to
  be combined together. For the `print` method of the output of
  `power4test_by_es()`, they are arguments to be passed to the `print`
  method of the output of
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  ([`print.power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)).

- by_seed:

  If set to a number, it will be used to generate the seeds for each
  call to
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
  If `NULL`, the default, then seeds will still be randomly generated
  but the results cannot be easily reproduced.

- by_nrep:

  If set to a number, it will be used to generate the number of
  replications (`nrep`) for each call to
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
  If set to a numeric vector of the same length as `pop_es_values`, then
  these are the `nrep` values for each of the calls, allowing for
  different numbers of replications for the population values. If
  `NULL`, the default, then the original `nrep` will be used. This
  argument is used by
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  for efficiency, and is rarely used when calling this function
  directly.

- save_sim_all:

  If `FALSE`, the dataset in the `power4test` objects are not saved, to
  reduce the size of the output. Default is `TRUE`.

- sort:

  WHen combining the objects, whether they will be sorted by population
  values. Default is `TRUE`.

- skip_checking_models:

  Whether the check of the data generation model will be checked.
  Default is `TRUE`. Should be set to `FALSE` only when users are
  certain the they are based on the same model, or when the model is not
  saved (e.g., `save_sim_all` set to `FALSE` when the objects were
  generated). This argument is used by
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  for efficiency, and is rarely used when calling the `c` method
  directly.

- original_object:

  The object to be converted to a `power4test_by_es` object.

- x:

  The object to be printed.

- print_all:

  If `TRUE`, all elements in `x`, that is, the results of all sample
  sizes examined, will be printed. If `FALSE`, then only those of the
  first value of the parameter will be printed.

- digits:

  The numbers of digits displayed after the decimal.

## Value

The function `power4test_by_es()` returns a `power4test_by_es` object,
which is a list of `power4test` objects, one for each population value
of the parameter.

The method `c.power4test_by_es()` returns a `power4test_by_es` object
with all the elements (tests for different values of `pop_es_values`)
combined.

The function `as.power4test_by_es()` returns a `power4test_by_es` object
converted from the input object.

The `print`-method of `power4test_by_es` objects returns the object
invisibly. It is called for its side-effect.

## Details

The function `power4test_by_es()` regenerates datasets for a set of
effect sizes (population values of a model parmeter) and does the stored
tests in each of them.

Optionally, it can also be run on a object with no stored tests. In this
case, additional arguments must be set to instruct
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
on the tests to be conducted.

It is usually used to examine the power over a sets of effect sizes
(population values).

The `c` method of `power4test_by_es` objects is used to combine tests
from different runs of `power4test_by_es()`.

The function `as.power4test_by_es()` is used to convert a `power4test`
object to a `power4test_by_es` object, if it is not already one. Useful
when concatenating `power4test` objects with `power4test_by_es` objects.

## See also

[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)

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

sim_only <- power4test(nrep = 2,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       R = 40,
                       ci_type = "boot",
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234)
#> Simulate the data:
#> Fit the model(s):
#> Generate bootstrap estimates:

test_out <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE,
                                        mc_ci = FALSE))
#> Do the test: test_indirect: x->m->y 

out <- power4test_by_es(test_out,
                            pop_es_name = "y ~ m",
                            pop_es_values = c(.10, .20))
#> 
#> Updating the simulation for new value: y ~ m = 0.100 
#> Re-simulate the data:
#> Fit the model(s):
#> Generate bootstrap estimates:
#> Update the test(s):
#> Update test_indirect: x->m->y :
#> 
#> Updating the simulation for new value: y ~ m = 0.200 
#> Re-simulate the data:
#> Fit the model(s):
#> Generate bootstrap estimates:
#> Update the test(s):
#> Update test_indirect: x->m->y :
out_reject <- rejection_rates(out)
out_reject
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     par    es   est   p.v reject r.cilo r.cihi
#> 1 y ~ m 0.100 0.043 1.000  0.000 -0.000  0.658
#> 2 y ~ m 0.200 0.038 1.000  0.500  0.095  0.905
#> Notes:
#> - par: The parameter being varied.
#> - es: The population value of 'par' in a trial.
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
```
