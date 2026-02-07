# Rejection Rates

Get all rejection rates of all tests stored in a `power4test` object or
other supported objects.

## Usage

``` r
rejection_rates(object, ...)

# Default S3 method
rejection_rates(object, ...)

# S3 method for class 'power4test'
rejection_rates(
  object,
  all_columns = FALSE,
  ci = TRUE,
  level = 0.95,
  se = FALSE,
  collapse = NULL,
  at_least_k = NULL,
  merge_all_tests = NULL,
  p_adjust_method = NULL,
  alpha = NULL,
  ...
)

# S3 method for class 'power4test_by_es'
rejection_rates(
  object,
  all_columns = FALSE,
  ci = TRUE,
  level = 0.95,
  se = FALSE,
  ...
)

# S3 method for class 'power4test_by_n'
rejection_rates(
  object,
  all_columns = FALSE,
  ci = TRUE,
  level = 0.95,
  se = FALSE,
  ...
)

# S3 method for class 'rejection_rates_df'
print(x, digits = 3, annotation = TRUE, abbreviate_col_names = TRUE, ...)
```

## Arguments

- object:

  The object from which the rejection rates are to be computed, such as
  a `power4test` object, a `power4test_by_n` object, or a
  `power4test_by_es` object.

- ...:

  Optional arguments. For the `print` method, these arguments will be
  passed to the `print` method of `data.frame` objects
  [`print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html).
  Not used by other methods.

- all_columns:

  If `TRUE`, all columns stored by a test will be extracted. Default is
  `FALSE` and only essential columns related to power will be printed.

- ci:

  If `TRUE`, confidence intervals for the rejection rates (column
  `reject` or `sig`) will be computed. The method is determined by the
  option `power4mome.ci_method`. If `NULL` or `"wilson"`,
  Wilson's (1927) method is used. If `"norm"`, normal approximation is
  used.

- level:

  The level of confidence for the confidence intervals, if `ci` is
  `TRUE`. Default is .95, denoting 95%.

- se:

  If `TRUE`, standard errors for the rejection rates (column `reject` or
  `sig`) will be computed. Normal approximation is used to compute the
  standard errors.

- collapse:

  Whether a single decision (significant vs. not significant) is made
  across all tests for a test that consists of several tests (e.g., the
  tests of several parameters). If `"none"`, tests will be summarized
  individually. If `"all_sig"`, then the set of tests is considered
  significant if all individual tests are significant. If
  `"at_least_one_sig"`, then the set of tests is considered significant
  if at least one of the tests is significant. If `"at_least_k_sig"`,
  then the set of tests is considered significant if at least `k` tests
  are significant, `k` set by the argument `at_least_k`. If `NULL`, will
  use the value stored in `object` (default is `"none"`).

- at_least_k:

  Used by `collapse`, the number of tests required to be significant for
  the set of tests to be considered significant. If `NULL`, will use the
  value stored in `object` (default is 1).

- merge_all_tests:

  If `TRUE`, all the tests in each replication will be merged into one
  test. If `NULL`, will use the value stored in `object` (default is
  `FALSE`).

- p_adjust_method:

  The method to be passed to
  [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) to adjust the
  *p*-values when testing the effects. Default is `"none"` and the
  *p*-values will not be adjusted. Ignored if some tests do not have
  *p*-values stored. NOTE: Use this only if all tests can be conducted
  using *p*-values. If `NULL`, will use the value stored in `object`
  (default is `"none"`).

- alpha:

  The level of significance to use when using `p_adjust_method`. The
  significance results (the column `sig`) will be updated using the
  adjusted *p*-values. Used only if `p_adjust_method` is not `"none"`.
  If `NULL`, will use the value stored in `object` (default is .05).

- x:

  The `rejection_rates_df` object to be printed.

- digits:

  The number of digits to be printed after the decimal.

- annotation:

  Logical. Whether additional notes will be printed.

- abbreviate_col_names:

  Logical. Whether some column names will be abbreviated.

## Value

The `rejection_rates` method returns a `rejection_rates_df` object, with
a `print` method.

If the input (`object`) is a `power4test` object, the
`rejection_rates_df` object is a data-frame like object with the number
of rows equal to the number of tests. Note that some tests, such as the
test by
[`test_parameters()`](https://sfcheung.github.io/power4mome/reference/test_parameters.md),
conduct one test for each parameter. Each such test is counted as one
test.

The data frame has at least these columns:

- `test`: The name of the test.

- `label`: The label for each test, or `"Test"` if a test only does one
  test (e.g.,
  [`test_indirect_effect()`](https://sfcheung.github.io/power4mome/reference/test_indirect_effect.md)).

- `pvalid`: The proportion of valid tests across all replications.

- `reject`: The rejection rate for each test. If the null hypothesis is
  false, then this is the power.

The `rejection_rates` method for `power4test_by_es` objects returns an
object of the class `rejection_rates_df_by_es`, which is a subclass of
`rejection_rates_df`. It is a data frame which is similar to the output
of `rejection_rates()`, with two columns added for the effect size
(`pop_es_name` and `pop_es_values`) for each test.

The `rejection_rates` method for `power4test_by_n` objects returns an
object of the class `rejection_rates_df_by_n`, which is a subclass of
`rejection_rates_df`. It is a data frame which is similar to the output
of a `power4test` object, with a column `n` added for the sample size
for each test.

The `print` method of a `rejection_rates_df` object returns the object
invisibly. It is called for its side-effect.

## Details

For a `power4test` object, rejection_rates loops over the tests stored
in a `power4test` object and retrieves the rejection rate of each test.

The `rejection_rates` method for `power4test_by_es` objects is used to
compute the rejection rates from a `power4test_by_es` object, with
effect sizes added to the output.

The `rejection_rates` method for `power4test_by_n` objects is used to
compute the rejection rates, with sample sizes added to the output.

## References

Wilson, E. B. (1927). Probable inference, the law of succession, and
statistical inference. *Journal of the American Statistical Association,
22*(158), 209-212.
[doi:10.1080/01621459.1927.10502953](https://doi.org/10.1080/01621459.1927.10502953)

## See also

[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
[`power4test_by_n()`](https://sfcheung.github.io/power4mome/reference/power4test_by_n.md),
and
[`power4test_by_es()`](https://sfcheung.github.io/power4mome/reference/power4test_by_es.md),
which are supported by this method.

## Examples

``` r
# Specify the population model

model_simple_med <-
"
m ~ x
y ~ m + x
"

# Specify the effect sizes (population parameter values)

model_simple_med_es <-
"
y ~ m: l
m ~ x: m
y ~ x: n
"

# Generate some datasets to check the model

sim_only <- power4test(nrep = 4,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 100,
                       R = 50,
                       ci_type = "boot",
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234)
#> Simulate the data:
#> Fit the model(s):
#> Generate bootstrap estimates:

# Do the test 'test_indirect_effect' on each datasets

test_out <- power4test(object = sim_only,
                       test_fun = test_indirect_effect,
                       test_args = list(x = "x",
                                        m = "m",
                                        y = "y",
                                        boot_ci = TRUE,
                                        mc_ci = FALSE))
#> Do the test: test_indirect: x->m->y 

# Do the test 'test_parameters' on each datasets
# and add the results to 'test_out'

test_out <- power4test(object = test_out,
                       test_fun = test_parameters)
#> Do the test: test_parameters: CIs  

# Compute and print the rejection rates for stored tests

rejection_rates(test_out)
#>                     test test_label    est   p.v reject r.cilo r.cihi
#> 1 test_indirect: x->m->y       Test  0.149 1.000  1.000  0.510  1.000
#> 2  test_parameters: CIs         m~x  0.317 1.000  1.000  0.510  1.000
#> 3  test_parameters: CIs         y~m  0.475 1.000  1.000  0.510  1.000
#> 4  test_parameters: CIs         y~x -0.080 1.000  0.000  0.000  0.490
#> Notes:
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.

# See the help pages of power4test_by_n() and power4test_by_es()
# for other examples.
```
