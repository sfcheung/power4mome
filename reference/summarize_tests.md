# Summarize Test Results

Extract and summarize test results.

## Usage

``` r
summarize_tests(
  object,
  collapse = c("none", "all_sig", "at_least_one_sig", "at_least_k_sig"),
  at_least_k = 1,
  merge_all_tests = FALSE,
  p_adjust_method = "none",
  alpha = 0.05
)

# S3 method for class 'test_summary_list'
print(x, digits = 3, ...)

# S3 method for class 'test_summary'
print(x, digits = 2, ...)

# S3 method for class 'test_out_list'
print(x, digits = 3, test_long = FALSE, ...)
```

## Arguments

- object:

  A `power4test` object or the element `test_all` in a `power4test`
  object.

- collapse:

  Whether a single decision (significant vs. not significant) is made
  across all tests for a test that consists of several tests (e.g., the
  tests of several parameters). If `"none"`, tests will be summarized
  individually. If `"all_sig"`, then the set of tests is considered
  significant if all individual tests are significant. If
  `"at_least_one_sig"`, then the set of tests is considered significant
  if at least one of the tests is significant. If `"at_least_k_sig"`,
  then the set of tests is considered significant if at least `k` tests
  are significant, `k` set by the argument `at_least_k`.

- at_least_k:

  Used by `collapse`, the number of tests required to be significant for
  the set of tests to be considered significant.

- merge_all_tests:

  If `TRUE`, all the tests in each replication will be merged into one
  test.

- p_adjust_method:

  The method to be passed to
  [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) to adjust the
  *p*-values when testing the effects. Default is `"none"` and the
  *p*-values will not be adjusted. The unadjusted *p*-values will be
  stored in the column `pvalue_org`. Ignored if some tests do not have
  *p*-values stored. NOTE: Use this only if all tests can be conducted
  using *p*-values.

- alpha:

  The level of significance to use when using `p_adjust_method`. The
  significance results (the column `sig`) will be updated using the
  adjusted *p*-values. Used only if `p_adjust_method` is not `"none"`.

- x:

  The object to be printed.

- digits:

  The numbers of digits after the decimal when printing numeric results.

- ...:

  Optional arguments. Not used.

- test_long:

  If `TRUE`, a detailed report will be printed.

## Value

The function `summarize_tests()` returns a list of the class
`test_summary_list`. Each element contains a summary of a test stored.
The elements are of the class `test_summary`, with these elements:

- `test_attributes`: The stored information of a test, for printing.

- `nrep`: The number of datasets (replications).

- `mean`: The means of numeric information. For significance tests,
  these are the rejection rates.

- `nvalid`: The number of non-`NA` replications used to compute each
  mean.

The `print` methods returns `x` invisibly. They are called for their
side effects.

## Details

The function `summarize_tests()` is used to extract information from
each test stored in a `power4test` object.

The method `print.test_out_list()` is used to print the content of a
list of test stored in a `power4test` object, with the option to print
just the names of tests.

## The role of `summarize_tests()` and related functions

The function `summarize_tests()` and related print methods are used by
the all-in-one function
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
and its `summary` method. Users usually do not call them directly,
though developers can use this function to develop other functions for
power analysis, or to build their own workflows to do the power
analysis.

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

es <-
"
y ~ m: l
m ~ x: m
y ~ x: n
"

# Simulated datasets

sim_only <- power4test(nrep = 2,
                       model = mod,
                       pop_es = es,
                       n = 100,
                       do_the_test = FALSE,
                       iseed = 1234)
#> Simulate the data:
#> Fit the model(s):

# Test the parameters in each dataset

test_out <- power4test(object = sim_only,
                       test_fun = test_parameters)
#> Do the test: test_parameters: CIs  

# Print the summary

summarize_tests(test_out)
#> 
#> =============== <test_parameters: CIs > ===============
#> 
#> Mean(s) across replication:
#>   test_label lhs op rhs    est    se      z pvalue   cilo  cihi   sig
#> 1        m~x   m  ~   x  0.382 0.103  3.698  0.000  0.179 0.585 1.000
#> 2        y~m   y  ~   m  0.443 0.094  4.715  0.000  0.259 0.627 1.000
#> 3        y~x   y  ~   x -0.029 0.104 -0.226  0.236 -0.232 0.174 0.000
#> 4       m~~m   m ~~   m  0.945 0.134  7.071  0.000  0.683 1.207 1.000
#> 5       y~~y   y ~~   y  0.836 0.118  7.071  0.000  0.604 1.068 1.000
#> 
#> - The column 'sig' shows the rejection rates.
#> - If the null hypothesis is false, the rate is the power.
#> - Number of valid replications for rejection rate(s): 2 
#> - Proportion of valid replications for rejection rate(s): 1.000 
```
