# Common Data Processing

For the `process_data` argument. It do three types of processing in one
step: Creating ordinal variables, generating missing values, and
computing scales scores.

## Usage

``` r
common_processes(
  data,
  cut_patterns = NULL,
  cuts = NULL,
  missing_values_args = list(),
  prop = 0.5,
  mech = "MCAR",
  method = c("mean", "sum"),
  na.rm = FALSE
)
```

## Arguments

- data:

  A data frame.

- cut_patterns:

  A named vector. The names are the names of the latent variables for
  which indicator scores will be converted. Each value must be the name
  of one of the built-in patterns (call
  [`cut_patterns()`](https://sfcheung.github.io/power4mome/reference/ordinal_variables.md)
  to list the patterns and their names). Can be used with `cuts` but a
  latent variable should appear only either in `cut_patterns` or `cuts`,
  not both.

- cuts:

  A named list. The names are the names of the latent variables for
  which indicator scores will be converted. Each element is a vector of
  the thresholds for the conversion. `-Inf` and `Inf` will be
  automatically included during the conversion. Can be used with
  `cut_patterns` but a latent variable should appear only either in
  `cut_patterns` or `cuts`, not both.

- missing_values_args:

  A named list of optional arguments to be passed to
  [`mice::ampute()`](https://amices.org/mice/reference/ampute.html).
  Note that `prop` and `mech` will override the values set for them in
  `missing_values_args`, if any.

- prop:

  The proportion of missingness. Default is 0.5, about 50% of the cases
  have missing data.

- mech:

  The missing data mechanism. Default is `"MCAR"` (missing completely at
  random). Other possible values are `"MAR"` (missing at random) and
  `"MNAR"` (missing not at random). Please refer to the help of
  [`mice::ampute()`](https://amices.org/mice/reference/ampute.html) for
  details.

- method:

  The method to be used to compute the scale scores. Can be `"mean"` or
  `"sum"`. Implemented by [`mean()`](https://rdrr.io/r/base/mean.html)
  and [`sum()`](https://rdrr.io/r/base/sum.html).

- na.rm:

  How missing value (`NA`) are handled. Default is `FALSE`, the same
  default value for [`mean()`](https://rdrr.io/r/base/mean.html) and
  [`sum()`](https://rdrr.io/r/base/sum.html).

## Value

The function returns a data frame with the processed data.

## Details

This function is to be used in the `process_data` argument of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

This function is simply a wrapper of the following three functions:

- [`ordinal_variables()`](https://sfcheung.github.io/power4mome/reference/ordinal_variables.md)

- [`missing_values()`](https://sfcheung.github.io/power4mome/reference/missing_values.md)

- [`scale_scores()`](https://sfcheung.github.io/power4mome/reference/scale_scores.md)

Please refer to these functions on how the raw data is processed.

## See also

[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
See also
[`ordinal_variables()`](https://sfcheung.github.io/power4mome/reference/ordinal_variables.md),
[missing_values](https://sfcheung.github.io/power4mome/reference/missing_values.md),
and
[scale_scores](https://sfcheung.github.io/power4mome/reference/scale_scores.md)
for the processes employed.

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
m ~ x: m
y ~ x: n
"

# Specify the numbers of indicators and reliability coefficients

k <- c(y = 3,
       m = 4,
       x = 5)
rel <- c(y = .70,
         m = .70,
         x = .70)

# Simulate the data

out <- power4test(
         nrep = 2,
         model = mod,
         pop_es = mod_es,
         n = 200,
         number_of_indicators = k,
         reliability = rel,
         process_data = list(
           fun = common_processes,
           args = list(
                     prop = .75,
                     cut_patterns = c(x = "-ma3", y = "-ma3", m = "-ma3")
                   )
           ),
         test_fun = test_parameters,
         test_args = list(op = "~"),
         parallel = FALSE,
         iseed = 1234)
#> Recommend setting 'parallel' to TRUE for faster analysis
#> Simulate the data:
#> Fit the model(s):
#> Do the test: test_parameters: CIs (op: ~) 

dat <- pool_sim_data(out)
head(dat, 50)
#>           y    m   x
#> 1  1.333333   NA 1.8
#> 2  2.666667   NA 1.8
#> 3  1.666667 2.50  NA
#> 4  2.333333   NA 2.0
#> 5  2.333333 2.75  NA
#> 6  2.000000 2.50  NA
#> 7  2.666667 2.00 2.0
#> 8  2.333333 1.50  NA
#> 9        NA 1.75 2.2
#> 10 1.666667 1.50  NA
#> 11 2.666667   NA 1.6
#> 12 2.333333 1.25  NA
#> 13       NA 1.75 1.8
#> 14       NA 1.50 2.2
#> 15 2.000000 2.50 2.0
#> 16 2.000000 2.00 2.6
#> 17       NA 2.50 3.0
#> 18 1.666667   NA 2.0
#> 19 1.333333 1.75  NA
#> 20       NA 3.00 2.0
#> 21 2.333333   NA 2.0
#> 22       NA 2.00 2.6
#> 23 2.333333 1.75  NA
#> 24 2.666667 2.75  NA
#> 25 2.333333 1.50  NA
#> 26 1.666667 1.25 2.2
#> 27       NA 2.75 2.4
#> 28 2.333333 1.75  NA
#> 29 1.666667   NA 1.6
#> 30 1.666667 2.25 1.8
#> 31 2.666667 2.50 1.2
#> 32 1.666667 2.00  NA
#> 33 1.000000   NA 1.4
#> 34       NA 1.50 2.0
#> 35       NA 1.25 2.0
#> 36 1.333333 1.50  NA
#> 37 1.333333 2.25  NA
#> 38 2.000000 2.00 2.4
#> 39 2.000000 1.75 1.8
#> 40       NA 1.75 2.0
#> 41 2.666667 3.00  NA
#> 42 1.666667 1.75 2.0
#> 43 1.000000 2.00  NA
#> 44 2.666667   NA 1.4
#> 45 2.333333   NA 1.4
#> 46 1.666667 1.75 2.2
#> 47 2.333333 2.00 2.4
#> 48 1.666667 1.75 2.2
#> 49 2.000000   NA 2.2
#> 50 1.666667   NA 2.6
```
