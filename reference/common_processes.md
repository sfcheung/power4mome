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
  prop = NULL,
  mech = "MCAR",
  method = c("none", "mean", "sum"),
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
  not both. If both `cut_patterns` and `cuts` are set to `NULL`, then
  the original data (`data`) will be returned unchanged.

- cuts:

  A named list. The names are the names of the latent variables for
  which indicator scores will be converted. Each element is a vector of
  the thresholds for the conversion. `-Inf` and `Inf` will be
  automatically included during the conversion. Can be used with
  `cut_patterns` but a latent variable should appear only either in
  `cut_patterns` or `cuts`, not both. If both `cut_patterns` and `cuts`
  are set to `NULL`, then the original data (`data`) will be returned
  unchanged.

- missing_values_args:

  A named list of optional arguments to be passed to
  [`mice::ampute()`](https://amices.org/mice/reference/ampute.html).
  Note that `prop` and `mech` will override the values set for them in
  `missing_values_args`, if any.

- prop:

  The proportion of missingness. Default is 0.5, about 50% of the cases
  have missing data. If set to `NULL`, then the original data (`data`)
  will be returned unchanged.

- mech:

  The missing data mechanism. Default is `"MCAR"` (missing completely at
  random). Other possible values are `"MAR"` (missing at random) and
  `"MNAR"` (missing not at random). Please refer to the help of
  [`mice::ampute()`](https://amices.org/mice/reference/ampute.html) for
  details.

- method:

  The method to be used to compute the scale scores. Can be `"mean"` or
  `"sum"`. Implemented by [`mean()`](https://rdrr.io/r/base/mean.html)
  and [`sum()`](https://rdrr.io/r/base/sum.html). If set to `"none"`,
  then the original data (`data`) will be returned unchanged.

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
#>    y1 y2 y3 m1 m2 m3 m4 x1 x2 x3 x4 x5
#> 1   2  1  1  1  1 NA  1  1  2  1  3  2
#> 2   3  2  3  3 NA  1  2  2  2  2  2  1
#> 3   1  2  2  3  3  2  2  2  1 NA  2  2
#> 4   2  3  2 NA  1  2  1  2  2  1  2  3
#> 5   2  2  3  3  3  2  3  1 NA  1  2  3
#> 6   2  3  1  3  3  2  2 NA  2  2  2  2
#> 7   3  2  3  2  2  3  1  2  1  2  2  3
#> 8   3  2  2  2  2  1  1  2  2  2  1 NA
#> 9  NA  2  2  2  2  1  2  2  2  3  2  2
#> 10  2  1  2  2  2  1  1  1  2  2  2 NA
#> 11  2  3  3  1  2 NA  1  1  1  2  2  2
#> 12  2  2  3  1  1  2  1  1  3 NA  2  2
#> 13  3  2 NA  1  2  2  2  2  2  2  2  1
#> 14  1  1 NA  2  2  1  1  2  3  3  1  2
#> 15  1  3  2  3  2  2  3  1  2  2  2  3
#> 16  2  2  2  2  2  1  3  3  3  2  2  3
#> 17  3  2 NA  2  3  3  2  3  3  3  3  3
#> 18  2  2  1  3  2 NA  2  2  2  2  2  2
#> 19  1  1  2  2  3  1  1  3  2 NA  2  3
#> 20 NA  2  2  3  3  3  3  2  2  3  2  1
#> 21  3  2  2  3 NA  2  2  2  2  2  2  2
#> 22  3 NA  2  2  2  3  1  2  3  2  3  3
#> 23  2  2  3  2  2  2  1  2  1  2  2 NA
#> 24  3  3  2  3  2  3  3  3  3 NA  2  2
#> 25  2  3  2  2  1  2  1  2  2  2  3 NA
#> 26  2  2  1  1  1  2  1  2  3  2  3  1
#> 27 NA  3  3  3  3  2  3  2  3  2  2  3
#> 28  2  2  3  2  2  2  1  2  2 NA  2  2
#> 29  2  1  2  2 NA  1  2  2  2  1  1  2
#> 30  2  2  1  2  2  2  3  1  2  3  1  2
#> 31  3  2  3  3  3  1  3  1  2  1  1  1
#> 32  2  2  1  2  2  2  2  3 NA  1  1  1
#> 33  1  1  1 NA  2  1  1  1  2  1  1  2
#> 34 NA  2  2  1  2  1  2  1  2  2  3  2
#> 35  1  2 NA  2  1  1  1  2  1  2  3  2
#> 36  1  2  1  1  1  1  3  2  3  3 NA  3
#> 37  1  2  1  2  2  2  3 NA  2  3  3  3
#> 38  2  2  2  2  2  2  2  3  2  3  3  1
#> 39  2  2  2  2  1  2  2  2  2  2  2  1
#> 40  3 NA  3  1  2  2  2  1  1  3  3  2
#> 41  3  2  3  3  3  3  3  3  3  3  2 NA
#> 42  2  2  1  2  2  2  1  2  2  2  1  3
#> 43  1  1  1  3  2  2  1  1  1 NA  3  1
#> 44  2  3  3  2  2 NA  2  2  1  2  1  1
#> 45  2  2  3 NA  1  2  1  2  2  1  1  1
#> 46  1  1  3  1  1  3  2  2  1  3  2  3
#> 47  2  2  3  2  2  3  1  2  2  2  3  3
#> 48  2  1  2  2  2  2  1  3  2  1  2  3
#> 49  2  3  1 NA  2  2  2  3  3  2  2  1
#> 50  2  1  2  2  2 NA  3  2  3  3  3  2
```
