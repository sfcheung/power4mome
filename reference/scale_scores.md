# Process Data by Computing Scale Scores

For the `process_data` argument. To compute scale scores from indicators
and replace the indicators scores by computed scale scores.

## Usage

``` r
scale_scores(data, method = c("mean", "sum"))
```

## Arguments

- data:

  A data frame with the indicator scores. It must has an attribute
  `number_of_indicators`. The same argument used by
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
  This attribute is used to identify the factor names and their
  indicators.

- method:

  The method to be used to compute the scale scores. Can be `"mean"` or
  `"sum"`. The default `na.rm = FALSE` will be used. Therefore, `data`
  must not have missing data.

## Value

It returns a data frame with the scale scores computed.

## Details

This function is to be used in the `process_data` argument of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

It retrieves the attribute `"number_of_indicators"`, stored by
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
to identify factors with indicators, computes the scale scores based on
`method`, and replace the indicators by the scale scores.

All subsequent steps, such as the test functions, will see only the
scale scores, or original scores if a variable has no indicator. The
model will also be fitted on the scale scores, not on the indicators.

It can be used to estimate power for analyzing the scale scores, taking
into account the measurement error due to imperfect reliability.

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
         process_data = list(fun = "scale_scores"),
         test_fun = test_parameters,
         test_args = list(op = "~"),
         parallel = FALSE,
         iseed = 1234)
#> Simulate the data:
#> Fit the model(s):
#> Do the test: test_parameters: CIs (op: ~) 

dat <- pool_sim_data(out)
head(dat)
#>            y           m           x
#> 1 -0.7035296 -1.75480445 -0.40209754
#> 2  0.3718087 -0.01079629 -0.25746074
#> 3 -0.4132790  0.87164340 -0.53160280
#> 4  0.3798879 -1.56624611 -0.15829899
#> 5  0.5091789  0.82256083 -0.54801654
#> 6  0.2830118  0.83206263 -0.05752692
```
