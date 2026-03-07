# Helpers for the Boos-and-Zhang (2000) Method

Helpers for the method by Boos and Zhang (2000) for estimating rejection
rate for resampling-based tests.

## Usage

``` r
Rs_bz_supported(
  alpha = 0.05,
  Rmax = getOption("power4mome.bz_Rmax", default = 359)
)

R_for_bz(R_target, alpha = 0.05)
```

## Arguments

- alpha:

  The level of significance, two-tailed.

- Rmax:

  The maximum number of resamples to be returned. Default is 359. Though
  it is possible to use 1999 resamples or even more with
  Boos-Zhang-2000, using such a large number of resamples defeats the
  goal to reduce processing time.

- R_target:

  The target maximum number of resamples.

## Value

The function `Rs_bz_supported()` returns a numeric vector of the numbers
of resamples supported.

The function `R_for_bz()` returns a scalar.

## Details

Boos and Zhang (2000) proposed a method to estimate the rejection rate
(power, if the null hypothesis is false) for methods based on
resampling, such as nonparametric bootstrapping. This method is used by
some functions in `power4mome`, such as
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md).

This method is implemented internally. Some helper functions regarding
this method is exported for users.

The function `Rs_bz_supported()` returns the number of bootstrap samples
(for bootstrapping) or simulated samples (for Monte Carlo), both called
resamples below for brevity, usually specified by the argument `R`, that
can be used for the Boos-Zhang-2000 method, given the desired two-tailed
level of significance, (.05 by default). If possible, setting the number
of resamples. (e.g., setting `R` when calling
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md))
will automatically enable the Boos-Zhang-2000 method (unless explicitly
turned off by setting the option `"power4mome.bz"` to `FALSE` by
`options("power4mome.bz") <- FALSE`), substantially reducing the
processing time. For now, only two-tailed tests are supported.

Given a target maximum number of resamples and a level of significance,
the function `R_for_bz()` returns largest number of resamples that can
be used for the Boos-Zhang-2000 method. This function can be used for
arguments such as `R` in
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
to automatically find the largest value supported by the Boos-Zhang-2000
method.

## References

Boos, D. D., & Zhang, J. (2000). Monte Carlo evaluation of
resampling-based hypothesis tests. *Journal of the American Statistical
Association*, *95*(450), 486–492.
[doi:10.1080/01621459.2000.10474226](https://doi.org/10.1080/01621459.2000.10474226)

## See also

[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)

## Examples

``` r
# === Rs_bz_supported ===

# alpha = .05
Rs_bz_supported()
#> [1]  39  79 119 159 199 239 279 319 359

# alpha = .01
Rs_bz_supported(alpha = .01)
#> [1] 199

# === R_for_bz ===

R_for_bz(200)
#> [1] 199
R_for_bz(500)
#> [1] 479
```
