# Random Variable From an Exponential Distribution

Generate random numbers from an exponential distribution, rescaled to
have user-specified population mean and standard deviation.

## Usage

``` r
rexp_rs(n = 10, rate = 1, pmean = 0, psd = 1, rev = FALSE)
```

## Arguments

- n:

  The number of random numbers to generate.

- rate:

  `rate` for
  [`stats::rexp()`](https://rdrr.io/r/stats/Exponential.html).

- pmean:

  Population mean.

- psd:

  Population standard deviation.

- rev:

  If TRUE, the distribution is revered to generate a negatively skewed
  distribution. Default is FALSE.

## Value

A vector of the generated random numbers.

## Details

First, specify the parameter, `rate`, and the desired population mean
and standard deviation. The random numbers, drawn from an exponential
distribution by
[`stats::rexp()`](https://rdrr.io/r/stats/Exponential.html), will then
be rescaled with the desired population mean and standard.

## Examples

``` r
set.seed(90870962)
x <- rexp_rs(n = 5000,
             rate = 4,
             pmean = 3,
             psd = 1)
mean(x)
#> [1] 2.999214
sd(x)
#> [1] 1.008795
hist(x)

```
