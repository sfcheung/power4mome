# Random Variable From a Lognormal Distribution

Generate random numbers from a lognormal distribution, rescaled to have
user-specified population mean and standard deviation.

## Usage

``` r
rlnorm_rs(n = 10, mui = 0, sigma = 1, rev = FALSE, pmean = 0, psd = 1)
```

## Arguments

- n:

  The number of random numbers to generate.

- mui:

  The parameter `mui` to be used by
  [`stats::rlnorm()`](https://rdrr.io/r/stats/Lognormal.html).

- sigma:

  The parameter `sigma` to be used by
  [`stats::rlnorm()`](https://rdrr.io/r/stats/Lognormal.html).

- rev:

  If TRUE, the distribution is revered to generate a negatively skewed
  distribution. Default is FALSE.

- pmean:

  Population mean.

- psd:

  Population standard deviation.

## Value

A vector of the generated random numbers.

## Details

First, specify the parameter, `mui` and `sigma`, and the desired
population mean and standard deviation. The random numbers, drawn from a
lognormal distribution by
[`stats::rlnorm()`](https://rdrr.io/r/stats/Lognormal.html), will then
be rescaled with the desired population mean and standard.

## Examples

``` r
set.seed(90870962)
x <- rlnorm_rs(n = 5000,
               mui = 0,
               sigma = 1,
               pmean = 0,
               psd = 1)
mean(x)
#> [1] -0.005312214
sd(x)
#> [1] 0.9400859
hist(x)

```
