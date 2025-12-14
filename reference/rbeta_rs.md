# Random Variable From a Beta Distribution

Generate random numbers from a beta distribution, rescaled to have
user-specified population mean and standard deviation.

## Usage

``` r
rbeta_rs(n = 10, shape1 = 0.5, shape2 = 0.5, pmean = 0, psd = 1)
```

## Arguments

- n:

  The number of random numbers to generate.

- shape1:

  `shape1` for [`stats::rbeta()`](https://rdrr.io/r/stats/Beta.html).

- shape2:

  `shape2` for [`stats::rbeta()`](https://rdrr.io/r/stats/Beta.html).

- pmean:

  Population mean.

- psd:

  Population standard deviation.

## Value

A vector of the generated random numbers.

## Details

First, specify the two parameters, `shape1` and `shape2`, and the
desired population mean and standard deviation. The random numbers,
drawn from a beta distribution by
[`stats::rbeta()`](https://rdrr.io/r/stats/Beta.html) will then be
rescaled with the desired population mean and standard deviation.

## Examples

``` r
set.seed(90870962)
x <- rbeta_rs(n = 5000,
              shape1 = .5,
              shape2 = .5,
              pmean = 3,
              psd = 1)
mean(x)
#> [1] 3.003229
sd(x)
#> [1] 0.9983932
hist(x)

```
