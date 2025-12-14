# Random Variable From a t Distribution

Generate random numbers from a *t* distribution, rescaled to have
user-specified population mean and standard deviation.

## Usage

``` r
rt_rs(n = 10, df = 5, pmean = 0, psd = 1)
```

## Arguments

- n:

  The number of random numbers to generate.

- df:

  `df` for [`stats::rt()`](https://rdrr.io/r/stats/TDist.html).

- pmean:

  Population mean.

- psd:

  Population standard deviation.

## Value

A vector of the generated random numbers.

## Details

First, specify the parameter `df` and the desired population mean and
standard deviation. The random numbers, drawn from the generalized
normal distribution by
[`stats::rt()`](https://rdrr.io/r/stats/TDist.html), will then be
rescaled with the desired population mean and standard.

## Examples

``` r
set.seed(90870962)
x <- rt_rs(n = 5000,
           df = 5,
           pmean = 3,
           psd = 1)
mean(x)
#> [1] 3.004783
sd(x)
#> [1] 0.9926204
hist(x)

```
