# Random Variable From a Generalized Normal Distribution

Generate random numbers from generalized normal distribution, rescaled
to have user-specified population mean and standard deviation.

## Usage

``` r
rpgnorm_rs(n = 10, p = 2, pmean = 0, psd = 1)
```

## Arguments

- n:

  The number of random numbers to generate.

- p:

  The parameter of the distribution. Must be a positive non-zero number.
  Default is 2, resulting in a normal distribution. Higher than 2
  results in negative excess kurtosis. Lower than 2 results in positive
  excess kurtosis.

- pmean:

  Population mean.

- psd:

  Population standard deviation.

## Value

A vector of the generated random numbers.

## Details

First, specify the parameter `p` and the desired population mean and
standard deviation. The random numbers, drawn from a generalized normal
distribution by
[`pgnorm::rpgnorm()`](https://rdrr.io/pkg/pgnorm/man/rpgnorm.html), will
then be rescaled with the desired population mean and standard.

## Examples

``` r
set.seed(90870962)
x <- rpgnorm_rs(n = 5000,
                p = 2,
                pmean = 0,
                psd = 1)
mean(x)
#> [1] -0.01118704
sd(x)
#> [1] 1.010004
hist(x)

x_kurt <- function(p) {gamma(5/p)*gamma(1/p)/(gamma(3/p)^2) - 3}

p <- 6
x <- rpgnorm_rs(n = 50000,
                p = p,
                pmean = 0,
                psd = 1)
mean(x)
#> [1] 0.002266498
sd(x)
#> [1] 1.001834
x_kurt(p)
#> [1] -1
qqnorm(x); qqline(x)


p <- 1
x <- rpgnorm_rs(n = 50000,
                p = p,
                pmean = 0,
                psd = 1)
mean(x)
#> [1] 0.007350372
sd(x)
#> [1] 0.9993787
x_kurt(p)
#> [1] 3
qqnorm(x); qqline(x)
```
