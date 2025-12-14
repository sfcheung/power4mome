# Random Variable From a Uniform Distribution

Generate random numbers from a uniform distribution, with user-specified
population mean and standard deviation.

## Usage

``` r
runif_rs(n = 10, min = 0, max = 1, pmean = 0, psd = 1)
```

## Arguments

- n:

  The number of random numbers to generate.

- min:

  min for runif.

- max:

  max for runif.

- pmean:

  Population mean.

- psd:

  Population standard deviation.

## Value

A vector of the generated random numbers.

## Details

First, the user specifies the parameters, min and max, and the desired
population mean and standard deviation. Then the random numbers will be
generated and rescaled with the desired population mean and standard.

## Examples

``` r
set.seed(90870962)
x <- runif_rs(n = 5000,
              min = 2,
              max = 4,
              pmean = 3,
              psd = 1)
mean(x)
#> [1] 2.987606
sd(x)
#> [1] 0.9978981
hist(x)

```
