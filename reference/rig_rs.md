# Random Variables Using the IG method.

Generate multivariate random numbers using the IG method.

## Usage

``` r
rig_rs(
  n = 10,
  sigma = diag(1),
  skew = 0,
  kurt = 0,
  rIG_args = list(),
  pmean = 0,
  psd = 1
)
```

## Arguments

- n:

  The number of cases to generate.

- sigma:

  The population covariance matrix. It is recommended to supply a
  correlation matrix (with diagonal elements equal to one). The numbers
  will be rescaled to have population means and SDs specified by `pmean`
  and `psd`, respectively.

- skew:

  A numeric vector of the population skewness coefficients. If it is a
  scalar, its value will be used as the population skewness of all
  variables to be generated.

- kurt:

  A numeric vector of the population excess kurtosis coefficients. If it
  is a scalar, its value will be used as the population excess kurtosis
  coefficients. of all variables to be generated.

- rIG_args:

  A list of extra arguments to be passed to
  [`covsim::rIG()`](https://rdrr.io/pkg/covsim/man/rIG.html).

- pmean:

  A numeric vector population means. If it is a scalar, its value will
  be used as the population means of all variables to be generated.

- psd:

  A numeric vector of population standard deviations. If it is a scalar,
  its value will be used as the population standard deviations of all
  variables to be generated.

## Value

A two-dimension matrix of the generated random numbers.

## Details

First, random numbers will be generated using
[`covsim::rIG()`](https://rdrr.io/pkg/covsim/man/rIG.html), with the
specified target population marginal skewness and excess kurtosis. The
random numbers will then be rescaled with the desired population means
and standard deviations.

## References

Foldnes, N. and Olson, U. H. (2016). A simple simulation technique for
nonnormal data with prespecified skewness, kurtosis, and covariance
matrix. *Multivariate Behavioral Research*, *51*(2–3), 207–219.
[doi:10.1080/00273171.2015.1133274](https://doi.org/10.1080/00273171.2015.1133274)

## Examples

``` r
set.seed(90870962)
sigma <- matrix(c(1, .3, .3, 1), 2, 2)
sigma
#>      [,1] [,2]
#> [1,]  1.0  0.3
#> [2,]  0.3  1.0
x <- rig_rs(n = 5000,
            sigma = sigma,
            skew = 2,
            kurt = 6
)
colMeans(x)
#> [1] -0.003175567  0.017891561
apply(x, 2, sd)
#> [1] 1.005155 1.026747
hist(x[, 1])

```
