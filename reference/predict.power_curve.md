# Predict Method for a 'power_curve' Object

Compute the predicted values in a model fitted by
[`power_curve()`](https://sfcheung.github.io/power4mome/reference/power_curve.md).

## Usage

``` r
# S3 method for class 'power_curve'
predict(object, newdata, ...)
```

## Arguments

- object:

  A `power_curve` object.

- newdata:

  A data frame with a column named `x`. It can also be a named list,
  with one element named `x` and is a vector of the values. If not
  supplied, values of `x` stored in `object` will be used.

- ...:

  Additional arguments. Passed to the corresponding `predict` method.

## Value

It returns a numeric vector of the predicted rejection rates.

## Details

The `predict` method of `power_curve` objects works in two modes.

If new data is not supplied (through `newdata`), it retrieves the stored
results and calls the corresponding methods to compute the predicted
values, which are the predicted rejection rates (power levels if the
null hypothesis is false, e.g., the population effect size is equal to
zero).

If new data is supplied, such as a named list with a vector of sample
sizes, they will be used to compute the predicted rejection rates.

## See also

[`power_curve()`](https://sfcheung.github.io/power4mome/reference/power_curve.md).

## Examples

``` r
# Specify the population model

model_simple_med <-
"
m ~ x
y ~ m + x
"

# Specify the effect sizes (population parameter values)

model_simple_med_es <-
"
y ~ m: l
m ~ x: m
y ~ x: s
"

# Simulate datasets to check the model
# Set `parallel` to TRUE for faster, usually much faster, analysis
# Set `progress` to TRUE to display the progress of the analysis

sim_only <- power4test(nrep = 10,
                       model = model_simple_med,
                       pop_es = model_simple_med_es,
                       n = 50,
                       fit_model_args = list(fit_function = "lm"),
                       do_the_test = FALSE,
                       iseed = 1234,
                       parallel = FALSE,
                       progress = FALSE)

# By n: Do a test for different sample sizes

out1 <- power4test_by_n(sim_only,
                        nrep = 10,
                        test_fun = test_parameters,
                        test_args = list(par = "y~x"),
                        n = c(25, 100, 200, 1000),
                        by_seed = 1234,
                        parallel = FALSE,
                        progress = FALSE)

pout1 <- power_curve(out1)
pout1
#> Call:
#> power_curve(object = out1)
#> 
#> Predictor: n (Sample Size)
#> 
#> Model:
#> 
#> Call:  stats::glm(formula = reject ~ x, family = "binomial", data = reject1)
#> 
#> Coefficients:
#> (Intercept)            x  
#>   -1.550280     0.004213  
#> 
#> Degrees of Freedom: 39 Total (i.e. Null);  38 Residual
#> Null Deviance:       54.55 
#> Residual Deviance: 38.37     AIC: 42.37
predict(pout1,
        newdata = list(x = c(150, 250, 500)))
#>         1         2         3 
#> 0.2852909 0.3782246 0.6355494 

# By pop_es: Do a test for different population values of a model parameter

out2 <- power4test_by_es(sim_only,
                             nrep = 10,
                             test_fun = test_parameters,
                             test_args = list(par = "y~x"),
                             pop_es_name = "y ~ x",
                             pop_es_values = seq(0, .7, .15),
                             by_seed = 1234,
                             parallel = FALSE,
                             progress = FALSE)

pout2 <- power_curve(out2)
pout2
#> Call:
#> power_curve(object = out2)
#> 
#> Predictor: es (Effect Size)
#> 
#> Model:
#> 
#> Call:  stats::glm(formula = reject ~ x, family = "binomial", data = reject1)
#> 
#> Coefficients:
#> (Intercept)            x  
#>      -3.347       17.231  
#> 
#> Degrees of Freedom: 49 Total (i.e. Null);  48 Residual
#> Null Deviance:       65.34 
#> Residual Deviance: 25    AIC: 29
predict(pout2,
        newdata = list(x = c(.25, .55)))
#>         1         2 
#> 0.7232684 0.9978282 
```
