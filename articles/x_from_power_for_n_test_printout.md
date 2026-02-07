# Sample Size Given Desired Power (Internal)

## NOTE

This article is for internal testing of the printout.

``` r
library(power4mome)
options(power4mome.bz = TRUE)
```

``` r
mod <-
"
m ~ x
y ~ m + x
"
```

``` r
mod_es <-
"
m ~ x: m
y ~ m: l
y ~ x: s
"
```

``` r
out <- power4test(nrep = 2,
                  model = mod,
                  pop_es = mod_es,
                  n = 50000,
                  iseed = 1234)
```

``` r
print(out,
      data_long = TRUE)
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> == Model on Variables/Indicators ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.300  
#>   y ~                        
#>     m                 0.500  
#>     x                 0.100  
#> 
#> Variances:
#>                    Population
#>    .m                 0.910  
#>    .y                 0.710  
#>     x                 1.000  
#> 
#> (Computing indirect effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>               ind
#> x -> m -> y 0.150
#> x -> y      0.100
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  2 
#> Sample Sizes:  50000 
#> 
#> ==== Descriptive Statistics ====
#> 
#>   vars     n mean sd skew kurtosis se
#> m    1 1e+05 0.00  1 0.01     0.03  0
#> y    2 1e+05 0.01  1 0.02     0.00  0
#> x    3 1e+05 0.00  1 0.01     0.01  0
#> 
#> ===== Parameter Estimates Based on All 2 Samples Combined =====
#> 
#> Total Sample Size: 100000 
#> 
#> ==== Standardized Estimates ====
#> 
#> Variances and error variances omitted.
#> 
#> Regressions:
#>                     est.std
#>   m ~                      
#>     x                 0.301
#>   y ~                      
#>     m                 0.496
#>     x                 0.101
#> 
#> 
#> ==================== Extra Element(s) Found ====================
#> 
#> - fit
#> 
#> === Element(s) of the First Dataset ===
#> 
#> ============ <fit> ============
#> 
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                         50000
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
```

``` r
out <- power4test(nrep = 200,
                  model = mod,
                  pop_es = mod_es,
                  n = 100,
                  R = 199,
                  ci_type = "mc",
                  test_fun = test_indirect_effect,
                  test_args = list(x = "x",
                                   m = "m",
                                   y = "y",
                                   mc_ci = TRUE),
                  iseed = 2345,
                  parallel = TRUE)
```

``` r
print(out,
      test_long = TRUE)
#> 
#> ====================== Model Information ======================
#> 
#> == Model on Factors/Variables ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> == Model on Variables/Indicators ==
#> 
#> m ~ x
#> y ~ m + x
#> 
#> ====== Population Values ======
#> 
#> Regressions:
#>                    Population
#>   m ~                        
#>     x                 0.300  
#>   y ~                        
#>     m                 0.500  
#>     x                 0.100  
#> 
#> Variances:
#>                    Population
#>    .m                 0.910  
#>    .y                 0.710  
#>     x                 1.000  
#> 
#> (Computing indirect effects for 2 paths ...)
#> 
#> == Population Conditional/Indirect Effect(s) ==
#> 
#> == Indirect Effect(s) ==
#> 
#>               ind
#> x -> m -> y 0.150
#> x -> y      0.100
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ======================= Data Information =======================
#> 
#> Number of Replications:  200 
#> Sample Sizes:  100 
#> 
#> Call print with 'data_long = TRUE' for further information.
#> 
#> ==================== Extra Element(s) Found ====================
#> 
#> - fit
#> - mc_out
#> 
#> === Element(s) of the First Dataset ===
#> 
#> ============ <fit> ============
#> 
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> =========== <mc_out> ===========
#> 
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 199 
#> 
#> 
#> =============== <test_indirect: x->m->y> ===============
#> 
#> Mean(s) across replication:
#>    est  cilo  cihi   sig pvalue
#>  0.151 0.050 0.271 0.875  0.029
#> 
#> - The value 'sig' is the rejection rate.
#> - If the null hypothesis is false, this is the power.
#> - Number of valid replications for rejection rate: 200 
#> - Proportion of valid replications for rejection rate: 1.000
```

``` r
out_n <- n_from_power(out,
                      what = "ub",
                      seed = 4567)
```

``` r
out_n
#> Call:
#> power4mome::x_from_power(object = out, x = "n", what = "ub", 
#>     goal = "close_enough", final_nrep = 200, final_R = 199, seed = 4567)
#> 
#>                           Setting
#> Predictor(x):         Sample Size
#> Parameter:                    N/A
#> goal:                close_enough
#> what:                          ub
#> algorithm:              bisection
#> Level of confidence:       95.00%
#> Target Power:               0.800
#> 
#> - Final Value of Sample Size (n): 75
#> 
#> - Final Estimated Power (CI): 0.730 [0.665, 0.787]
#> 
#> Call `summary()` for detailed results.
```

``` r
summary(out_n)
#> 
#> ====== x_from_power Results ======
#> 
#> Call:
#> x_from_power(object = out, x = "n", what = "ub", goal = "close_enough", 
#>     final_nrep = 200, final_R = 199, seed = 4567)
#> 
#> Predictor (x): Sample Size 
#> 
#> - Target Power: 0.800 
#> - Goal: Find 'x' with estimated upper confidence bound close enough to
#>   the target power.
#> 
#> === Major Results ===
#> 
#> - Final Value (Sample Size): 75
#> 
#> - Final Estimated Power: 0.730 
#> - Confidence Interval: [0.665; 0.787]
#> - Level of confidence: 95.0%
#> - Based on 200 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 100 to 75 
#> - Time spent in the search: 19.03 secs 
#> - The final crude model for the power-predictor relation:
#> 
#> Model Type: Logistic Regression 
#> 
#> Call:
#> power_curve(object = by_x_1, formula = power_model, start = power_curve_start, 
#>     lower_bound = lower_bound, upper_bound = upper_bound, nls_args = nls_args, 
#>     nls_control = nls_control, verbose = progress)
#> 
#> Predictor: n (Sample Size)
#> 
#> Model:
#> 
#> Call:  stats::glm(formula = reject ~ x, family = "binomial", data = reject1)
#> 
#> Coefficients:
#> (Intercept)            x  
#>    -1.04617      0.02887  
#> 
#> Degrees of Freedom: 599 Total (i.e. Null);  598 Residual
#> Null Deviance:       689.8 
#> Residual Deviance: 652.5     AIC: 656.5
#> 
#> - Detailed Results:
#> 
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.158 1.000  0.610  0.541  0.675
#> 2  75 0.149 1.000  0.730  0.665  0.787
#> 3 100 0.151 1.000  0.875  0.822  0.914
#> Notes:
#> - n: The sample size in a trial.
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
```

``` r
plot(out_n)
```

![The Power Curve](x_from_power_for_n_plot_test-1.png)

The Power Curve

``` r
out_n_lb <- n_from_power(out,
                         what = "lb",
                         seed = 2345)
```

``` r
out_n_lb
#> Call:
#> power4mome::x_from_power(object = out, x = "n", what = "lb", 
#>     goal = "close_enough", final_nrep = 200, final_R = 199, seed = 2345)
#> 
#>                           Setting
#> Predictor(x):         Sample Size
#> Parameter:                    N/A
#> goal:                close_enough
#> what:                          lb
#> algorithm:              bisection
#> Level of confidence:       95.00%
#> Target Power:               0.800
#> 
#> - Final Value of Sample Size (n): 96
#> 
#> - Final Estimated Power (CI): 0.840 [0.783, 0.884]
#> 
#> Call `summary()` for detailed results.
```

``` r
summary(out_n_lb)
#> 
#> ====== x_from_power Results ======
#> 
#> Call:
#> x_from_power(object = out, x = "n", what = "lb", goal = "close_enough", 
#>     final_nrep = 200, final_R = 199, seed = 2345)
#> 
#> Predictor (x): Sample Size 
#> 
#> - Target Power: 0.800 
#> - Goal: Find 'x' with estimated lower confidence bound close enough to
#>   the target power.
#> 
#> === Major Results ===
#> 
#> - Final Value (Sample Size): 96
#> 
#> - Final Estimated Power: 0.840 
#> - Confidence Interval: [0.783; 0.884]
#> - Level of confidence: 95.0%
#> - Based on 200 replications.
#> 
#> === Technical Information ===
#> 
#> - Algorithm: bisection 
#> - Tolerance for 'close enough': Within 0.02000 of 0.800 
#> - The range of values explored: 100 to 96 
#> - Time spent in the search: 35.82 secs 
#> - The final crude model for the power-predictor relation:
#> 
#> Model Type: Logistic Regression 
#> 
#> Call:
#> power_curve(object = by_x_1, formula = power_model, start = power_curve_start, 
#>     lower_bound = lower_bound, upper_bound = upper_bound, nls_args = nls_args, 
#>     nls_control = nls_control, verbose = progress)
#> 
#> Predictor: n (Sample Size)
#> 
#> Model:
#> 
#> Call:  stats::glm(formula = reject ~ x, family = "binomial", data = reject1)
#> 
#> Coefficients:
#> (Intercept)            x  
#>    -1.15443      0.03022  
#> 
#> Degrees of Freedom: 999 Total (i.e. Null);  998 Residual
#> Null Deviance:       1059 
#> Residual Deviance: 1002  AIC: 1006
#> 
#> - Detailed Results:
#> 
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.154 1.000  0.580  0.511  0.646
#> 2  75 0.147 1.000  0.775  0.712  0.827
#> 3  91 0.153 1.000  0.820  0.761  0.867
#> 4  96 0.148 1.000  0.840  0.783  0.884
#> 5 100 0.151 1.000  0.875  0.822  0.914
#> Notes:
#> - n: The sample size in a trial.
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
```

``` r
plot(out_n_lb)
```

![The Power Curve](x_from_power_for_n_lb_plot_test-1.png)

The Power Curve

``` r
n_power_region <- n_region_from_power(out,
                                      seed = 2468)
#> 
#> =========== Phase 1: Upper Bound ===========
#> 
#> Find the approximate region with power significantly below 0.8 ...
#> 
#> --- Setting ---
#> 
#> Algorithm:  bisection 
#> Goal:  close_enough 
#> What:  ub   (Upper bound of the confidence interval) 
#> 
#> --- Progress  ---
#> 
#> - Set 'progress = FALSE' to suppress displaying the progress.
#> - Set 'simulation progress = FALSE' to suppress displaying the progress
#>   in the simulation.
#> 
#> Initial interval: [50, 100] 
#> 
#> 
#> Do the simulation for the lower bound:
#> 
#> Try x = 50 
#> 
#> Updating the simulation for sample size: 50 
#> Re-simulate the data:
#> Fit the model(s):
#> Generate Monte Carlo estimates:
#> Update the test(s):
#> Update test_indirect: x->m->y :
#> 
#> Estimated power at 50: 0.575, 95.0% confidence interval: [0.506,0.641]
#> 
#> Initial interval: [50, 100] 
#> 
#> - Rejection Rates:
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.150 1.000  0.575  0.506  0.641
#> 2 100 0.151 1.000  0.875  0.822  0.914
#> 
#> 
#> == Enter extending interval ...
#> The interval is already valid: [50, 100] 
#> == Exit extending interval ...
#> 
#> Iteration # 1 
#> 
#> Try x = 75 
#> 
#> Updating the simulation for sample size: 75 
#> Re-simulate the data:
#> Fit the model(s):
#> Generate Monte Carlo estimates:
#> Update the test(s):
#> Update test_indirect: x->m->y :
#> 
#> Estimated power at 75: 0.780, 95.0% confidence interval: [0.718,0.832]
#> - Rejection Rates:
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.150 1.000  0.575  0.506  0.641
#> 2  75 0.153 1.000  0.780  0.718  0.832
#> 3 100 0.151 1.000  0.875  0.822  0.914
#> 
#> New interval: [50, 75] 
#> Power curve used to find the next x ...
#> Updated x: 68 
#> 
#> Iteration # 2 
#> 
#> Try x = 68 
#> 
#> Updating the simulation for sample size: 68 
#> Re-simulate the data:
#> Fit the model(s):
#> Generate Monte Carlo estimates:
#> Update the test(s):
#> Update test_indirect: x->m->y :
#> 
#> Estimated power at 68: 0.730, 95.0% confidence interval: [0.665,0.787]
#> - Rejection Rates:
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.150 1.000  0.575  0.506  0.641
#> 2  68 0.155 1.000  0.730  0.665  0.787
#> 3  75 0.153 1.000  0.780  0.718  0.832
#> 4 100 0.151 1.000  0.875  0.822  0.914
#> 
#> - 'nls()' estimation skipped when less than 4 values of predictor examined.
#> Solution found.
#> 
#> ========== Final Stage ==========
#> 
#> - Start at 2026-02-07 13:54:59 
#> - Rejection Rates:
#> 
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.150 1.000  0.575  0.506  0.641
#> 2  68 0.155 1.000  0.730  0.665  0.787
#> 3  75 0.153 1.000  0.780  0.718  0.832
#> 4 100 0.151 1.000  0.875  0.822  0.914
#> Notes:
#> - n: The sample size in a trial.
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
#> 
#> - Estimated Power Curve:
#> 
#> Call:
#> power_curve(object = by_x_1, formula = power_model, start = power_curve_start, 
#>     lower_bound = lower_bound, upper_bound = upper_bound, nls_args = nls_args, 
#>     nls_control = nls_control, verbose = progress)
#> 
#> Predictor: n (Sample Size)
#> 
#> Model:
#> 
#> Call:  stats::glm(formula = reject ~ x, family = "binomial", data = reject1)
#> 
#> Coefficients:
#> (Intercept)            x  
#>    -1.33500      0.03369  
#> 
#> Degrees of Freedom: 799 Total (i.e. Null);  798 Residual
#> Null Deviance:       916.9 
#> Residual Deviance: 868   AIC: 872
#> 
#> 
#> - Final Value: 68 
#> 
#> - Final Estimated Power: 0.7300 
#> - Confidence Interval: [0.6646; 0.7868]
#> - CI Level: 95.00%
#> 
#> =========== Phase 2: Lower Bound ===========
#> 
#> Find the approximate region with power significantly above 0.8 ...
#> 
#> --- Setting ---
#> 
#> Algorithm:  bisection 
#> Goal:  close_enough 
#> What:  lb   (Lower bound of the confidence interval) 
#> 
#> --- Progress  ---
#> 
#> - Set 'progress = FALSE' to suppress displaying the progress.
#> - Set 'simulation progress = FALSE' to suppress displaying the progress
#>   in the simulation.
#> 
#> Initial interval: [50, 100] 
#> 
#> 
#> Initial interval: [50, 100] 
#> 
#> - Rejection Rates:
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.150 1.000  0.575  0.506  0.641
#> 2  68 0.155 1.000  0.730  0.665  0.787
#> 3  75 0.153 1.000  0.780  0.718  0.832
#> 4 100 0.151 1.000  0.875  0.822  0.914
#> 
#> 
#> == Enter extending interval ...
#> The interval is already valid: [50, 100] 
#> == Exit extending interval ...
#> 
#> Iteration # 1 
#> 
#> Try x = 75 
#> 
#> Updating the simulation for sample size: 75 
#> Re-simulate the data:
#> Fit the model(s):
#> Generate Monte Carlo estimates:
#> Update the test(s):
#> Update test_indirect: x->m->y :
#> 
#> Estimated power at 75: 0.710, 95.0% confidence interval: [0.644,0.768]
#> - Rejection Rates:
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.150 1.000  0.575  0.506  0.641
#> 2  68 0.155 1.000  0.730  0.665  0.787
#> 3  75 0.153 1.000  0.780  0.718  0.832
#> 4  75 0.147 1.000  0.710  0.644  0.768
#> 5 100 0.151 1.000  0.875  0.822  0.914
#> 
#> New interval: [75, 100] 
#> Power curve used to find the next x ...
#> Updated x: 92 
#> 
#> Iteration # 2 
#> 
#> Try x = 92 
#> 
#> Updating the simulation for sample size: 92 
#> Re-simulate the data:
#> Fit the model(s):
#> Generate Monte Carlo estimates:
#> Update the test(s):
#> Update test_indirect: x->m->y :
#> 
#> Estimated power at 92: 0.855, 95.0% confidence interval: [0.800,0.897]
#> - Rejection Rates:
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.150 1.000  0.575  0.506  0.641
#> 2  68 0.155 1.000  0.730  0.665  0.787
#> 3  75 0.153 1.000  0.780  0.718  0.832
#> 4  75 0.147 1.000  0.710  0.644  0.768
#> 5  92 0.154 1.000  0.855  0.800  0.897
#> 6 100 0.151 1.000  0.875  0.822  0.914
#> 
#> - 'nls()' estimation skipped when less than 4 values of predictor examined.
#> Solution found.
#> 
#> ========== Final Stage ==========
#> 
#> - Start at 2026-02-07 13:55:17 
#> - Rejection Rates:
#> 
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.150 1.000  0.575  0.506  0.641
#> 2  68 0.155 1.000  0.730  0.665  0.787
#> 3  75 0.153 1.000  0.780  0.718  0.832
#> 4  75 0.147 1.000  0.710  0.644  0.768
#> 5  92 0.154 1.000  0.855  0.800  0.897
#> 6 100 0.151 1.000  0.875  0.822  0.914
#> Notes:
#> - n: The sample size in a trial.
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
#> 
#> - Estimated Power Curve:
#> 
#> Call:
#> power_curve(object = by_x_1, formula = power_model, start = power_curve_start, 
#>     lower_bound = lower_bound, upper_bound = upper_bound, nls_args = nls_args, 
#>     nls_control = nls_control, verbose = progress)
#> 
#> Predictor: n (Sample Size)
#> 
#> Model:
#> 
#> Call:  stats::glm(formula = reject ~ x, family = "binomial", data = reject1)
#> 
#> Coefficients:
#> (Intercept)            x  
#>    -1.35382      0.03321  
#> 
#> Degrees of Freedom: 1199 Total (i.e. Null);  1198 Residual
#> Null Deviance:       1339 
#> Residual Deviance: 1277  AIC: 1281
#> 
#> 
#> - Final Value: 92 
#> 
#> - Final Estimated Power: 0.8550 
#> - Confidence Interval: [0.7995; 0.8971]
#> - CI Level: 95.00%
n_power_region
#> Call:
#> n_region_from_power(object = out, seed = 2468)
#> 
#>                      Setting                                      
#> Predictor(x)         Sample Size                                  
#> Goal:                Power significantly below or above the target
#> algorithm:           bisection                                    
#> Level of confidence: 95.00%                                       
#> Target Power:        0.800                                        
#> 
#> Solution: 
#> 
#> Approximate region of sample sizes with power:
#> - not significantly different from 0.800: 68 to 92
#> - significantly lower than 0.800: 68
#> - significantly higher than 0.800: 92
#> 
#> Confidence intervals of the estimated power:
#> - for the lower bound (68): [0.665, 0.787]
#> - for the upper bound (92): [0.800, 0.897]
#> 
#> Call `summary()` for detailed results.
```

``` r
plot(n_power_region)
```

![The Power Curve (Region)](region_plot-1.png)

The Power Curve (Region)

``` r
n_power_region2 <- n_region_from_power(out_n_lb,
                                      seed = 24680)
#> 
#> =========== Phase 1: Upper Bound ===========
#> 
#> Find the approximate region with power significantly below 0.8 ...
#> 
#> --- Setting ---
#> 
#> Algorithm:  bisection 
#> Goal:  close_enough 
#> What:  ub   (Upper bound of the confidence interval) 
#> 
#> --- Progress  ---
#> 
#> - Set 'progress = FALSE' to suppress displaying the progress.
#> - Set 'simulation progress = FALSE' to suppress displaying the progress
#>   in the simulation.
#> 
#> Initial interval: [50, 75] 
#> 
#> 
#> Initial interval: [50, 75] 
#> 
#> - Rejection Rates:
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.154 1.000  0.580  0.511  0.646
#> 2  75 0.147 1.000  0.775  0.712  0.827
#> 3  91 0.153 1.000  0.820  0.761  0.867
#> 4  96 0.148 1.000  0.840  0.783  0.884
#> 5 100 0.151 1.000  0.875  0.822  0.914
#> 
#> 
#> == Enter extending interval ...
#> The interval is already valid: [50, 75] 
#> == Exit extending interval ...
#> 
#> Iteration # 1 
#> 
#> Try x = 63 
#> 
#> Updating the simulation for sample size: 63 
#> Re-simulate the data:
#> Fit the model(s):
#> Generate Monte Carlo estimates:
#> Update the test(s):
#> Update test_indirect: x->m->y :
#> 
#> Estimated power at 63: 0.700, 95.0% confidence interval: [0.633,0.759]
#> - Rejection Rates:
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.154 1.000  0.580  0.511  0.646
#> 2  63 0.156 1.000  0.700  0.633  0.759
#> 3  75 0.147 1.000  0.775  0.712  0.827
#> 4  91 0.153 1.000  0.820  0.761  0.867
#> 5  96 0.148 1.000  0.840  0.783  0.884
#> 6 100 0.151 1.000  0.875  0.822  0.914
#> 
#> New interval: [63, 75] 
#> Power curve used to find the next x ...
#> Updated x: 71 
#> 
#> Iteration # 2 
#> 
#> Try x = 71 
#> 
#> Updating the simulation for sample size: 71 
#> Re-simulate the data:
#> Fit the model(s):
#> Generate Monte Carlo estimates:
#> Update the test(s):
#> Update test_indirect: x->m->y :
#> 
#> Estimated power at 71: 0.730, 95.0% confidence interval: [0.665,0.787]
#> - Rejection Rates:
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.154 1.000  0.580  0.511  0.646
#> 2  63 0.156 1.000  0.700  0.633  0.759
#> 3  71 0.153 1.000  0.730  0.665  0.787
#> 4  75 0.147 1.000  0.775  0.712  0.827
#> 5  91 0.153 1.000  0.820  0.761  0.867
#> 6  96 0.148 1.000  0.840  0.783  0.884
#> 7 100 0.151 1.000  0.875  0.822  0.914
#> 
#> - 'nls()' estimation skipped when less than 4 values of predictor examined.
#> Solution found.
#> 
#> ========== Final Stage ==========
#> 
#> - Start at 2026-02-07 13:55:35 
#> - Rejection Rates:
#> 
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.154 1.000  0.580  0.511  0.646
#> 2  63 0.156 1.000  0.700  0.633  0.759
#> 3  71 0.153 1.000  0.730  0.665  0.787
#> 4  75 0.147 1.000  0.775  0.712  0.827
#> 5  91 0.153 1.000  0.820  0.761  0.867
#> 6  96 0.148 1.000  0.840  0.783  0.884
#> 7 100 0.151 1.000  0.875  0.822  0.914
#> Notes:
#> - n: The sample size in a trial.
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
#> 
#> - Estimated Power Curve:
#> 
#> Call:
#> power_curve(object = by_x_1, formula = power_model, start = power_curve_start, 
#>     lower_bound = lower_bound, upper_bound = upper_bound, nls_args = nls_args, 
#>     nls_control = nls_control, verbose = progress)
#> 
#> Predictor: n (Sample Size)
#> 
#> Model:
#> 
#> Call:  stats::glm(formula = reject ~ x, family = "binomial", data = reject1)
#> 
#> Coefficients:
#> (Intercept)            x  
#>    -1.08914      0.02958  
#> 
#> Degrees of Freedom: 1399 Total (i.e. Null);  1398 Residual
#> Null Deviance:       1543 
#> Residual Deviance: 1480  AIC: 1484
#> 
#> 
#> - Final Value: 71 
#> 
#> - Final Estimated Power: 0.7300 
#> - Confidence Interval: [0.6646; 0.7868]
#> - CI Level: 95.00%
#> 
#> =========== Phase 2: Lower Bound ===========
#> 
#> Find the approximate region with power significantly above 0.8 ...
#> 
#> --- Setting ---
#> 
#> Algorithm:  bisection 
#> Goal:  close_enough 
#> What:  lb   (Lower bound of the confidence interval) 
#> 
#> --- Progress  ---
#> 
#> - Set 'progress = FALSE' to suppress displaying the progress.
#> - Set 'simulation progress = FALSE' to suppress displaying the progress
#>   in the simulation.
#> 
#> --- Solution Already Found ---
#> 
#> Solution already found in the object. Search will be skipped.
#> - 'nls()' estimation skipped when less than 4 values of predictor examined.
#> 
#> ========== Final Stage ==========
#> 
#> - Start at 2026-02-07 13:55:35 
#> - Rejection Rates:
#> 
#> [test]: test_indirect: x->m->y 
#> [test_label]: Test 
#>     n   est   p.v reject r.cilo r.cihi
#> 1  50 0.154 1.000  0.580  0.511  0.646
#> 2  63 0.156 1.000  0.700  0.633  0.759
#> 3  71 0.153 1.000  0.730  0.665  0.787
#> 4  75 0.147 1.000  0.775  0.712  0.827
#> 5  91 0.153 1.000  0.820  0.761  0.867
#> 6  96 0.148 1.000  0.840  0.783  0.884
#> 7 100 0.151 1.000  0.875  0.822  0.914
#> Notes:
#> - n: The sample size in a trial.
#> - p.v: The proportion of valid replications.
#> - est: The mean of the estimates in a test across replications.
#> - reject: The proportion of 'significant' replications, that is, the
#>   rejection rate. If the null hypothesis is true, this is the Type I
#>   error rate. If the null hypothesis is false, this is the power.
#> - r.cilo,r.cihi: The confidence interval of the rejection rate, based
#>   on Wilson's (1927) method.
#> - Refer to the tests for the meanings of other columns.
#> 
#> - Estimated Power Curve:
#> 
#> Call:
#> power_curve(object = by_x_1, formula = power_curve_args$power_model, 
#>     start = power_curve_args$start, lower_bound = power_curve_args$lower_bound, 
#>     upper_bound = power_curve_args$upper_bound, nls_args = power_curve_args$nls_args, 
#>     nls_control = power_curve_args$nls_control, verbose = progress)
#> 
#> Predictor: n (Sample Size)
#> 
#> Model:
#> 
#> Call:  stats::glm(formula = reject ~ x, family = "binomial", data = reject1)
#> 
#> Coefficients:
#> (Intercept)            x  
#>    -1.08914      0.02958  
#> 
#> Degrees of Freedom: 1399 Total (i.e. Null);  1398 Residual
#> Null Deviance:       1543 
#> Residual Deviance: 1480  AIC: 1484
#> 
#> 
#> - Final Value: 96 
#> 
#> - Final Estimated Power: 0.8400 
#> - Confidence Interval: [0.7829; 0.8843]
#> - CI Level: 95.00%
n_power_region2
#> Call:
#> n_region_from_power(object = out_n_lb, seed = 24680)
#> 
#>                      Setting                                      
#> Predictor(x)         Sample Size                                  
#> Goal:                Power significantly below or above the target
#> algorithm:           bisection                                    
#> Level of confidence: 95.00%                                       
#> Target Power:        0.800                                        
#> 
#> Solution: 
#> 
#> Approximate region of sample sizes with power:
#> - not significantly different from 0.800: 71 to 96
#> - significantly lower than 0.800: 71
#> - significantly higher than 0.800: 96
#> 
#> Confidence intervals of the estimated power:
#> - for the lower bound (71): [0.665, 0.787]
#> - for the upper bound (96): [0.783, 0.884]
#> 
#> Call `summary()` for detailed results.
```

``` r
plot(n_power_region2)
```

![The Power Curve (Region) Version 2](region_plot2-1.png)

The Power Curve (Region) Version 2
