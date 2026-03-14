# Workflow of 'x_from_power()' Using the 'power_curve' Algorithm

## Goal

This technical appendix describes the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
in [power4mome](https://sfcheung.github.io/power4mome/) when using the
`"power_curve"` algorithm. This algorithm belongs to a family of
algorithm called surrogate function approximation method (see Chalmers,
2024 for a review).

## `x_from_power()`

The following is the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
when the algorithm `"power_curve"` is used. In simulation-based power
analysis, a number of replications is to be done for each value of `x`,
and this can be slow when Monte Carlo or bootstrapping confidence
intervals are involved in the test in each replication. It is not
feasible, and also not necessary, to accurately estimate the levels of
power along many values across a range of `x`, if the goal is to find a
value of `x` with estimated power close to the target power. Therefore,
steps are taken to balance speed and precision when finding the
solution.

From Power to x (Sample Size or Effect Size \[Parameter Value\])

In `x_from_power`, `x` can be a sample size (`n`) or a population value
(`es`, “effect size”) of the selected model parameter.

## Technical Details

These are the basic steps of the algorithm:

- Pre-iteration search

  - Several candidate values of `x` spanning a range is selected, using
    a small initial number of replications (`nrep0`). If
    simulation-based methods such as Monte Carlo or bootstrapping is
    used, a small initial number of resampling (`R0`) is also used.

  - The levels of power for these levels are estimated.

  - A working power curve is fitted to these values of `x` and the
    estimated levels of power.

- The search

  - The working power curve will be used to identify one or more
    probable values of `x` (`x_j`) to examine.

  - The levels of power for `x_j` are estimated.

  - The working power curve will be updated.

  - Examine the values

    - If the number of replications is not yet equal to the target
      number (`final_nrep`), then the search will continue, increasing
      the number of replications for high precision if necessary.

  - If the number of replications is already equal to the target number
    (`final_nrep`), then the values tried will be examined to see
    whether a solution has been found.

### Termination Criteria

If one of the following criteria is met, the search will end:

- A solution is found. This depends on the `goal`. For `ci_hit`, a
  solution is found if the confidence interval of the estimated power
  includes the target power. For `close_enough`, a solution is found if
  the difference between the estimated power and the target power is
  less than the tolerance (`tolerance`). The tolerance can be set
  manually by the argument `tolerance`, and its default is .02 for
  `close_enough`.

- The range of changes of `x` in the `last_k` iterations is less than
  `delta_tol`. For `power_curve`, it suggests that the search failed,
  and a new search using a different seed or different interval is
  needed. To change `delta_tol`, set it through the `control` argument
  (e.g., `control = list(delta_tol = .02)`. The number of trails (*k*),
  default to 3, can be changed through the `control` argument (e.g.,
  `control = list(last_k = 5)`).

- The maximum number of trials (iterations) for the search is reached.
  The default value is determined by the calling function, default to 10
  for `power_curve`, and can be changed through the argument
  `max_trials` of
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and friends.

Note that the working power curve is only used to assist the
identification of the values of `x` to examine. Whether a value of `x`
(e.g., a sample size) is considered a solution is determined solely by
the estimated power, and/or the confidence interval. Therefore, whether
the working power curve is “correct” along a wide range of `x` is not
essential. The working power curve serves it role as long as it can lead
to a predicted value of `x` close enough to the solution.

### The Working Power Curve

Strictly speaking, the model to be fitted is not the true power curve,
but a model that is “good enough” for the values of `x` examined so far
(that’s why these models are called surrogate functions).

The function
[`power_curve()`](https://sfcheung.github.io/power4mome/reference/power_curve.md)
is used to fit a working power curve. Unlike some previous
implementations, one or models are tried, in sequence, to determine the
working power curve. The current internal default models for sample
sizes are as following, tried in this order:

- `reject ~ 1 - I(exp((a - x) / b))`, fitted by
  [`nls()`](https://rdrr.io/r/stats/nls.html)

- `reject ~ I((x - c0)^e) / (b + I((x - c0)^e))`, fitted by
  [`nls()`](https://rdrr.io/r/stats/nls.html)

- `reject ~ x`, fitted by a logistic regression model using
  [`glm()`](https://rdrr.io/r/stats/glm.html)

- `reject ~ x`, fitted by a linear regression model using
  [`lm()`](https://rdrr.io/r/stats/lm.html).

The current internal default models for effect sizes are as following,
tried in this order:

- `reject ~ 1 - I(exp((a - x) / b))`, fitted by
  [`nls()`](https://rdrr.io/r/stats/nls.html)

- `reject ~ 1 - 1 / I((1 + (x / d)^a)^b)`, fitted by
  [`nls()`](https://rdrr.io/r/stats/nls.html)

- `reject ~ 1 - exp(x / a) / I((1 + exp(x / a))^b)`, fitted by
  [`nls()`](https://rdrr.io/r/stats/nls.html)

- `reject ~ 1 - 2 / (exp(x / d) + exp(-x / d))`, fitted by
  [`nls()`](https://rdrr.io/r/stats/nls.html)

- `reject ~ 1 / (1 + a * exp(-b * x))`, fitted by
  [`nls()`](https://rdrr.io/r/stats/nls.html)

- `reject ~ x`, fitted by a logistic regression model using
  [`glm()`](https://rdrr.io/r/stats/glm.html)

- `reject ~ x`, fitted by a linear regression model using
  [`lm()`](https://rdrr.io/r/stats/lm.html).

The term `reject` is the estimated power for
[`nls()`](https://rdrr.io/r/stats/nls.html) and
[`lm()`](https://rdrr.io/r/stats/lm.html), and the significant test
results (0 for not significant and 1 for significant) for
[`glm()`](https://rdrr.io/r/stats/glm.html) logistic regression model.
If appropriate, the number of replications will be used as the weights
when fitting a model.

The [`nls()`](https://rdrr.io/r/stats/nls.html) models will be fitted
first, and the model with the smallest deviance will be used. If all
models failed, which is possible for
[`nls()`](https://rdrr.io/r/stats/nls.html), then the logistic
regression model will be fitted. If this model also failed to fit, then
the linear model will be attempted.

Although the relation between power and `x` certainly is not linear for
a wide range of `x` (e.g., sample sizes), this algorithm only needs a
working power curve that is a good-enough approximation for the interval
being searched. Therefore, if the interval is narrow enough and close to
solution, a simple linear model can also be a good approximation.

## Annotation

- `by_x_1`

  - The collection of all values tried and their results. It is updated
    whenever new value(s) is/are tried.

- `fit_1`

  - The latest power curve estimated by `power_curve`, using the values
    tried, stored in `by_x_1`. It is updated whenever `by_x_1` is
    updated.

- `x_j`

  - The value(s) for which power levels will be estimated in a trial.

- `nrep_j`

  - The number of replications to be used when estimate the power level
    for a value of `x`. In a trial, the numbers of replications can be
    different for different values, for efficiency.

- `by_x_j`

  - The results for of
    [`power4test_by_n()`](https://sfcheung.github.io/power4mome/reference/power4test_by_n.md)
    or
    [`power4test_by_es()`](https://sfcheung.github.io/power4mome/reference/power4test_by_es.md)
    given `x_j` for a trial.

- `x_out`

  - The value of `x` which is a *candidate* solution (e.g., with
    estimated power closest to the target value).

- `power_out`, `nrep_out`, `ci_out`, `by_x_out`

  - Results based on `x_out`.

- `ci_hit`

  - Logical. Set to `TRUE` if there is at least one value of `x` with
    the confidence interval of the estimated power including the target
    power.

- `final_nrep`

  - The desired number of replications for the solution. This value
    determines the desired level of precision (the width of the
    confidence interval) in the solution.

- The sequences of values for `nrep`, `R`, and the number of `x` in a
  trial.

  - The initial number of replications (`nrep`) can be smaller than
    `final_nrep`, such that the initial trials, though with lower
    precision (wider confidence intervals), are faster to run. As the
    solution is likely to be be found (values of `x` with estimated
    power close to the target value found), `nrep` will be increased
    successively to `final_nrep`, such that a trial is slower to run but
    has a higher precision. Other values that affect the speed, such as
    the number of values of `x` (`xs_per_trial`) and the number of
    iterations (`R`) in Monte Carlo confidence intervals and
    bootstrapping, are also increased successively.

- `x_final`

  - The value of `x` in the solution (e.g., with estimated power closest
    to the target value), if found.

- `power_final`, `nrep_final`, `ci_final`, `by_x_final`

  - Results based on `x_final`.

- Main functions used

  - [`power4test_by_n()`](https://sfcheung.github.io/power4mome/reference/power4test_by_n.md)
    and
    [`power4test_by_es()`](https://sfcheung.github.io/power4mome/reference/power4test_by_es.md),
    for estimating the power levels for a set of values of `x`.

  - [`power_curve()`](https://sfcheung.github.io/power4mome/reference/power_curve.md),
    for estimating the relation between power and the value of `x`,
    based on the values of `x` having been examined.

  - The internal function `estimate_x_range()`, for determining the
    value(s) of `x` to be examined in a trial, given the value(s)
    examined so far and the tentative power curve.

## References

Chalmers, R. P. (2024). Solving variables with Monte Carlo simulation
experiments: A stochastic root-solving approach. *Psychological
Methods*. <https://doi.org/10.1037/met0000689>
