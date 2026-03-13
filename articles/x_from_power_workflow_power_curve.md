# Workflow of 'x_from_power()' Using the 'power_curve' Algorithm

## Goal

This technical appendix describes the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
in [power4mome](https://sfcheung.github.io/power4mome/) when using the
`"power_curve"` algorithm.

## `x_from_power()`

The following is the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
when the algorithm `"power_curve"` is used. Simulation is to be done for
each value of `x`, and this can be slow when Monte Carlo or
bootstrapping confidence intervals are involved in the test. It is not
feasible, and also not necessary, to accurately estimate the levels of
power along many values across a range of `x`, if the goal is to find
*the* value of `x` with the target power. Therefore, steps are taken to
balance speed and precision when finding the solution.

From Power to x (Sample Size or Effect Size \[Parameter Value\])

In `x_from_power`, `x` can be a sample size (`n`) or a population value
(`es`, “effect size”) of the selected model parameter.

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
