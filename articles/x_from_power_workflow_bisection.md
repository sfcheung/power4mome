# Workflow of 'x_from_power()' Using the 'bisection' Algorithm

## Goal

This technical appendix describes the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
in [power4mome](https://sfcheung.github.io/power4mome/) when using the
`"bisection"` algorithm.

## `x_from_power()`

The following is the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
when the algorithm `"bisection"` is used.

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

- `x_i`

  - The value for which power levels will be estimated in a trial.

- Termination criterion: `tol`

  - The tolerance used to determine whether the estimated power is a
    solution if the goal is “close enough” to the target power.

- Termination criterion: `delta_tol`

  - The tolerance used to determine whether the range of changes of `x`
    (sample size or effect size) in the last *k* trials is “too small.”
    Unlike some other algorithm, having similar values across trials
    that are *not* solutions (e.g., not “close enough”) indicate a
    problem in convergence, and so the algorithm will terminate.

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
