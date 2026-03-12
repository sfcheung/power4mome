# Workflow of 'x_from_power()' Using the 'probabilistic_bisection' Algorithm

## Goal

This technical appendix describes the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
in [power4mome](https://sfcheung.github.io/power4mome/) when using the
`"probabilistic_bisection"` algorithm.

## `x_from_power()`

The following is the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
when the algorithm `"probabilistic_bisection"` is used.

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

  - The value(s) for which power levels will be estimated in a trial.

- `final_nrep`

  - The desired number of replications for the solution. This value
    determines the desired level of precision (the width of the
    confidence interval) in the solution.

- `final_R`

  - The desired number of R for bootstrapping and Monte Carlo simulation
    for the solution.

- Termination criterion: `tol`

  - The tolerance used to determine whether the estimated power is
    “close enough” to the target power. If not set, determined by the
    confidence interval for a power equal to the target power, given
    `final_nrep`. Therefore, the larger the `final_nrep`, the smaller
    the tolerance. Used only when doing a final check using
    `final_nrep`.

- Termination criterion: `hdr_power_tol`

  - The tolerance used to determine whether the width of the
    highest-density region of power is narrow enough. If not set,
    determined by the confidence interval for a power equal to the
    target power, given `final_nrep`. Therefore, the larger the
    `final_nrep`, the smaller the tolerance. It is used to determine
    whether a final check using `final_nrep` will be conducted next.

- Termination criterion: `delta_tol`

  - The tolerance used to determine whether the range of changes of `x`
    (sample size or effect size) in the last *k* trials is small enough.
    It is used to determine whether a final check using `final_nrep`
    will be conducted next.

- Termination criterion: `delta_tol_f`

  - The tolerance used to determine whether the range of changes of the
    objective function value in the last *k* trials is small enough. It
    is used to determine whether a final check using `final_nrep` will
    be conducted next. Although used in the check, this criterion is
    rarely met because the number of replications per trial (`nrep_i`)
    is designed to be small (e.g., around 50) for efficiency.

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
