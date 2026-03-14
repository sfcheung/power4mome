# Workflow of 'x_from_power()' Using the 'probabilistic_bisection' Algorithm

## Goal

This technical appendix describes the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
in [power4mome](https://sfcheung.github.io/power4mome/) when using the
`"probabilistic_bisection"` algorithm, based on the algorithm proposed
by Waeber et al. (2013), applied to power analysis by Chalmers (2024).

## `x_from_power()`

The following is the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
when the algorithm `"probabilistic_bisection"` is used.

From Power to x (Sample Size or Effect Size \[Parameter Value\])

In `x_from_power`, `x` can be a sample size (`n`) or a population value
(`es`, “effect size”) of the selected model parameter.

## Technical Details

- The interval used in the search is fixed throughout the search. Users
  can set the interval manually if so desired.

### Termination Criteria

The following criteria are used to decide when the search will be ended.

- `tol`

  The tolerance used to determine whether the estimated power is “close
  enough” to the target power. If not set, determined by the confidence
  interval for a power equal to the target power, given `final_nrep`
  (the number of replications for the solution) and `ci_level` (the
  level of confidence for the estimated power): the distance from the
  confidence limit closet to the target power, multiplied by .90.
  Therefore, the larger the `final_nrep`, the smaller the tolerance.
  Used only when doing a final check using `final_nrep`. When calling
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and related functions, it can be set manually by the argument
  `tolerance`. It is used to determine whether a value of `x` is
  considered to be a solution. The default value ensured that the
  confidence interval of the solution includes the target power.

- `max_final_checks`

  The maximum number of final checks. It is possible that a candidate
  value turned out does not meet the tolerance (the estimated power not
  “close enough” to the target power). The search will then continue.
  The value `max_final_checks` determine the maximum number of final
  checks to try. If this number of checks is reached but solution not
  found, the search will terminate. The default value is 5, and can be
  changed through the `control` argument (e.g.,
  `control = list(max_final_checks = 2)`).

- `max_trials`

  The maximum number of trials (iterations) for the search. If this
  number of trials is reached, the search will stop. The default value
  is determined by the calling function, default to 100 for
  `probabilistic_bisection`, and can be changed through the argument
  `max_trials` of
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and friends.

- `total_nrep`

  The total number of replications for the whole search process. If this
  number is reached, the search will stop. The default value is
  determined by the calling function, default to 5000, and can be
  changed through the `control` argument (e.g.,
  `control = list(total_nrep = 2)`).

### Number of Replications in Each Iteration

The number of replications in a trial (`nrep_i`) is
`total_nrep / max_trials`. Therefore, the default number of iterations
is 50. It will be adjusted upward such that the estimated power in an
iteration will not be exactly equal to the target power. Therefore, with
a target power of .80, the default number of iterations is 51, because
an estimated power of exactly .80 is possible with 50 replications.

### Final Checks

A value will be considered a solution only if it passed the final check,
carried out using the number of replications equal to `final_nrep`.

In an iteration, after the density function is updated, the next
iteration will be final check if

- there are still unused attempts for final checks available (i.e., the
  number of attempted final checks is not equal to `max_final_checks`),
  and

- the iteration is not during the cooldown period after the previous
  final check, if any,

and at least one of the following criteria is met (described in details
next):

- The range of changes of the candidate value (`x_i`), such as the
  sample size, in the `last_k` iterations is less than `delta_tol`.

- The range of changes of the function values (the deviation of the
  estimated power from the target power, if the goal is “close_enough”)
  in the `last_k` iterations is less than `delta_tol_f`.

- The width of the (derived) highest-density region of the estimated
  power is less than or equal to `hdr_power_tol` *and* the region
  includes the target power.

If a solution is found in a final check, the search will terminate.

If a solution is not found and this final check is not the last one
available, the search will resume. The next `final_check_cooldown`
iterations (default to `last_k`) will be the cooldown period and final
check will not be carried out regardless of other criteria.

### Final Check Criteria

The following criteria will be used to determine whether a final check
will be conducted next.

- `hdr_power_tol`

  The tolerance used to determine whether the width of the
  highest-density region of power is narrow enough. If not set, to be
  conservative, it is set to the .90 of the width of the confidence
  interval for a power equal to the target power, given `final_nrep`.
  Therefore, the larger the `final_nrep`, the smaller the tolerance. It
  is used to determine whether a final check using `final_nrep` will be
  conducted next. To change it, set it through the `control` argument
  (e.g., `control = list(hdr_power_tol = .02)`.

- `delta_tol`

  The tolerance used to determine whether the range of changes of `x`
  (sample size or effect size) in the last *k* trials is small enough.
  If the change is small enough, a solution may have been found. It is
  used to determine whether a final check using `final_nrep` will be
  conducted next. The default value is 2 for `n` (sample size) and .002
  for `es` (effect size). To change it, set it through the `control`
  argument (e.g., `control = list(delta_tol = .02)`. The number of
  trails (*k*), default to 5, can be changed through the `control`
  argument (e.g., `control = list(last_k = 3)`).

- `delta_tol_f`

  The tolerance used to determine whether the range of changes of the
  objective function value in the last *k* trials is small enough. It is
  used to determine whether a final check using `final_nrep` will be
  conducted next. The default value is half of the width of the
  confidence interval at the target power. Although used in the check,
  this criterion is rarely met because the number of replications per
  trial (`nrep_i`) is designed to be small (e.g., around 50) for
  efficiency. To change it, set it through the `control` argument (e.g.,
  `control = list(delta_tol_f = .02)`. The number of trails (*k*),
  default to 5, can be changed through the `control` argument (e.g.,
  `control = list(last_k = 3)`).

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

## References

Chalmers, R. P. (2024). Solving variables with Monte Carlo simulation
experiments: A stochastic root-solving approach. *Psychological
Methods*. <https://doi.org/10.1037/met0000689>

Waeber, R., Frazier, P. I., & Henderson, S. G. (2013). Bisection search
with noisy responses. *SIAM Journal on Control and Optimization*,
*51*(3), 2261–2279. <https://doi.org/10.1137/120861898>
