# Workflow of 'x_from_power()' Using the 'bisection' Algorithm

## Goal

This technical appendix describes the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
in [power4mome](https://sfcheung.github.io/power4mome/) when using the
`"bisection"` algorithm. This algorithm, called the informal bisection
algorithm in Chalmers (2024), uses a modified version of the bisection
method.

## `x_from_power()`

The following is the workflow of
[`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md),
when the algorithm `"bisection"` is used.

From Power to x (Sample Size or Effect Size \[Parameter Value\])

In `x_from_power`, `x` can be a sample size (`n`) or a population value
(`es`, “effect size”) of the selected model parameter.

## Technical Details

These are the basic steps of the algorithm:

- Pre-iteration search

  - An initial interval of `x` will be determined. For the
    `close_enough` goal, the estimated differences in the levels of
    power of the lower and upper bounds from the target power must be of
    opposite signs (e.g., the power). For example, if the target power
    is .80 and `x` is sample size, then the lower bound must have an
    estimated power less than .80 and the upper bound must have an
    estimated power higher than .80. This initial interval can be a wide
    one, and it is preferable to have a wide interval due to the random
    error in estimating the power.

- The search

  - The mean of the interval is initially used as a candidate solution
    (`x_i`).

  - The level of power for `x_i` will be estimated.

  - If `x_i` is a solution (e.g., the estimated power is “close enough”
    to the target power), then the search will end.

  - If `x_i` is not a solution, the interval will be updated using `x_i`
    as one of the bounds, such that estimated level of power of one of
    the bounds is above the target power and that of the other bond is
    below the target power.

  - If some conditions are met, the interval will be expanded before
    doing the next iteration.

Due to the random nature of the estimation, unlike using the bisection
method for deterministic function (the same value of `x` guarantees the
same value of `y`), it is possible that solution is outside the interval
after several iterations. One sign for this is having a narrow interval
but the mean is not a solution. Whether an interval is too narrow is
controlled by `min_interval_width`, which is 2 for sample size and .001
for effect size.

If this happens, the algorithm will try to expand the interval by a
certain amount and use this expanded interval for the next iteration.

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
  `delta_tol`. For `bisection`, it suggests that the search failed, and
  a new search using a different seed or different interval is needed.
  To change `delta_tol`, set it through the `control` argument (e.g.,
  `control = list(delta_tol = .02)`. The number of trails (*k*), default
  to 3, can be changed through the `control` argument (e.g.,
  `control = list(last_k = 5)`).

- The maximum number of trials (iterations) for the search is reached.
  The default value is determined by the calling function, default to 10
  for `bisection`, and can be changed through the argument `max_trials`
  of
  [`x_from_power()`](https://sfcheung.github.io/power4mome/reference/x_from_power.md)
  and friends.

Note that interval is only used to find a solution. Whether a value of
`x` (e.g., a sample size) is considered a solution is determined solely
by the estimated power, and/or the confidence interval.

### Determining the value to examine

The original bisection method use the mean (center) of the interval as
the value to examine in an iteration. The current implementation
supports two more variants, and the power-curve assited method is
enabled by default.

- Muller’s Method

  This version is supported (adding `control = list(muller = TRUE`).
  However, preliminary tests found that this method sometimes failed
  even in simple scenarios. Therefore, it is not enabled by default.

- Power-Curve Assisted Method

  This method finds a working power curve for the current interval,
  using all values of `x` examined inside this interval, and use this
  working power curve to estimate the solution. If this estimated
  solution is also inside the interval, then this solution will be used
  as `x_i` in the next iteration. This method is enabled by default. To
  disable it, use `control = list(use_power_curve_assist = FALSE)`. If
  `use_power_curve_hybrid` is `TRUE`, the default, then the mean of the
  power-curve-assisted value and the center of the interval will be use
  as `x_i` in the next iteration. To disable this, use
  `control = list(use_power_curve_hybrid = FALSE)`.

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

Chalmers, R. P. (2024). Solving variables with Monte Carlo simulation
experiments: A stochastic root-solving approach. *Psychological
Methods*. <https://doi.org/10.1037/met0000689>
