# Workflow of \`power4test()\`

## Goal

This technical appendix describes the workflow of
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
in [power4mome](https://sfcheung.github.io/power4mome/).

## From Population to Power (`power4test()`)

The main function
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
can do all these tasks in one call:

- Generate `nrep` datasets (`nrep` replications) based on a population
  model and values of the population parameters (“effect sizes”)
  (conducted by
  [`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md)).

- Fit one or more models to each of the datasets (conducted by
  [`fit_model()`](https://sfcheung.github.io/power4mome/reference/fit_model.md)).

- (Optional) Generate Monte Carlo or bootstrap estimates based on the
  fitted model(s), for doing tests by methods such as Monte Carlo or
  bootstrap confidence intervals (conducted by
  [`gen_mc()`](https://sfcheung.github.io/power4mome/reference/gen_mc.md)
  and
  [`gen_boot()`](https://sfcheung.github.io/power4mome/reference/gen_boot.md)).

- (Optional) Do one or more tests on each of the model fit results
  (conducted by
  [`do_test()`](https://sfcheung.github.io/power4mome/reference/do_test.md)).

- Return a
  [`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md)
  object.

Power can then be estimated for each of the conducted tests by functions
such as
[`rejection_rates()`](https://sfcheung.github.io/power4mome/reference/rejection_rates.md).

The following is the workflow:

From Population to Power
