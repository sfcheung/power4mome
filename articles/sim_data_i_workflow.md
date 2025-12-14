# Workflows of \`sim_data_i()

## Goal

This technical appendix describes the workflow of the internal function
`sim_data_i()` in [power4mome](https://sfcheung.github.io/power4mome/).

## Generate Simulated Data (`sim_data_i()`)

The internal function `sim_data_i()` is called `m` times by
[`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md)
to generate `m` datasets. This is the workflow of `sim_data_i()`:

The Workflow of sim_data_i()

When generating many datasets, some of steps do not need to be repeated.
Therefore, the required objects (e.g., the parameter table) will be
supplied by
[`sim_data()`](https://sfcheung.github.io/power4mome/reference/sim_data.md),
and `sim_data_i()` will only do the data generation.
