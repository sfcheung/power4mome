# Parse YAML-Stye Values For 'pop_es'

Convert a YAML string to a vector or list of `pop_es` specification.

## Usage

``` r
pop_es_yaml(text)
```

## Arguments

- text:

  The multiline string to be parsed to a specification of population
  values.

## Value

Either a named vector (for a single-group model) or a list of named
vector (for a multigroup model) of the population values of the
parameters (the effect sizes).

## Details

The function `pop_es_yaml()` allows users to specify population values
of a model using one single string, as in 'lavaan' model syntax.

## Specify 'pop_es' Using a Multiline String

When setting the argument `pop_es`, instead of using a named vector or
named list for `pop_es`, the population values of model parameters can
also be specified using a multiline string, as illustrated below, to be
parsed by `pop_es_yaml()`.

### Single-Group Model

This is an example of the multiline string for a single-group model:

    y ~ m: l
    m ~ x: m
    y ~ x: nil

The string must follow this format:

- Each line starts with `tag:`.

  - `tag` can be the name of a parameter, in `lavaan` model syntax
    format.

    - For example, `m ~ x` denotes the path from `x` to `m`.

  - A tag in `lavaan` model syntax can specify more than one parameter
    using `+`.

    - For example, `y ~ m + x` denotes the two paths from `m` and `x` to
      `y`.

  - Alternatively, the `tag` can be either `.beta.` or `.cov.`.

    - Use `.beta.` to set the default values for all regression
      coefficients.

    - Use `.cov.` to set the default values for all correlations of
      exogenous variables (e.g., predictors).

- After each tag is the value of the population value:

  \-`nil` for nil (zero),

  - `s` for small,

  - `m` for medium, and

  - `l` for large.

  - `si`, `mi`, and `li` for small, medium, and large a standardized
    indirect effect, respectively.

  Note: `n` *cannot* be used in this mode.

  The value for each label is determined by `es1` and `es2` as described
  in
  [`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md).

  - The value can also be set to a numeric value, such as `.30` or
    `-.30`.

This is another example:

    .beta.: s
    y ~ m: l

In this example, all regression coefficients are `small`, while the path
from `m` to `y` is large.

### Multigroup Model

This is an example of the string for a multigroup model:

    y ~ m: l
    m ~ x:
      - nil
      - s
    y ~ x: nil

The format is similar to that for a single-group model. If a parameter
has the same value for all groups, then the line can be specified as in
the case of a single-group model: `tag: value`.

If a parameter has different values across groups, then it must be in
this format:

- A line starts with the tag, followed by two or more lines. Each line
  starts with a hyphen `-` and the value for a group.

For example:

    m ~ x:
      - nil
      - s

This denotes that the model has two groups. The values of the path from
`x` to `m` for the two groups are 0 (`nil`) and small (`s`),
respectively.

Another equivalent way to specify the values are using `[]`, on the same
line of a tag.

For example:

    m ~ x: [nil, s]

The number of groups is inferred from the number of values for a
parameter. Therefore, if a tag has more than one value, each tag must
has the same number of value, or only one value.

The tag `.beta.` and `.cov.` can also be used for multigroup models.

### Which Approach To Use

Note that using named vectors or named lists is more reliable. However,
using a multiline string is more user-friendly. If this method failed,
please use named vectors or named list instead.

### Technical Details

The multiline string is parsed by
[`yaml::read_yaml()`](https://yaml.r-lib.org/reference/read_yaml.html).
Therefore, the format requirement is actually that of YAML. Users
knowledgeable of YAML can use other equivalent way to specify the
string.

## See also

[`ptable_pop()`](https://sfcheung.github.io/power4mome/reference/ptable_pop.md),
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
and other functions that have the `pop_es` argument.

## Examples

``` r
mod_es <- c("y ~ m" = "l",
            "m ~ x" = "m",
            "y ~ x" = "nil")

mod_es_yaml <-
"
y ~ m: l
m ~ x: m
y ~ x: nil
"

pop_es_yaml(mod_es_yaml)
#> y ~ m m ~ x y ~ x 
#>   "l"   "m" "nil" 

```
