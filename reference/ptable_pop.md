# Generate the Population Model

Generate the complete population model using the model syntax and
user-specified effect sizes (population parameter values).

## Usage

``` r
ptable_pop(
  model = NULL,
  pop_es = NULL,
  es1 = c(n = 0, nil = 0, s = 0.1, m = 0.3, l = 0.5, si = 0.141, mi = 0.361, li = 0.51,
    sm = 0.2, ml = 0.4),
  es2 = c(n = 0, nil = 0, s = 0.05, m = 0.1, l = 0.15, sm = 0.075, ml = 0.125),
  es_ind = c("si", "mi", "li"),
  standardized = TRUE,
  n_std = 1e+05,
  std_force_monte_carlo = FALSE,
  add_cov_for_moderation = TRUE
)

model_matrices_pop(x, ..., drop_list_single_group = TRUE)
```

## Arguments

- model:

  String. The model defined by `lavaan` model syntax. See 'Details'.

- pop_es:

  It can be a data frame with these columns: `lhs`, `op`, `rhs`, and
  `pop`. The first three columns correspond to those in a `lavaan`
  parameter table. The column `pop` stores the population values. The
  column `es` stores the original labels, for reference. It can also be
  a named character vector (named list for multigroup models) or a
  multiline string, which are the preferred approaches. See the help
  page on how to specify this vector.

- es1:

  Set the values for each label of the effect size (population value)
  for correlations and regression paths. Used only if `pop_es` is a
  named vector or a multiline string. See the help page on how to
  specify this argument.

- es2:

  Set the values for each label of the effect size (population value)
  for product term. Used only if `pop_es` is a named vector or a
  multiline string. See the help page on how to specify this argument.

- es_ind:

  The names of labels denoting the effect size of an indirect effect.
  They will be used to determine the population values of the component
  paths along an indirect path.

- standardized:

  Logical. If `TRUE`, the default, variances and error variances are
  scaled to ensure the population variances of the endogenous variables
  are close to one, and hence the effect sizes (population values) are
  standardized effect sizes if the variances of the continuous exogenous
  variables are also equal to one.

- n_std:

  The sample size used to determine the error variances by simulation
  when `std_force_monte_carlo` is `TRUE`.

- std_force_monte_carlo:

  Logical. If `FALSE`, the default, standardization is done analytically
  if the model has no product terms, and by simulation if the model has
  product terms. That is, error variances required to ensure implied
  variances equal to one are determined by simulation. If `TRUE`,
  simulation will be used whether the model has product terms or not.
  Always fall back to simulation if analytical standardization failed.

- add_cov_for_moderation:

  Logical. If `TRUE`, the default, for a model in which one or product
  terms for moderation involve one or more mediator, covariances between
  their error terms and the product terms will be added automatically.
  If these covariances are not added, the model may not be invariant to
  linear transformation of some variables in the model.

- x:

  It can be a 'lavaan' model syntax, to be passed to `ptable_pop()`, or
  a parameter table with the column `start` set to the population
  values, such as the output of `ptable_pop()`.

- ...:

  If `x` is a model syntax, these are arguments to be passed to
  `ptable_pop()`.

- drop_list_single_group:

  If `TRUE` and the number of groups is equal to one, the output will be
  a list of matrices of one group only. Default if `TRUE`.

## Value

The function `ptable_pop()` returns a `lavaan` parameter table of the
model, with the column `start` set to the population values.

The function `model_matrices_pop()` returns a `lavaan` LISREL-style
model matrices (like the output of
[`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
with `what` set to `"free"`), with matrix elements set to the population
values. If `x` is the model syntax, it will be stored in the attributes
`model`. If the model is a multigroup model with *k* groups (*k* greater
than 1), then it returns a list of *k* lists of `lavaan` LISREL-style
model matrices unless `drop_list_single_group` is `TRUE`.

## Details

The function `ptable_pop()` generates a `lavaan` parameter table that
can be used to generate data based on the population values of model
parameters.

## The role of `ptable_pop()`

The function `ptable_pop()` is used by the all-in-one function
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).
Users usually do not call this function directly, though developers can
use this function to develop other functions for power analysis, or to
build their own workflows to do the power analysis.

## Specify the Population Model by 'model'

### Single-Group Model

For a single-group model, `model` should be a `lavaan` model syntax
string of the *form* of the model. The population values of the model
parameters are to be determined by `pop_es`.

If the model has latent factors, the syntax in `model` should specify
only the *structural model* for the *latent factors*. There is no need
to specify the measurement part. Other functions will generate the
measurement part on top of this model.

For example, this is a simple mediation model:

    "m ~ x
     y ~ m + x"

Whether `m`, `x`, and `y` denote observed variables or latent factors
are determined by other functions, such as
[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md).

### Multigroup Model

Because the model is the population model, equality constraints are
irrelevant and the model syntax specifies only the *form* of the model.
Therefore, `model` is specified as in the case of single group models.

## Specify 'pop_es' Using Named Vectors

The argument `pop_es` is for specifying the population values of model
parameters. This section describes how to do this using named vectors.

### Single-Group Model

If `pop_es` is specified by a named vector, it must follow the
convention below.

- The names of the vectors are `lavaan` names for the selected
  parameters. For example, `m ~ x` denotes the path from `x` to `m`.

- Alternatively, the names can be either `".beta."` or `".cov."`. Use
  `".beta."` to set the default values for all regression coefficients.
  Use `".cov."` to set the default values for all correlations of
  exogenous variables (e.g., predictors).

- The names can also be of this form: `".ind.(<path>)"`, whether
  `<path>` denote path in the model. For example, `".ind.(x->m->y)"`
  denotes the path from `x` through `m` to `y`. Alternatively, the
  `lavaan` symbol `~` can also be used: `".ind.(y~m~x)"`. This form is
  used to set the indirect effect (standardized, by default) along this
  path. The value for this name will override other settings.

- If using `lavaan` names, can specify more than one parameter using
  `+`. For example, `y ~ m + x` denotes the two paths from `m` and `x`
  to `y`.

- The value of each element can be the label for the effect size: `n`
  for nil, `s` for small, `m` for medium, and `l` for large. The value
  for each label is determined by `es1` and `es2`. See the section on
  specifying these two arguments.

- The value of `pop_es` can also be set to a value, but it must be
  quoted as a string, such as `"y ~ x" = ".31"`.

This is an example:

    c(".beta." = "s",
      "m1 ~ x" = "-m",
      "m2 ~ m1" = "l",
      "y ~ x:w" = "s")

In this example,

- All regression coefficients are set to small (`s`) by default, unless
  specified otherwise.

- The path from `x` to `m1` is set to medium and negative (`-m`).

- The path from `m1` to `m2` is set to large (`l`).

- The coefficient of the product term `x:w` when predicting `y` is set
  to small (`s`).

#### Indirect Effect

When setting an indirect effect to a symbol (default: `"si"`, `"mi"`,
`"li"`, with `"i"` added to differentiate them from the labels for a
direct path), the corresponding value is used to determine the
population values of *all* component paths along the pathway. All the
values are assumed to be equal. Therefore, `".ind.(x->m->y)" = ".20"` is
equivalent to setting `m ~ x` and `y ~ m` to the square root of .20,
such that the corresponding indirect effect is equal to the designated
value.

This behavior, though restricted, is for quick manipulation of the
indirect effect. If different values along a pathway, set the value for
each path directly.

Only nonnegative value is supported. Therefore,
`".ind.(x->m->y)" = "-si"` and `".ind.(x->m->y)" = "-.20"` will throw an
error.

### Multigroup Model

The argument `pop_es` also supports multigroup models.

For `pop_es`, instead of named vectors, named *list* of named vectors
should be used.

- The names are the parameters, or keywords such as `.beta.` and
  `.cov.`, like specifying the population values for a single group
  model.

- The elements are character vectors. If it has only one element (e.g.,
  a single string), then it is the the population value for all groups.
  If it has more than one element (e.g., a vector of three strings),
  then they are the population values of the groups. For a model of *k*
  groups, each vector must have either *k* elements or one element.

This is an example:

    list("m ~ x" = "m",
         "y ~ m" = c("s", "m", "l"))

In this model, the population value of the path `m ~ x` is medium (`m`)
for all groups, while the population values for the path `y ~ m` are
small (`s`), medium (`m`), and large (`l`), respectively.

## Specify 'pop_es' Using a Multiline String

When setting the argument `pop_es`, instead of using a named vector or
named list for `pop_es`, the population values of model parameters can
also be specified using a multiline string, as illustrated below, to be
parsed by
[`pop_es_yaml()`](https://sfcheung.github.io/power4mome/reference/pop_es_yaml.md).

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
  in `ptable_pop()`.

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

## Set the Values for Effect Size Labels ('es1' and 'es2')

The vector `es1` is for correlations, regression coefficients, and
indirect effect, and the vector `es2` is for for standardized moderation
effect, the coefficients of a product term. These labels are to be used
in interpreting the specification in `pop_es`.

## The role of `model_matrices_pop()`

The function `model_matrices_pop()` generates models matrices with
population values, used by `ptable_pop()`. Users usually do not call
this function directly, though developers can use this build their own
workflows to generate the data.

## See also

[`power4test()`](https://sfcheung.github.io/power4mome/reference/power4test.md),
and
[`pop_es_yaml()`](https://sfcheung.github.io/power4mome/reference/pop_es_yaml.md)
on an alternative way to specify population values.

## Examples

``` r
# Specify the model

model1 <-
"
m1 ~ x + c1
m2 ~ m1 + x2 + c1
y ~  m2 + m1 + x + w + x:w + c1
"

# Specify the population values

model1_es <- c("m1 ~ x" = "-m",
               "m2 ~ m1" = "s",
               "y ~ m2" = "l",
               "y ~ x" = "m",
               "y ~ w" = "s",
               "y ~ x:w" = "s",
               "x ~~ w" = "s")

ptable_final1 <- ptable_pop(model1,
                            pop_es = model1_es)
ptable_final1
#>    id lhs op rhs group user block free ustart exo label plabel      start est
#> 1   1  m1  ~   x     1    1     1    1     NA   0         .p1. -0.3000000   0
#> 2   2  m1  ~  c1     1    1     1    2     NA   0         .p2.  0.0000000   0
#> 3   3  m2  ~  m1     1    1     1    3     NA   0         .p3.  0.1000000   0
#> 4   4  m2  ~  x2     1    1     1    4     NA   0         .p4.  0.0000000   0
#> 5   5  m2  ~  c1     1    1     1    5     NA   0         .p5.  0.0000000   0
#> 6   6   y  ~  m2     1    1     1    6     NA   0         .p6.  0.5000000   0
#> 7   7   y  ~  m1     1    1     1    7     NA   0         .p7.  0.0000000   0
#> 8   8   y  ~   x     1    1     1    8     NA   0         .p8.  0.3000000   0
#> 9   9   y  ~   w     1    1     1    9     NA   0         .p9.  0.1000000   0
#> 10 10   y  ~ x:w     1    1     1   10     NA   0        .p10.  0.0500000   0
#> 11 11   y  ~  c1     1    1     1   11     NA   0        .p11.  0.0000000   0
#> 12 12  m1 ~~  m1     1    0     1   12     NA   0        .p12.  0.9102952   1
#> 13 13  m2 ~~  m2     1    0     1   13     NA   0        .p13.  0.9899976   1
#> 14 14   y ~~   y     1    0     1   14     NA   0        .p14.  0.6509433   1
#> 15 15   x ~~   x     1    0     1   15     NA   0        .p15.  1.0000000   1
#> 16 16   x ~~  c1     1    0     1   16     NA   0        .p16.  0.0000000   0
#> 17 17   x ~~  x2     1    0     1   17     NA   0        .p17.  0.0000000   0
#> 18 18   x ~~   w     1    0     1   18     NA   0        .p18.  0.1000000   0
#> 19 19   x ~~ x:w     1    0     1   19     NA   0        .p19.  0.0000000   0
#> 20 20  c1 ~~  c1     1    0     1   20     NA   0        .p20.  1.0000000   1
#> 21 21  c1 ~~  x2     1    0     1   21     NA   0        .p21.  0.0000000   0
#> 22 22  c1 ~~   w     1    0     1   22     NA   0        .p22.  0.0000000   0
#> 23 23  c1 ~~ x:w     1    0     1   23     NA   0        .p23.  0.0000000   0
#> 24 24  x2 ~~  x2     1    0     1   24     NA   0        .p24.  1.0000000   1
#> 25 25  x2 ~~   w     1    0     1   25     NA   0        .p25.  0.0000000   0
#> 26 26  x2 ~~ x:w     1    0     1   26     NA   0        .p26.  0.0000000   0
#> 27 27   w ~~   w     1    0     1   27     NA   0        .p27.  1.0000000   1
#> 28 28   w ~~ x:w     1    0     1   28     NA   0        .p28.  0.0000000   0
#> 29 29 x:w ~~ x:w     1    0     1   29     NA   0        .p29.  1.0000000   1

# Use a multiline string, illustrated by a simpler model

model2 <-
"
m ~ x
y ~ m + x
"

model2_es_a <- c("m ~ x" = "s",
                 "y ~ m" = "m",
                 "y ~ x" = "nil")

model2_es_b <-
"
m ~ x: s
y ~ m: m
y ~ x: nil
"

ptable_model2_a <- ptable_pop(model2,
                              pop_es = model2_es_a)
ptable_model2_b <- ptable_pop(model2,
                              pop_es = model2_es_b)

ptable_model2_a
#>   id lhs op rhs group user block free ustart exo label plabel start est
#> 1  1   m  ~   x     1    1     1    1     NA   0         .p1.  0.10   0
#> 2  2   y  ~   m     1    1     1    2     NA   0         .p2.  0.30   0
#> 3  3   y  ~   x     1    1     1    3     NA   0         .p3.  0.00   0
#> 4  4   m ~~   m     1    0     1    4     NA   0         .p4.  0.99   1
#> 5  5   y ~~   y     1    0     1    5     NA   0         .p5.  0.91   1
#> 6  6   x ~~   x     1    0     1    6     NA   0         .p6.  1.00   1
ptable_model2_b
#>   id lhs op rhs group user block free ustart exo label plabel start est
#> 1  1   m  ~   x     1    1     1    1     NA   0         .p1.  0.10   0
#> 2  2   y  ~   m     1    1     1    2     NA   0         .p2.  0.30   0
#> 3  3   y  ~   x     1    1     1    3     NA   0         .p3.  0.00   0
#> 4  4   m ~~   m     1    0     1    4     NA   0         .p4.  0.99   1
#> 5  5   y ~~   y     1    0     1    5     NA   0         .p5.  0.91   1
#> 6  6   x ~~   x     1    0     1    6     NA   0         .p6.  1.00   1

identical(ptable_model2_a,
          ptable_model2_b)
#> [1] TRUE

# model_matrices_pop

model_matrices_pop(ptable_final1)
#> $lambda
#>     m1 m2 y x c1 x2 w x:w
#> m1   0  0 0 0  0  0 0   0
#> m2   0  0 0 0  0  0 0   0
#> y    0  0 0 0  0  0 0   0
#> x    0  0 0 0  0  0 0   0
#> c1   0  0 0 0  0  0 0   0
#> x2   0  0 0 0  0  0 0   0
#> w    0  0 0 0  0  0 0   0
#> x:w  0  0 0 0  0  0 0   0
#> 
#> $theta
#>     m1 m2 y x c1 x2 w x:w
#> m1   0                   
#> m2   0  0                
#> y    0  0 0              
#> x    0  0 0 0            
#> c1   0  0 0 0  0         
#> x2   0  0 0 0  0  0      
#> w    0  0 0 0  0  0 0    
#> x:w  0  0 0 0  0  0 0   0
#> 
#> $psi
#>        m1    m2     y     x    c1    x2     w   x:w
#> m1  0.910                                          
#> m2  0.000 0.990                                    
#> y   0.000 0.000 0.651                              
#> x   0.000 0.000 0.000 1.000                        
#> c1  0.000 0.000 0.000 0.000 1.000                  
#> x2  0.000 0.000 0.000 0.000 0.000 1.000            
#> w   0.000 0.000 0.000 0.100 0.000 0.000 1.000      
#> x:w 0.000 0.000 0.000 0.000 0.000 0.000 0.000 1.000
#> 
#> $beta
#>      m1  m2 y    x c1 x2   w  x:w
#> m1  0.0 0.0 0 -0.3  0  0 0.0 0.00
#> m2  0.1 0.0 0  0.0  0  0 0.0 0.00
#> y   0.0 0.5 0  0.3  0  0 0.1 0.05
#> x   0.0 0.0 0  0.0  0  0 0.0 0.00
#> c1  0.0 0.0 0  0.0  0  0 0.0 0.00
#> x2  0.0 0.0 0  0.0  0  0 0.0 0.00
#> w   0.0 0.0 0  0.0  0  0 0.0 0.00
#> x:w 0.0 0.0 0  0.0  0  0 0.0 0.00
#> 
#> attr(,"model")
#> [1] "\nm1 ~ x + c1\nm2 ~ m1 + x2 + c1\ny ~  m2 + m1 + x + w + x:w + c1\n"

model_matrices_pop(model1,
                   pop_es = model1_es)
#> $lambda
#>     m1 m2 y x c1 x2 w x:w
#> m1   0  0 0 0  0  0 0   0
#> m2   0  0 0 0  0  0 0   0
#> y    0  0 0 0  0  0 0   0
#> x    0  0 0 0  0  0 0   0
#> c1   0  0 0 0  0  0 0   0
#> x2   0  0 0 0  0  0 0   0
#> w    0  0 0 0  0  0 0   0
#> x:w  0  0 0 0  0  0 0   0
#> 
#> $theta
#>     m1 m2 y x c1 x2 w x:w
#> m1   0                   
#> m2   0  0                
#> y    0  0 0              
#> x    0  0 0 0            
#> c1   0  0 0 0  0         
#> x2   0  0 0 0  0  0      
#> w    0  0 0 0  0  0 0    
#> x:w  0  0 0 0  0  0 0   0
#> 
#> $psi
#>       m1   m2    y    x   c1   x2    w  x:w
#> m1  0.91                                   
#> m2  0.00 0.99                              
#> y   0.00 0.00 0.65                         
#> x   0.00 0.00 0.00 1.00                    
#> c1  0.00 0.00 0.00 0.00 1.00               
#> x2  0.00 0.00 0.00 0.00 0.00 1.00          
#> w   0.00 0.00 0.00 0.10 0.00 0.00 1.00     
#> x:w 0.00 0.00 0.00 0.00 0.00 0.00 0.00 1.00
#> 
#> $beta
#>      m1  m2 y    x c1 x2   w  x:w
#> m1  0.0 0.0 0 -0.3  0  0 0.0 0.00
#> m2  0.1 0.0 0  0.0  0  0 0.0 0.00
#> y   0.0 0.5 0  0.3  0  0 0.1 0.05
#> x   0.0 0.0 0  0.0  0  0 0.0 0.00
#> c1  0.0 0.0 0  0.0  0  0 0.0 0.00
#> x2  0.0 0.0 0  0.0  0  0 0.0 0.00
#> w   0.0 0.0 0  0.0  0  0 0.0 0.00
#> x:w 0.0 0.0 0  0.0  0  0 0.0 0.00
#> 
#> attr(,"model")
#> [1] "\nm1 ~ x + c1\nm2 ~ m1 + x2 + c1\ny ~  m2 + m1 + x + w + x:w + c1\n"
```
