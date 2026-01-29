# power4mome

(Version 0.1.1.31, updated on 2026-01-29, [release
history](https://sfcheung.github.io/power4mome/news/index.html))

This package is for power analysis and sample size determination for
moderation, mediation, and moderated mediation.

It includes functions for power analysis and sample size determination
for moderation, mediation, and moderated mediation effects in models
fitted by structural equation modeling (SEM) or multiple linear
regression. For SEM, both latent variable models and path models of
observed variables are supported.

For more information on this package, please visit its GitHub page:

<https://sfcheung.github.io/power4mome/>

A get-started guide illustrates how to use this package:

<https://sfcheung.github.io/power4mome/articles/power4mome.html>

Code templates are also available for common mediation, moderation, and
moderated mediation models:

<https://sfcheung.github.io/power4mome/articles/>

# Philosophy

The package was developed with this philosophy:

- Easy to specify the population models, even with latent variables.

- As few manually-set numbers as possible when specifying the population
  models, with convenient default values.

- As few restrictions as possible on the form of the models, even when
  the models have latent factors.

- As automatic as possible in finding the sample size with the target
  power.

To achieve this comes with some costs, and some of the goals conflict
with other goals: Being flexible usually means being less user-friendly,
and being easy to specify the model usually means not supporting some
models.

Therefore, we also try to

- balance these goals, and

- allow the functions to be used in different ways, to accommodate
  scenarios that prioritize these goals differently.

# Installation

The stable version at CRAN can be installed by
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html):

``` r
install.packages("power4mome")
```

The latest developmental version of this package can be installed by
`remotes::install_github`:

``` r
remotes::install_github("sfcheung/power4mome")
```

# Background

Some of us the developers have developed the package
[`manymome`](https://sfcheung.github.io/manymome/) (Cheung & Cheung,
2024) for computing and testing effects in models with mediation,
mediation, or moderated mediation. The tests are usually done by
simulation-based methods such as Monte Carlo or bootstrap confidence
intervals, due to the complicated sampling distributions of the effects.
Therefore, there are no simple ways to determine the power of the test
analytically and accurately. The computation becomes more complicated
when latent variables are involved, necessitating a simulation-based
method to estimate the sample size.

There are already many excellent packages out there for estimating power
in structural equation modeling in general, and some are also
specifically for mediation or moderated mediation. We are not intended
to replace with them or reinvent the wheel. We just want to have a tool
that meet our own needs:

1.  It leverages on the flexibility of `manymome` in testing an indirect
    effect or conditional effect with little limitations on the model.

2.  It allows users (us and our collaborators) to specify the population
    model as easy (quickly) as typical power analysis programs.

We ourselves know how to do the power estimation on our own by
simulation, if necessary. However, time is usually a concern, and we
would like to have a tool that, though specifically designed with
mediation, moderation, and moderated mediation in mind and may be
limited in scope (though it is a “big” scope), is easy for our daily use
in estimating power.

So here it is, `power4mome`, developed with we ourselves as the users,
but we believe are also useful for others who need to do power analysis
for mediation, moderation, and moderated mediation.

# Not Just That …

But `power4mome` is not just for mediation, moderation, and moderated
mediation. We avoided wrote the functions just for these effects, and
have left room for testing other effects, as hinted in some examples in
the help pages. They may be introduced later. For now, supporting
effects that can be tested by `manymome` is our priority.

# Issues

If you have any suggestions and found any bugs, please feel free to open
a GitHub issue:

<https://github.com/sfcheung/power4mome/issues>

Thanks.

# Reference

Cheung, S. F., & Cheung, S.-H. (2024). *manymome*: An R package for
computing the indirect effects, conditional effects, and conditional
indirect effects, standardized or unstandardized, and their bootstrap
confidence intervals, in many (though not all) models. *Behavior
Research Methods, 56*(5), 4862–4882.
<https://doi.org/10.3758/s13428-023-02224-z>
