# power4mome 0.1.1.3

- Improve the function for extending the
  initial interval before doing a
  bisection search. (0.1.1.1)

- Changed the default method for
  rejection rate confidence intervals
  to Wilson's (1927) method.
  For backward compatibility, use
  `options(power4mome.ci_method = "norm")`
  to set the default method to
  normal approximation. (0.1.1.2)

- Add the `test_method` argument for
  tests of indirect effects and their
  variants to use asymmetric *p*-values
  to do the tests. (0.1.1.3)

# power4mome 0.1.1

- Updated to be compatible with the
  forthcoming version of `lavaan`,
  0.9-12. (0.1.1)

# power4mome 0.1.0

- First public version. (0.1.0)
