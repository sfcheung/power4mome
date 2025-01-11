#' @title Generate the Population Model
#'
#' @description Generate the complete
#' population model using the model
#' syntax and user-specified effect
#' sizes.
#'
#' @details It generates a `lavaan`
#' parameter tables that can be used
#' to generate data based on the population
#' values of model parameters.
#'
#' @return
#' A `lavaan` parameter table of the
#' model, with `start` set to the
#' population values.
#'
#' @param model String. The model defined
#' by `lavaan` model syntax.
#'
#' @param pop_es It can a data frame
#' with these columns: `lhs`, `op`,
#' `rhs`, and `pop`. The first three
#' columns correspond to those in a
#' `lavaan` parameter table. The column
#' `pop` stores the population values.
#' The column `es` stores the original
#' labels, for reference. It can also be
#' A named character vector. The names
#' are `lavaan` for the selected
#' parameters. For example, `m ~ x`
#' denotes the path from `x` to `m`. Can
#' specify more than one parameters. For
#' example, `y ~ m + x` denotes the two
#' paths from `m` and `x` to `y`. The
#' value is the label for the effect
#' size: `s` for small, `m` for medium,
#' and `l` for large. There are two
#' possible values, one set, `es1`, for
#' correlations and regression
#' coefficients, the other set, `es2`,
#' for standardized moderation effect,
#' the coefficients of a product term.
#'
#' @param es1 A named vector to set the
#' values for each label of the effect
#' size of correlations and regression
#' paths.
#' Default is `c("s" = .10, "m" = .30, "l" = .50)`.
#' Used only if `par_es` is a named
#' vector.
#'
#' @param es2 A named vector to set the
#' values for each label of the effect
#' size of product term.
#' Default is `c("s" = .05, "m" = .10, "l" = .15)`.
#' Used only if `par_es` is a named
#' vector.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @examples
#'
#' model1 <-
#' "
#' m1 ~ x + c1
#' m2 ~ m1 + x2 + c1
#' y ~  m2 + m1 + x + w + x:w + c1
#' "
#'
#' model1_es <- c("m1 ~ x" = "-m",
#'                "m2 ~ m1" = "s",
#'                "y ~ m2" = "l",
#'                "y ~ x" = "m",
#'                "y ~ w" = "s",
#'                "y ~ x:w" = "s",
#'                "x ~~ w" = "s")
#'
#' ptable_final1 <- ptable_pop(model1,
#'                             pop_es = model1_es)
#' ptable_final1
#'
#' @export
# Input:
# - model: The model syntax
# - pop_es: The parameter table of parameters with population values OR
#           A named vector of the effect size labels for selected parameters
# Output:
# - The parameter table with population values
ptable_pop <- function(model,
                       pop_es,
                       es1 = c("s" = .10,
                               "m" = .30,
                               "l" = .50),
                       es2 = c("s" = .05,
                               "m" = .10,
                               "l" = .15)) {
  if (is.character(pop_es)) {
    pop_es <- fix_par_es(pop_es,
                         model = model)
    par_pop <- set_pop(pop_es,
                       es1 = es1,
                       es2 = es2)
  } else {
    par_pop <- pop_es
  }
  fit0 <- lavaan::sem(model,
                      do.fit = FALSE)
  ptable0 <- lavaan::parTable(fit0)
  par_pop2 <- merge(par_pop,
                       ptable0[, c("lhs", "op", "rhs", "id")],
                       all.x = TRUE,
                       all.y = FALSE)
  par_pop2 <- par_pop2[, c("id", "lhs", "op", "rhs", "pop")]

  ptable1 <- merge(ptable0,
                   par_pop2,
                   all.x = TRUE)
  ptable1[!is.na(ptable1$pop), "start"] <- ptable1[!is.na(ptable1$pop), "pop"]
  ptable1$pop <- NULL
  # TODO:
  # - Check equality constraints
  ptable1
}
