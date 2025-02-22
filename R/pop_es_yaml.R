#' @title Parse YAML-Stye 'pop_es'
#'
#' @description Convert a YAML string
#' to a vector or list of `pop_es`
#' specification.
#'
#' @details
#' This allows users to specify population
#' values of a model using one single
#' string, as in 'lavaan' model syntax.
#'
#' ## Single-Group Model
#'
#' This is an example of the string
#' for a single-group model:
#'
#' \preformatted{
#' y ~ m: l
#' m ~ x: m
#' y ~ x: nil
#' }
#'
#' The string must follow this format:
#'
#' - Each line start with `tag:`.
#'
#'   - `tag` can be the name of a
#'      parameter, in `lavaan` model
#'      syntax format.
#'
#'     - For example, `m ~ x`
#'       denotes the path from `x` to `m`.
#'
#'   - A tag in `lavaan` model syntax can
#'     specify more than one parameter
#'     using `+`.
#'
#'     - For example, `y ~ m + x`
#'       denotes the two paths from `m` and
#'       `x` to `y`.
#'
#'   - Alternatively, the `tag` can be
#'   either `.beta.` or `.cov.`.
#'
#'     - Use `.beta.` to set the default
#'       values for all regression coefficients.
#'
#'     - Use `.cov.` to set the default
#'       values for all correlations of
#'       exogenous variables (e.g., predictors).
#'
#' - After each tag is the value of the
#'   population value: `nil`
#'   for nil (zero), `s` for small, `m` for
#'   medium, and `l` for large. (Note:
#'   `n` *cannot* be used in this mode.)
#'   The
#'   value for each label is determined
#'   by `es1` and `es2` as described
#'   in [ptable_pop()].
#'
#'   - The value can also be
#'     set to a numeric value, such as
#'     `.30` or `-.30`.
#'
#' This is another example:
#'
#' \preformatted{
#' .beta: s
#' y ~ m: l
#' }
#'
#' In this example, all regression
#' coefficients are `small`, while
#' the path from `m` to `y` is large.
#'
#' ## Multigroup Model
#'
#' This is an example of the string
#' for a multigroup model:
#'
#' \preformatted{
#' y ~ m: l
#' m ~ x:
#'   - nil
#'   - s
#' y ~ x: nil
#' }
#'
#' The format is similar to that for
#' a single-group model. If a parameter
#' has the same value for all groups,
#' then the line can be specified
#' as in the case of a single-group
#' model: `tag: value`.
#'
#' If a parameter has the different
#' values across groups, then it must
#' be in this format:
#'
#' - A line starts with the tag, followed
#'   by two or more lines. Each line
#'   starts with a hyphen `-` adn the
#'   value for a group.
#'
#' For example:
#'
#' \preformatted{
#' m ~ x:
#'   - nil
#'   - s
#' }
#'
#' This denotes that the model has
#' two group. The values of the path
#' from `x` to `m` are 0 (`nil`) and
#' small (`s`), respectively.
#'
#' Another equivalent way to specify
#' the values are using `[]`, on
#' the same line of a tag.
#'
#' For example:
#'
#' \preformatted{
#' m ~ x: [nil, s]
#' }
#'
#' The number of groups is inferred
#' from the number of values for
#' a parameter. Therefore, if a tag
#' has more than one value, each tag
#' must has the same number of value,
#' or only one value.
#'
#' The tag `.beta.` and `.cov.` can
#' also be used for multigroup models.
#'
#' ## Which Approach To Use
#'
#' Note that using named vectors or
#' named lists is more reliable. However,
#' using one single string is
#' more user-friendly. If this method
#' failed, please ues named vectors or
#' named list instead.
#'
#' ## Technical Details
#'
#' The string is parsed by [yaml::read_yaml()].
#' Therefore, the format requirement
#' is actually that of YAML. Users
#' knowledgeable of YAML can use other
#' equivalent way to specify the string.
#'
#' @return
#' Either a named vector (for a
#' single-group model) or a list of
#' named vector (for a multigroup model).
#'
#' @param text The string to be parsed
#' to a specification of population
#' values.
#'
#' @seealso [ptable_pop()]
#'
#' @examples
#'
#' mod_es <- c("y ~ m" = "l",
#'             "m ~ x" = "m",
#'             "y ~ x" = "nil")
#'
#' mod_es_yaml <-
#' "
#' y ~ m: l
#' m ~ x: m
#' y ~ x: nil
#' "
#'
#' pop_es_yaml(mod_es_yaml)
#'
#'
#' @export
pop_es_yaml <- function(text) {
  # TODO:
  # - May add other features in the future.
  out <- yaml::read_yaml(text = text,
                         handlers = my_hr)
  length_max <- sapply(out,
                       length)
  if (all(length_max == 1)) {
    out <- unlist(out)
  }
  out
}

#' @noRd
pop_es_yaml_check <- function(x) {
  chk1 <- isTRUE(length(x) == 1)
  chk2 <- isTRUE(is.character(x))
  chk3 <- isTRUE(is.null(names(x)))
  if (chk1 && chk2 && chk3) {
    out <- pop_es_yaml(x)
  } else {
    out <- x
  }
  return(out)
}

#' @noRd
# Used to handle 'n' in YAML, which
# is converted to `FALSE`.
# Should not be needed because `nil`
# should be used.
my_hr <- list(seq = function(x) {
                out <- as.character(x)
                out <- gsub("FALSE",
                            "n",
                            out,
                            fixed = TRUE)
                out
              },
              "bool.fix" = function(x) {
                # Does not work for now.
                # But it should not be needed anyway
                ifelse(x,
                       yes = x,
                       no = "n")
              })