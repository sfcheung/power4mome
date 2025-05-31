#' @title Parse YAML-Stye Values For 'pop_es'
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
#' @inheritSection ptable_pop Specify 'pop_es' Using a Multiline String
#'
#'
#' @return
#' Either a named vector (for a
#' single-group model) or a list of
#' named vector (for a multigroup model).
#'
#' @param text The multiline string to be parsed
#' to a specification of population
#' values.
#'
#' @seealso [ptable_pop()],
#' [power4test()], and other functions
#' that have the `pop_es` argument.
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
