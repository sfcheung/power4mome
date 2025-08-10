# From semhelpinghands
# Modified for internal use

#' @title Add Covariances Between
#' Exogenous Variables
#'
#' @description It generates the
#' 'lavaan' model syntax for exogenous
#' variables in a `lavaan` model.
#'
#' @details The function [lavaan::sem()]
#' usually will set covariances between
#' "exogenous" variables free when
#' `fixed.x = FALSE` ("exogenous" is
#' defined here as variables that appear
#' on the right hand side but not on the
#' left hand side of the `~` operator`).
#' However, if a covariance between the
#' residual term of an endogenous
#' variable and an exogenous variable is
#' manually set to free, [lavaan::sem()]
#' may not set the aforementioned
#'  covariances free. Users will need to
#' free them manually, and there may be
#' a lot of them in some models.
#'
#' This function gets a model syntax
#' and generates the syntax for these
#' covariances. Users can then inspect
#' it, modify it if necessary, and then
#' copy and paste it to the model
#' syntax.
#'
#' @return
#' [add_exo_cov()] returns a one-element
#' character vector of the syntax, with
#' lines separated by "\\n". The
#' generated syntax is appended to the
#' input model syntax.
#'
#' [auto_exo_cov()] returns a
#' one-element character vector of the
#' generated syntax, with lines
#' separated by "\\n".
#'
#' @param model The model syntax to
#' which the covariances are to be
#' added.
#'
#' @param FUN Name (as string) of the
#' `lavaan` wrapper to be called.
#' Normally should be `"sem"`, the
#' default.
#'
#' @param print Logical. Whether the
#' generated syntax should also be
#' printed by [cat()]. Default is
#' `TRUE`.
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' set.seed(8976223)
#' n <- 100
#' x <- rnorm(n)
#' m <- .5 * x + rnorm(n, 0, sqrt(.4))
#' z <- rnorm(n)
#' y <- .4 * m + .3 * z * m + rnorm(n, 0, .5)
#' dat <- data.frame(x, m, z, y)
#' dat$zm <- dat$z * dat$m
#' mod0 <-
#' "
#' m ~ x
#' y ~ m + z + zm
#' m ~~ z + zm
#' "
#' fit <- sem(mod0, dat, fixed.x = FALSE)
#'
#' # Add covariances. Also printed by default.
#' mod0_cov <- add_exo_cov(mod0)
#'
#' # Fit the model
#' fit_cov <- sem(mod0_cov, dat, fixed.x = FALSE)
#'
#' # Manually adding the covariances
#' mod1 <-
#' "
#' m ~ x
#' y ~ m + z + zm
#' m ~~ z + zm
#' z  ~~ zm + x
#' zm ~~ x
#' "
#' fit1 <- sem(mod1, dat, meanstructure = TRUE, fixed.x = FALSE)
#'
#' # Compare the results
#'
#' # No manual covariances
#' fit
#'
#' # Automatically generated covariances
#' fit_cov
#'
#' # Manually added covariances
#' fit1
#'
#' @noRd

#' @describeIn auto_cov Add covariances
#' between exogenous variables to the
#' model syntax and than return the
#' modified model syntax.
#' @noRd
add_exo_cov <- function(model, FUN = "sem", print = TRUE) {
  out <- paste0(model, "\n",
                "# Added by auto_exo_cov\n",
                auto_exo_cov(model = model,
                              FUN = FUN,
                              print = FALSE))
  if (print) {
    cat(out)
  }
  out
}

#' @describeIn auto_cov Generate the
#' model syntax for the covariances
#' between exogenous variables.
#' @noRd
auto_exo_cov <- function(model, FUN = "sem", print = TRUE) {
  # TODO:
  # - Is requireNamespace() necessary?
  requireNamespace("lavaan", quietly = TRUE)
  fit0 <- do.call(FUN,
                  list(model = model,
                        do.fit = FALSE,
                        fixed.x = TRUE,
                        warn = FALSE))
  # TODO:
  # - Support multigroup models
  if (lavaan::lavInspect(fit0, "ngroups") != 1) {
    stop("Does not support a model with more than one group.")
  }
  isivov <- get_exo(fit0, type = "ov")
  isivlv <- get_exo(fit0, type = "lv")
  if (length(isivov) > 1) {
    outov <- gen_cov(isivov)
  } else {
    outov <- character(0)
  }
  if (length(isivlv) > 1) {
    outiv <- gen_cov(isivlv)
  } else {
    outiv <- character(0)
  }
  out <- paste0(outov, outiv)
  if (print) {
    cat(out)
  }
  out
}

# Get the exogenous observed variables and latent variables
#' @noRd

get_exo <- function(fit, type = c("ov", "lv")) {
  # TODO:
  # - Use lavaan::lavNames() to find these variables
  type <- match.arg(type)
  ptable <- lavaan::parameterTable(fit)
  isdv <- unique(ptable$lhs[ptable$op == "~"])
  onrhs <- unique(ptable$rhs[ptable$op == "~"])
  isiv <- setdiff(onrhs, isdv)
  isiv[isiv %in% lavaan::lavNames(fit, type)]
}

# Generate covariances
#' @noRd

gen_cov <- function(vars) {
  p <- length(vars)
  out <- character(0)
  for (i in seq_len(p)) {
    if (i < p) {
      out <- paste0(out,
                    vars[i],
                    " ~~ ",
                    paste0(vars[-seq_len(i)],
                          collapse = " + "),
                    "\n")
    }
  }
  out
}
