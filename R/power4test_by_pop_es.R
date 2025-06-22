#' @title Power By Effect Sizes
#'
#' @description Estimate power for a
#' set of effect sizes (population
#' values of a model parameter).
#'
#' @details This function regenerates
#' datasets for a set of effect sizes
#' (population values of a model parmeter)
#' and does the stored tests in each of
#' them.
#'
#' Optionally, it can also be run
#' on a object with no stored tests.
#' In this case, additional arguments
#' must be set to instruct [power4test()]
#' on the tests to be conducted.
#'
#' It is usually used to examine the
#' power over a sets of effect sizes
#' (population values).
#'
#' @return The function
#' [power4test_by_es()] returns a
#' `power4test_by_es` object, which is a
#' list of `power4test` objects, one for
#' each population value of the parameter.
#'
#' @inheritParams power4test_by_n
#'
#' @param object A `power4test` object,
#' or a `power4test_by_es` object.
#' If it is a `power4test_by_es` object,
#' the first element, which is a
#' `power4test` object, will be used
#' as the value of this argument.
#'
#' @param pop_es_name The name of the
#' parameter. See the help page
#' of [ptable_pop()] on the names for
#' the argument `pop_es`.
#'
#' @param pop_es_values A numeric
#' vector of the population values
#' of the parameter specified in
#' `pop_es_names`.
#'
# @param progress <- Inherited
#'
#' @param ... For [power4test_by_es()],
#' they are arguments to be passed
#' to [power4test()]. For [c.power4test_by_es()],
#' they are [power4test_by_es()] outputs
#' to be combined together.
#' For the `print` method of the output
#' of [power4test_by_es()], they are
#' arguments to be passed to the
#' `print` method of the output of
#' [power4test()] ([print.power4test()]).
#'
# @param by_seed <- Inherited
#'
#' @param by_nrep If set to a number,
#' it will be used to generate the
#' number of replications (`nrep`) for
#' each call to [power4test()].
#' If set to a numeric vector of the
#' same length as `pop_es_values`, then these are
#' the `nrep` values for each of the
#' calls, allowing for different numbers
#' of replications for the population values.
#' If `NULL`, the default, then the
#' original `nrep` will be used.
#' This argument is used by
#' [x_from_power()] for efficiency, and
#' is rarely used when calling
#' this function directly.
#'
# @param save_sim_all <- Inherited
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#' # Specify the model
#'
#' model_simple_med <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' # Specify the population values
#'
#' model_simple_med_es <-
#' "
#' m ~ x: m
#' y ~ m: l
#' y ~ x: n
#' "
#'
#' sim_only <- power4test(nrep = 2,
#'                        model = model_simple_med,
#'                        pop_es = model_simple_med_es,
#'                        n = 100,
#'                        R = 40,
#'                        ci_type = "boot",
#'                        fit_model_args = list(fit_function = "lm"),
#'                        do_the_test = FALSE,
#'                        iseed = 1234)
#'
#' test_out <- power4test(object = sim_only,
#'                        test_fun = test_indirect_effect,
#'                        test_args = list(x = "x",
#'                                         m = "m",
#'                                         y = "y",
#'                                         boot_ci = TRUE,
#'                                         mc_ci = FALSE))
#'
#' out <- power4test_by_es(test_out,
#'                             pop_es_name = "y ~ m",
#'                             pop_es_values = c(.10, .20))
#' out_reject <- rejection_rates(out)
#' out_reject
#'
#' @export
power4test_by_es <- function(object,
                             pop_es_name = NULL,
                             pop_es_values = NULL,
                             progress = TRUE,
                             ...,
                             by_seed = NULL,
                             by_nrep = NULL,
                             save_sim_all = TRUE) {
  if (!inherits(object, "power4test") &&
      !inherits(object, "power4test_by_es")) {
    stop("Only support 'power4test' or 'power4test_by_es' objects.")
  }
  # Use the first object if the object is a power4test_by_es object:
  if (inherits(object, "power4test_by_es")) {
    object <- object[[1]]
  }
  if (!is.null(object$sim_all[[1]]$group_name)) {
    stop("Does not support multigroup models for now.")
  }
  if (is.null(pop_es_name) || is.null(pop_es_values)) {
    stop("Both 'pop_es_name' and 'pop_es_values' must be set.")
  }
  out <- list()

  if (length(by_seed) == length(pop_es_values)) {
    seeds <- by_seed
  } else {
    if (!is.null(by_seed)) {
      set.seed(by_seed)
    }
    seeds <- sample.int(99999999,
                        size = length(pop_es_values))
  }

  if (length(by_nrep) == length(pop_es_values)) {
    new_nrep <- by_nrep
  } else {
    if (!is.null(by_nrep)) {
      new_nrep <- rep(by_nrep, length(pop_es_values))
    } else {
      new_nrep <- NULL
    }
  }
  # TODO
  # - Think about to handle MG models,
  #   for which pop_values can be vectors.
  for (i in seq_along(pop_es_values)) {
    x <- pop_es_values[i]
    tmp <- ifelse(x < .001,
                  sprintf("%5.3e", x),
                  formatC(x, digits = 3, format = "f"))
    p_name <- paste0(pop_es_name,
                     " = ",
                     tmp)
    if (progress) {
      cat("\nUpdating the simulation for new value:",
          p_name,
          "\n")
    }
    tmp <- as.character(x)
    names(tmp) <- pop_es_name
    if (is.null(new_nrep)) {
      tmp_out <- tryCatch(power4test(
                            object = object,
                            pop_es = tmp,
                            progress = progress,
                            iseed = seeds[i],
                            ...,
                          ),
                          error = function(e) e)
    } else {
      tmp_out <- tryCatch(power4test(
                            object = object,
                            pop_es = tmp,
                            nrep = new_nrep[i],
                            progress = progress,
                            iseed = seeds[i],
                            ...
                          ),
                          error = function(e) e)
    }

    if (inherits(tmp_out, "error")) {

      if (progress) {
        msg <- "Error in estimating power for this value:"
        msg <- c(msg, paste0("- ", names(tmp), ": ", tmp))
        msg <- c(msg, "The error message:")
        msg <- c(msg, paste0("- ", tmp_out$message))
        msg <- c(msg, "This value is skipped.")
        cat(c("", msg, ""), sep = "\n")
      }

      next
    }

    if (!save_sim_all) {
      # Keep the first element
      tmp_out$sim_all <- tmp_out$sim_all[1]
    }
    out[[p_name]] <- tmp_out
    attr(out[[p_name]], "pop_es_name") <- pop_es_name
    attr(out[[p_name]], "pop_es_value") <- x
  }
  class(out) <- c("power4test_by_es", class(out))
  # attr(out, "pop_es_name") <- pop_es_name
  # attr(out, "pop_es_values") <- pop_es_values
  out
}


#' @rdname power4test_by_es
#'
#' @param sort WHen combining the
#' objects, whether they will be sorted
#' by population values. Default is `TRUE`.
#'
# @param skip_checking_models <- Inherited
#'
#' @return
#' The method [c.power4test_by_es()] returns
#' a `power4test_by_es` object with
#' all the elements (tests for different
#' values of `pop_es_values`) combined.
#'
#' @details
#' The method [c.power4test_by_es()]
#' is used to combine tests from different
#' runs of [power4test_by_es()].
#' @export
c.power4test_by_es <- function(...,
                               sort = TRUE,
                               skip_checking_models = FALSE) {
  # Check whether they have the same ptable
  tmp <- list(...)
  if ((length(tmp) > 1) && !skip_checking_models) {
    ptables <- lapply(tmp,
                      \(x) {x[[1]]$sim_all[[1]]$ptable})
    for (i in seq_along(ptables)[-1]) {
      chk <- identical(ptables[[i]][, c("lhs", "op", "rhs", "group", "user", "block")],
                       ptables[[i - 1]][, c("lhs", "op", "rhs", "group", "user", "block")])
      if (!chk) {
        stop("Not all objects are based on the same model.")
      }
    }
  }
  out <- NextMethod()
  out["sort"] <- NULL
  out["skip_checking_models"] <- NULL

  all_pop_es_name <- sapply(out,
                            \(x) {attr(x, "pop_es_name")})
  all_pop_es_name <- unique(all_pop_es_name)
  if (length(all_pop_es_name) != 1) {
    stop("Not all objects have the same parameters changes: ",
         paste0(all_pop_es_name, collapse = ","))
  }

  all_pop_es_values <- sapply(out,
                              \(x) {attr(x, "pop_es_value")})
  class(out) <- c("power4test_by_es", class(out))
  # attr(out, "pop_es_name") <- all_pop_es_name
  # attr(out, "pop_es_values") <- all_pop_es_values
  if (!sort) {
    return(out)
  }
  i <- order(all_pop_es_values)
  out <- out[i]
  class(out) <- c("power4test_by_es", class(out))
  # attr(out, "pop_es_name") <- all_pop_es_name
  # attr(out, "pop_es_values") <- all_pop_es_values[i]
  return(out)
}

#' @param object_by_es A `power4test_by_es`
#' object, which is an output of
#' [power4test_by_es()].
#'
#' @param all_columns If `TRUE`, all
#' columns stored by a test will be
#' printed. Default is `FALSE` and
#' only essential columns related to
#' power will be printed.
#'
#' @return
#' The function
#' returns a data frame which is
#' similar to the output of
#' [rejection_rates()], with a
#' column added for the effect size (`pop_es_name` and
#' `pop_es_values`)
#' for each test.
#'
#' @description
#' The function
#' is used to extract the rejection
#' rates form a `power4test_by_es`
#' object, with effect sizes added to
#' the output.
#'
#' @noRd
rejection_rates_by_es <- function(object_by_es,
                                  all_columns = FALSE,
                                  ...) {
  tmpfct <- function(x) {
    out_i <- rejection_rates(x,
                             all_columns = all_columns,
                             ...)
    pn <- attr(x, "pop_es_name")
    pv <- attr(x, "pop_es_value")
    out_i <- data.frame(par = pn,
                        es = pv,
                        out_i)
    out_i
  }
  out <- mapply(tmpfct,
                x = object_by_es,
                # pv = attr(object_by_es, "pop_es_values"),
                # MoreArgs = list(pn = attr(object_by_es, "pop_es_name")),
                SIMPLIFY = FALSE)
  out <- do.call(rbind,
                 out)
  rownames(out) <- NULL
  out
}

#' @rdname power4test_by_es
#'
#' @param original_object The object
#' to be converted to a `power4test_by_es`
#' object.
#'
#' @return
#' The function [as.power4test_by_es()] returns
#' a `power4test_by_es` object converted
#' from the input object.
#'
#' @details
#' The function [as.power4test_by_es()]
#' is used to convert a `power4test`
#' object to a `power4test_by_es`
#' object, if it is not already one.
#' Useful when concatenating
#' `power4test` objects with
#' `power4test_by_es` objects.
#' @export
as.power4test_by_es <- function(original_object,
                                pop_es_name) {
  if (inherits(original_object, "power4test")) {
    x0 <- pop_es(original_object,
                 pop_es_name = pop_es_name)
    original_object <- list(original_object)
    class(original_object) <- c("power4test_by_es", class(original_object))
    names(original_object) <- paste0(pop_es_name,
                                     " = ",
                                     as.character(x0))
    attr(original_object[[1]], "pop_es_name") <- pop_es_name
    attr(original_object[[1]], "pop_es_value") <- x0
    return(original_object)
  }
  if (inherits(original_object, "power4test_by_es")) {
    return(original_object)
  }
  stop("original_object not of a supported class.")
}


#' @rdname power4test_by_es
#'
#' @param x The object
#' to be printed.
#'
#' @param digits The numbers of digits
#' displayed after the decimal.
#'
#' @param print_all If `TRUE`, all
#' elements in `x`, that is, the results
#' of all sample sizes examined, will
#' be printed. If `FALSE`, then only
#' those of the first value of the
#' parameter
#' will be printed.
#'
#' @return
#' The `print`-method of `power4test_by_es`
#' objects returns the object invisibly.
#' It is called for its side-effect.
#'
#' @export
print.power4test_by_es <- function(
    x,
    print_all = FALSE,
    digits = 3,
    ...
  ) {
  pop_es_name <- attr(x[[1]], "pop_es_name")
  x_tried <- sapply(
                x,
                function(xx) {attr(xx, "pop_es_value")}
              )
  x_tried_str <- formatC(
                      x_tried,
                      digits = digits,
                      format = "f"
                    )
  x_tried_all_str <- paste0(
                        x_tried_str,
                        collapse = ", "
                      )
  catwrap(
      c(paste("The parameter examined:", pop_es_name),
        paste("The value(s) examined:", x_tried_all_str))
      )

  if (print_all) {
    cat("\nThe output for each `power4test` analysis:\n")
    ii <- seq_along(x)
  } else {
    cat("\nThe output of the first element:\n")
    ii <- 1
  }

  for (i in ii) {
    x_i <- attr(x[[i]], "pop_es_value")
    x_i_str <- formatC(
                  x_i,
                  digits = digits,
                  format = "f"
                )
    xx <- x[[i]]
    class(xx) <- "power4test"
    cat(header_str("",
                  hw = .84,
                  sym = "-",
                  prefix = "\n",
                  suffix = "\n",
                  sep_i = ""))
    cat(header_str(paste0("Parameter: ", pop_es_name),
                  hw = .8,
                  sym = "-",
                  suffix = "\n"))
    cat(header_str(paste0("Value: ", x_i_str),
                  hw = .8,
                  sym = "-",
                  suffix = "\n"))
    cat(header_str("",
                  hw = .84,
                  sym = "-",
                  suffix = "\n",
                  sep_i = ""))
    print(xx,
          digits = digits,
          ...)
  }
  if (!print_all) {
    cat("\n")
    catwrap(
        "Print with 'print_all = TRUE' to print all elements."
        )
  }
  invisible(x)
}