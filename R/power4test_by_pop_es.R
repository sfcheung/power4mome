#' @title Power By Effect Sizes
#'
#' @description Estimate power for a
#' range of effect sizes.
#'
#' @details This function regenerates
#' datasets for a set of effect sizes
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
#' power over a range of effect sizes.
#'
#' @return The function
#' [power4test_by_es()] returns a
#' `power4test_by_es` object, which is a
#' list of `power4test` objects, one for
#' each effect size.
#'
#' @param object A `power4test` object,
#' or a `power4test_by_es` object.
#' If it is a `power4test_by_es` object,
#' the first element will be used
#' for running the simulation.
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
#' @param progress Logical. Whether
#' progress of the simulation will be
#' displayed.
#'
#' @param ... For [power4test_by_es()],
#' they are arguments to be passed
#' to [power4test()]. For [power4test_by_es()],
#' they are [power4test_by_es()] outputs
#' to be combined together.
#'
#' @param by_seed If set to a number,
#' it will be used to generate the
#' seeds for each call to [power4test()].
#' If `NULL`, the default, then seeds
#' will still be randomly generated but
#' the results cannot be easily reproduced.
#'
#'
#' @param by_nrep If set to a number,
#' it will be used to generate the
#' number of replications (`nrep`) for
#' each call to [power4test()].
#' If set to a numeric vector of the
#' same length as `n`, then these are
#' the `nrep` values for each of the
#' calls, allowing for different numbers
#' of replications.
#' If `NULL`, the default, then the
#' original `nrep` will be used.
#'
#'
#' @seealso [power4test()]
#'
#' @examples
#'
#'
#' model_simple_med <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#'
#' model_simple_med_es <- c("y ~ m" = "l",
#'                          "m ~ x" = "m",
#'                          "y ~ x" = "n")
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
#' power_all_test_only_new_es <- power4test(object = test_out,
#'                                          pop_es = c("y ~ m" = ".10"))
#'
#' out <- power4test_by_es(test_out,
#'                             pop_es_name = "y ~ m",
#'                             pop_es_values = c(.10, .20))
#' out_reject <- get_rejection_rates_by_es(out)
#' out_reject
#'
#' @export
power4test_by_es <- function(object,
                                 pop_es_name = NULL,
                                 pop_es_values = NULL,
                                 progress = TRUE,
                                 ...,
                                 by_seed = NULL,
                                 by_nrep = NULL) {
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
    p_name <- paste0(pop_es_name,
                     " = ",
                     as.character(x))
    if (progress) {
      cat("\nUpdating the simulation for new value:",
          p_name,
          "\n")
    }
    tmp <- as.character(x)
    names(tmp) <- pop_es_name
    if (is.null(new_nrep)) {
      out[[p_name]] <- power4test(object = object,
                                  pop_es = tmp,
                                  progress = progress,
                                  iseed = seeds[i],
                                  ...)
    } else {
      out[[p_name]] <- power4test(object = object,
                                  pop_es = tmp,
                                  nrep = new_nrep[i],
                                  progress = progress,
                                  iseed = seeds[i],
                                  ...)
    }
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
#' @param sort WHen combining `power4test_by_es`
#' objects, whether they will be sorted
#' by effect size. Default is `TRUE`.
#'
#' @return
#' The method [c.power4test_by_es()] returns
#' a `power4test_by_es` object with
#' all the elements (tests for different
#' sample sizes) combined.
#'
#' @description
#' The method [c.power4test_by_es()]
#' is used to combine tests from different
#' runs of [power4test_by_es()].
#' @export
c.power4test_by_es <- function(...,
                                   sort = TRUE) {
  # Check whether they have the same ptable
  tmp <- list(...)
  if (length(tmp) > 1) {
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

#' @rdname power4test_by_es
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
#' The function [get_rejection_rates_by_es()]
#' returns a data frame which is
#' similar to the output of
#' [get_rejection_rates()], with a
#' column added for the effect size (`pop_es_name` and
#' `pop_es_values`)
#' for each test.
#'
#' @description
#' The function [get_rejection_rates_by_es()]
#' is used to extract the rejection
#' rates form a `get_rejection_rates_by_es`
#' object, with effect sizes added to
#' the output.
#'
#' @export
get_rejection_rates_by_es <- function(object_by_es,
                                          all_columns = FALSE) {
  tmpfct <- function(x) {
    out_i <- get_rejection_rates(x,
                                 all_columns = all_columns)
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