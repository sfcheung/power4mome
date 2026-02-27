fix_many_lm_model <- function(model) {
  # Try to use lavaan
  ptable <- lavaan::lavaanify(model)
  ptable <- ptable[ptable$op == "~", ]
  all_y <- lavaan::lavNames(ptable, "eqs.y")
  all_models <- lapply(
      all_y,
      function(y) {
        pt1 <- ptable[ptable$lhs == y, ]
        all_x_i <- pt1$rhs
        model_i <- paste(
                    y,
                    "~",
                    paste(all_x_i,
                          collapse = " + ")
                  )
      })
  all_models <- unlist(all_models)
  all_models <- paste0(
                  all_models,
                  collapse = "\n"
                )
  all_models <- paste0(all_models, "\n")
  all_models
}

#' @noRd
# Does not work for model specified in more than one line.
fix_many_lm_model_old <- function(model) {
  # Remove lines that cannot be fitted by lm()
  tmp <- strsplit(
            model,
            split = "\n",
            fixed = TRUE)
  tmp <- unlist(tmp)
  tmp2 <- sapply(
            tmp,
            function(x) {
              isFALSE(inherits(
                tryCatch(stats::terms(stats::as.formula(x)),
                        silent = TRUE,
                        error = function(e) e),
                "error"))
            })
  out <- tmp[tmp2]
  out <- paste(out,
               collapse = "\n")
  out
}
