fix_many_lm_model <- function(model) {
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
