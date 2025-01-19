#' @noRd

do_FUN <- function(X,
                   FUN,
                   ...,
                   iseed = NULL,
                   parallel = TRUE,
                   progress = TRUE,
                   ncores = max(1, parallel::detectCores(logical = FALSE) - 1)) {
  if (!isTRUE(ncores > 0)) {
    parallel <- FALSE
  }
  if (parallel) {
    cl <- parallel::makeCluster(ncores)
    if (!is.null(iseed)) {
      parallel::clusterSetRNGStream(cl = cl,
                                    iseed = iseed)
    }
    on.exit(try(parallel::stopCluster(cl), silent = TRUE))
  } else {
    cl <- NULL
    if (!is.null(iseed)) set.seed(iseed)
  }
  if (progress) {
    # Can be used for both serial and parallel
    out <- pbapply::pblapply(X = X,
                             FUN = FUN,
                             ...,
                             cl = cl)
  } else {
    if (parallel) {
      out <- parallel::parLapplyLB(cl = cl,
                                   X = X,
                                   fun = FUN,
                                   ...)
    } else {
      out <- lapply(X = X,
                    FUN = FUN,
                    ...)
    }
  }
  return(out)
}
