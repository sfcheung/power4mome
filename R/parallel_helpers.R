#' @noRd

do_FUN <- function(X,
                   FUN,
                   ...,
                   iseed = NULL,
                   parallel = TRUE,
                   progress = TRUE,
                   ncores = max(1, parallel::detectCores(logical = FALSE) - 1),
                   cl = NULL) {
  stop_cluster <- TRUE
  if (inherits(cl, "cluster")) {

    # ==== A cluster is supplied. Force parallel ====

    # If cl is supplied, never stop it.

    parallel <- TRUE
    ncores <- length(cl)
    stop_cluster <- FALSE
  }
  if (!isTRUE(ncores > 0)) {
    parallel <- FALSE
  }
  if (parallel) {

    # ==== Make a cluster ====

    if (is.null(cl)) {
      cl <- parallel::makeCluster(ncores)
    }
    if (!is.null(iseed)) {
      parallel::clusterSetRNGStream(cl = cl,
                                    iseed = iseed)
    }
    on.exit(if (stop_cluster) {try(parallel::stopCluster(cl), silent = TRUE)})
  } else {

    # ==== Serial ====

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
