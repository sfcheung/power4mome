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

    # Export all functions defined in the global environment

    global_names <- ls(envir = .GlobalEnv)
    global_fs <- sapply(
                  global_names,
                  \(x) is.function(get(x))
                )
    if (any(global_fs)) {
      to_export <- global_names[global_fs]
      parallel::clusterExport(cl, to_export)
    }

    # Reproduce the search paths
    pkgs <- .packages()
    pkgs <- rev(pkgs)
    parallel::clusterExport(cl, "pkgs", envir = environment())
    parallel::clusterEvalQ(cl, {
                    sapply(pkgs,
                    function(x) {
                      library(x,
                             character.only = TRUE)
                    })
                  })

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

      use_lb <- getOption("power4mome.use_lb", FALSE)

      if (use_lb) {
        # ==== load balancing ====
        out <- parallel::parLapplyLB(cl = cl,
                                     X = X,
                                     fun = FUN,
                                     ...)
      } else {
        # ==== No load balancing ====
        out <- parallel::parLapply(cl = cl,
                                   X = X,
                                   fun = FUN,
                                   ...)
      }
      # out <- parallel::parLapplyLB(cl = cl,
      #                              X = X,
      #                              fun = FUN,
      #                              ...)
    } else {
      out <- lapply(X = X,
                    FUN = FUN,
                    ...)
    }
  }
  return(out)
}
