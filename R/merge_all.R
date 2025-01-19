#' @title Title In Title Case
#'
#' @description One paragraph description.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
#'
merge_all <- function(data_all,
                      fit_all,
                      mc_all = NULL) {
  nrep <- length(data_all)
  if (nrep != length(fit_all)) {
    stop("The numbers of replications do not match.")
  }
  if ((nrep != length(mc_all)) && !is.null(mc_all)) {
    stop("The numbers of replications do not match.")
  }
  if (is.null(mc_all)) {
    mc_all <- rep(NA, nrep)
  }
  tmpfct <- function(x, y, z) {
    x$fit <- y
    x$mc_out <- z
    x
  }
  out0 <- mapply(tmpfct,
                 x = data_all,
                 y = fit_all,
                 z = mc_all,
                 SIMPLIFY = FALSE)
  return(out0)
}
